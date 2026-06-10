(* Copyright (C) 2026 Vates.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module D = Debug.Make (struct let name = __MODULE__ end)

(* HTTP headers associated with a GET upgrade request. *)
type upgrade_headers = {
    upgrade: string
  ; connection: string
  ; websocket_key: string
  ; websocket_version: string
}

(* Extract upgrade-related headers, ensuring all are present. *)
let parse_upgrade_headers hs =
  let ( let* ) = Option.bind in
  let read k = Hashtbl.find_opt hs (String.lowercase_ascii k) in
  let* upgrade = read "Upgrade" in
  let* connection = read "Connection" in
  let* websocket_key = read "Sec-WebSocket-Key" in
  let* websocket_version = read "Sec-WebSocket-Version" in
  Some {upgrade; connection; websocket_key; websocket_version}

(* Send 426 Upgrade Required to client. *)
let reply_upgrade_required (fd : Unix.file_descr) =
  let res = Http.Response.(make "426" "Upgrade Required" |> to_wire_string) in
  Xapi_stdext_unix.Unixext.really_write_string fd res

(* Send 101 Switching Protocols to client. *)
let reply_switching_protocols (fd : Unix.file_descr) hash =
  let headers =
    [
      ("Upgrade", "websocket")
    ; ("Connection", "Upgrade")
    ; ("Sec-WebSocket-Accept", hash)
    ]
  in
  let res =
    Http.Response.(make ~headers "101" "Switching Protocols" |> to_wire_string)
  in
  Xapi_stdext_unix__Unixext.really_write_string fd res

let challenge_response key =
  let ( >> ) f g x = g (f x) in
  let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  key ^ guid |> Sha1.(string >> to_bin) |> Base64.encode_string

module Frame = struct
  type opcode =
    | Continuation
    | Text
    | Binary
    | Closing
    | Ping
    | Pong
    | Unknown of int

  let string_of_opcode = function
    | Continuation ->
        "Continuation"
    | Text ->
        "Text"
    | Binary ->
        "Binary"
    | Closing ->
        "Closing"
    | Ping ->
        "Ping"
    | Pong ->
        "Pong"
    | Unknown o ->
        Printf.sprintf "Unknown(%x)" o

  let opcode_of_int = function
    | 0 ->
        Continuation
    | 1 ->
        Text
    | 2 ->
        Binary
    | 8 ->
        Closing
    | 9 ->
        Ping
    | 10 ->
        Pong
    | o ->
        Unknown o

  type header = {fin: bool; opcode: opcode; mask: string; len: int}

  let string_to_hex s =
    let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
      s ;
    Buffer.contents b

  let string_of_header h =
    Printf.sprintf "[FIN = %b, OP = %s, MASK = %s, LEN = %d]" h.fin
      (string_of_opcode h.opcode)
      (string_to_hex h.mask) h.len
end

module Parser = struct
  open Angstrom

  let parse_fin_opcode =
    let* b = any_uint8 in
    let fin = (b land (1 lsl 7)) lsr 7 <> 0 in
    let opc = Frame.opcode_of_int (b land 0xf) in
    return (fin, opc)

  let parse_mask_and_len =
    let* b = any_uint8 in
    let mask = (b land (1 lsl 7)) lsr 7 <> 0 in
    let* len =
      let len = b land ((1 lsl 7) - 1) in
      match len with
      | _ when len <= 125 ->
          return len
      | 126 ->
          let* len = BE.any_uint16 in
          return len
      | _ ->
          (* We could handle this, but it's a bit unreasonable. *)
          fail "Too large."
    in
    let* key =
      if mask then
        let* key = take 4 in
        return key
      else
        return "\x00\x00\x00\x00"
    in
    return (key, len)

  let parse_header =
    let* fin, opcode = parse_fin_opcode in
    let* mask, len = parse_mask_and_len in
    return Frame.{fin; opcode; mask; len}
end

module Ring = struct
  type t = {buffer: bytes; mutable head: int; mutable tail: int}

  let create sz =
    let sz = sz + 1 in
    let buffer = Bytes.of_string (String.make sz '.') in
    let head = 0 in
    let tail = head in
    {buffer; head; tail}

  let used r =
    let cap = Bytes.length r.buffer in
    (r.tail - r.head + cap) mod cap

  let is_full r = used r = Bytes.length r.buffer - 1

  let free r = Bytes.length r.buffer - used r - 1

  let free_cont r =
    let cap = Bytes.length r.buffer in
    let hd = r.head in
    let tl = r.tail in
    if tl < hd then
      hd - tl - 1
    else if hd = 0 then
      cap - tl - 1
    else
      cap - tl

  let used_cont r =
    let cap = Bytes.length r.buffer in
    let hd = r.head in
    let tl = r.tail in
    if tl >= hd then
      tl - hd
    else
      cap - hd

  let debug r =
    D.debug
      "{ used = %d, used_cont = %d, free = %d, free_cont = %d, head = %d, tail \
       = %d }\n"
      (used r) (used_cont r) (free r) (free_cont r) r.head r.tail

  let peek_n r n =
    let len = min n (used r) in
    let cont = min len (used_cont r) in
    let buf = Bytes.create len in
    Bytes.blit r.buffer r.head buf 0 cont ;
    if cont < len then Bytes.blit r.buffer 0 buf cont (len - cont) ;
    Bytes.unsafe_to_string buf

  let read_all r =
    let cap = Bytes.length r.buffer in
    let b = Buffer.create 32 in
    for i = 0 to used r - 1 do
      Buffer.add_char b (Bytes.get r.buffer r.head) ;
      Bytes.set r.buffer r.head '.' ;
      r.head <- (r.head + 1) mod cap
    done ;
    Buffer.contents b
end

type task = Call of string

module WorkQueue = struct
  type t = {
      queue: (Unix.file_descr * task) Queue.t
    ; mutex: Mutex.t
    ; condition: Condition.t
  }

  let create () =
    let queue = Queue.create () in
    let mutex = Mutex.create () in
    let condition = Condition.create () in
    {queue; mutex; condition}
end

module Encoding = struct
  (* For now, only single frames. *)
  let encode_text_frame s =
    let l = String.length s in
    let b = Buffer.create l in
    (* FIN + Text; single frames only for now. *)
    Buffer.add_char b '\x81' ;
    (* Add the mask + length byte, all server -> client frames must be
       unmasked. *)
    if l <= 125 then
      Buffer.add_char b (Char.chr l)
    else if l >= 126 && l <= 65535 then begin
      Buffer.add_char b (Char.chr 126) ;
      Buffer.add_uint16_be b l
    end else
      raise (invalid_arg "String too long") ;
    Buffer.add_string b s ;
    Buffer.to_bytes b
end

module Worker = struct
  let parse_jsonrpc s =
    try Some (Jsonrpc.version_id_and_call_of_string s) with _ -> None

  let handle_task (fd, task) =
    match task with
    | Call str -> (
      match parse_jsonrpc str with
      | Some (version, id, call) ->
          D.debug "Parsed request as: %s" (Jsonrpc.string_of_call call) ;
          let req =
            Http.Request.make ~user_agent:"websocket" Http.Post "/jsonrpc"
          in
          let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
          let res =
            (* BEGIN HACK *)
            (* The problem with using 'b' here is that deeper parts of
              Xapi may privilege this request, because it looks like the
              local domain socket. *)
            Api_server.Server.dispatch_call req b call
            |> Jsonrpc.string_of_response ~id ~version
            (* END HACK *)
          in
          Unix.(close a ; close b) ;
          D.debug "Evaluated to: %s" res ;
          (* Quick prototyping test, this should be queued as a response for the client. *)
          (* We may self-pipe a notification fd that we pass to each
            worker, so we can wake epoll when a worker completes work. *)
          (* For example, we enqueue this as like: { data = bytes, mutable written = 0 }.
            Then, if our client exists still, we mark it for EPOLLOUT.
            During epoll out, we try to drain the outgoing ring, e.g.
            while free_cont out_ring > 0 do
              front: { data, written } = take from front of queue
              amount = min (() - front.written) (free_cont out_ring)
              n = try write .. amount .. with (EAGAIN | EWOULDBLOCK) -> 0
            done

            We should avoid the worker thread populating the outgoing ring directly.
            I think it should be the job of the epoll loop to try and
            shuffle data between the queue of responses and outgoing
            ring. It may be that each worker receives the client as
            part of a task, rather than the fd (can query aliveness
            status + lock and mutate per-client response queue).
         *)
          Encoding.encode_text_frame res
          |> Bytes.to_string
          |> Xapi_stdext_unix__Unixext.really_write_string
               fd (* Debugging purposes: client may already be disconnected. *)
      | _ ->
          ()
    )

  let rec work (wq : WorkQueue.t) =
    Mutex.lock wq.mutex ;
    while Queue.is_empty wq.queue do
      Condition.wait wq.condition wq.mutex
    done ;
    let task = Queue.take_opt wq.queue in
    Mutex.unlock wq.mutex ;
    ( match task with
    | Some task -> (
        D.debug "Got some work to do!" ;
        try handle_task task
        with e -> D.debug "Worker exception: %s" (Printexc.to_string e)
      )
    | _ ->
        ()
    ) ;
    work wq
end

module Server = struct
  type state =
    | Reading_header of {state: Frame.header Angstrom.Unbuffered.state}
    | Reading_payload of {hdr: Frame.header; buffer: Buffer.t}

  type client = {
      fd: Unix.file_descr
    ; r_ring: Ring.t
    ; mutable state: state
    ; mutable message_op: Frame.opcode
    ; message: Buffer.t
    ; mutable alive: bool Atomic.t
  }

  exception Client_eof of client

  let create_client fd =
    Unix.set_nonblock fd ;
    let r_ring = Ring.create (8092 * 2) in
    let state = Angstrom.Unbuffered.parse Parser.parse_header in
    let state = Reading_header {state} in
    let message_op = Frame.Text in
    let message = Buffer.create 512 in
    let alive = Atomic.make true in
    {fd; r_ring; state; message_op; message; alive}

  type t = {
      clients: (Unix.file_descr, client) Hashtbl.t
    ; epoll: Polly.t
    ; mutex: Mutex.t
    ; queue: WorkQueue.t
  }

  let create () =
    let clients = Hashtbl.create 16 in
    let epoll = Polly.create () in
    let mutex = Mutex.create () in
    let queue = WorkQueue.create () in
    ignore (Thread.create Worker.work queue : Thread.t) ;
    {clients; epoll; mutex; queue}

  let has_epoll_in es = Polly.Events.(test es inp)

  let drain_into_ring client =
    let r = client.r_ring in
    let rec go () =
      let again = ref true in
      if Ring.is_full r then () ;
      let cont = Ring.free_cont r in
      if cont > 0 then begin
        ( try
            let n = Unix.read client.fd r.buffer r.tail cont in
            if n <= 0 then
              raise (Client_eof client)
            else begin
              let cap = Bytes.length r.buffer in
              r.tail <- (r.tail + n) mod cap
            end
          with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
            again := false
        ) ;
        if !again then go ()
      end
    in
    go ()

  let handle_input_event s _ fd es =
    let c =
      Mutex.lock s.mutex ;
      let r = Hashtbl.find_opt s.clients fd in
      Mutex.unlock s.mutex ; r
    in
    match c with
    | Some c ->
        if has_epoll_in es then begin
          try drain_into_ring c with Client_eof c -> Atomic.set c.alive false
        end
    | _ ->
        ()

  let advance_state s c =
    let open Angstrom in
    match c.state with
    | Reading_header {state} -> (
      match state with
      | Fail _ | Done _ ->
          failwith "Invalid client state"
      | Partial {continue; _} -> (
          let r = c.r_ring in
          let chunk =
            let avail = Ring.used r in
            let s =
              ( if avail < 14 then
                  avail
                else
                  14
              )
              |> Ring.peek_n r
            in
            let n = String.length s in
            Bigstringaf.of_string ~off:0 ~len:n s
          in
          match
            continue chunk ~off:0 ~len:(Bigstringaf.length chunk) Incomplete
          with
          | Partial {committed; _} ->
              assert (committed = 0) ;
              ()
          | Fail (_, _, err) ->
              failwith err
          | Done (consumed, hdr) ->
              D.debug "Received header: %s" (Frame.string_of_header hdr) ;
              ( match hdr.opcode with
              | Unknown _ | Ping | Pong | Binary ->
                  (* TODO: proper handling. *)
                  failwith "Unknown opcode!"
              | Text | Closing ->
                  c.message_op <- hdr.opcode
              | Continuation ->
                  ()
              ) ;
              (* BEGIN HACK *)
              if hdr.opcode = Frame.Closing then begin
                (* This is for debugging only. In reality, Closing is a
                 logical message and should be replied to properly. *)
                  Xapi_stdext_unix__Unixext.really_write_string c.fd "\x88\x00"
              end ;
              (* END HACK *)
              let r = c.r_ring in
              let cap = Bytes.length r.buffer in
              r.head <- (r.head + consumed) mod cap ;
              (* Switch to reading an associated payload. *)
              (* Note: this may be elided in some cases, e.g. if the
                length is 0. Some opcodes, like Ping, must be responded
                to with the received payload. *)
              let buffer = Buffer.create 256 in
              c.state <- Reading_payload {hdr; buffer}
        )
    )
    | Reading_payload {hdr; buffer} ->
        let r = c.r_ring in
        let seen = Buffer.length buffer in
        let remaining = hdr.len - seen in
        let amount =
          let used = Ring.used r in
          Int.min remaining used
        in
        let chunk =
          (* Unmask the chunk. *)
          let data = Ring.peek_n r amount in
          let cap = Bytes.length r.buffer in
          r.head <- (r.head + amount) mod cap ;
          String.mapi
            (fun i ch ->
              let open Char in
              chr (code ch lxor code hdr.mask.[(seen + i) mod 4])
            )
            data
        in
        (* Append the unmasked chunk to the current frame's payload buffer. *)
        Buffer.add_string buffer chunk ;

        if seen + amount >= hdr.len then begin
          (* We've seen an entire payload (not necessarily an entire message). *)
            Buffer.add_buffer c.message buffer ;
            (* If this payload is the final in a sequence, we've seen an
            entire message. *)
            if hdr.fin then begin
              (* TODO: record which opcode each logical message
                 started with, as we may buffer a payload to handle a
                 ping, which should not execute anything. *)
                let msg = Buffer.contents c.message in
                if c.message_op = Frame.Text then (
                  D.debug "Pushing work item: %s" msg ;
                  Mutex.lock s.queue.mutex ;
                  Queue.push (c.fd, Call msg) s.queue.queue ;
                  Mutex.unlock s.queue.mutex ;
                  Condition.signal s.queue.condition ;
                  D.debug "Pushed work item"
                ) else
                  D.debug "Received non-text (%s) data: %s"
                    (Frame.string_of_opcode c.message_op)
                    msg ;
                Buffer.reset c.message
            end ;
            c.state <-
              Reading_header
                {state= Angstrom.Unbuffered.parse Parser.parse_header}
        end

  let process_events s =
    while true do
      (* TODO: can we stop and start the websocket server dynamically? *)
      let _ready = Polly.wait s.epoll 16 2000 (handle_input_event s) in
      let cs =
        Mutex.lock s.mutex ;
        let dead, alive =
          Hashtbl.fold
            (fun _ c (dead, alive) ->
              if Atomic.get c.alive then
                (dead, c :: alive)
              else
                (c :: dead, alive)
            )
            s.clients ([], [])
        in
        List.iter
          (fun c ->
            D.debug "Removing %d from server" (Obj.magic c) ;
            Hashtbl.remove s.clients c.fd ;
            Polly.del s.epoll c.fd
          )
          dead ;
        Mutex.unlock s.mutex ;
        (* Note: some clients may populate the ring buffer and then
           disconnect, as they do not want replies (although their
           messages still need processing). I'm deferring the
           management of those intricacies until the logical message
           handling is implemented. *)
        dead @ alive
      in
      let cs = ref cs in
      let i = ref 0 in
      (* TODO: do this in a more fair - and less arbitrary - way, assuming advance_state stabilises. *)
      while !cs <> [] && !i < 200 do
        cs :=
          List.filter
            (fun c ->
              let prev = Ring.used c.r_ring in
              if prev > 0 then advance_state s c ;
              Ring.used c.r_ring < prev
            )
            !cs ;
        incr i
      done
    done

  let register_client s fd =
    let c = create_client fd in
    Polly.(add s.epoll fd Events.inp) ;
    Mutex.lock s.mutex ;
    Hashtbl.replace s.clients fd c ;
    Mutex.unlock s.mutex

  let run s = process_events s
end

let server = Server.create ()

let () =
  D.debug "Starting websocket server" ;
  ignore (Thread.create Server.run server : Thread.t)

let websocket_upgrade_handler (req : Http.Request.t) (fd : Unix.file_descr) _ =
  let headers =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (k, v) -> Hashtbl.replace tbl (String.lowercase_ascii k) v)
      req.additional_headers ;
    tbl
  in
  (* It's important to mark the request as "should close", or else the
     server may write to our descriptor (after returning from
     here). *)
  req.close <- true ;
  match parse_upgrade_headers headers with
  | Some
      ( {upgrade= "websocket"; connection= "Upgrade"; websocket_version= "13"; _}
        as req
      ) ->
      let hash = challenge_response req.websocket_key in
      reply_switching_protocols fd hash ;
      let fd = Unix.dup fd in
      Server.register_client server fd ;
      D.debug "Handed file descriptor off to websocket server"
  | _ ->
      reply_upgrade_required fd
