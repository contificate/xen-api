(*
 * Copyright (C) 2026 Vates.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module C = Client.Client

let vm_export rpc session_id vm ~output =
  let uuid = C.VM.get_uuid ~rpc ~session_id ~self:vm in
  let args = ["vm-export"; "vm=" ^ uuid; "filename=" ^ output] in
  ignore (Qt.cli_cmd args : string)

let vm_import rpc session_id sr ~input =
  let sr = C.SR.get_uuid ~rpc ~session_id ~self:sr in
  let args = ["vm-import"; "filename=" ^ input; "sr-uuid=" ^ sr] in
  Qt.cli_cmd args
  |> String.split_on_char ','
  |> List.map (fun uuid -> C.VM.get_by_uuid ~rpc ~session_id ~uuid)

let exactly_one xs msg = match xs with [x] -> x | _ -> Alcotest.fail msg

(* This tests ensures that snapshot_of relations are preserved across
   a full export + reimport cycle. *)
let import_vdi_refs_test rpc session_id sr_info template () =
  let ( let@ ) = ( @@ ) in
  let sr = sr_info.Qt.sr in
  let@ vm = Qt.VM.with_new rpc session_id ~template ~sr in
  let vdi =
    C.VDI.create ~rpc ~session_id ~name_label:"foo" ~name_description:"" ~sR:sr
      ~virtual_size:10_000_000L ~_type:`user ~sharable:false ~read_only:false
      ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]
  in
  (* Create a snapshot and attach it to the VM. *)
  let snapshot = C.VDI.snapshot ~rpc ~session_id ~vdi ~driver_params:[] in
  C.VDI.set_name_label ~rpc ~session_id ~self:snapshot ~value:"bar" ;
  let _vbd =
    C.VBD.create ~rpc ~session_id ~vM:vm ~vDI:snapshot ~userdevice:"1"
      ~bootable:false ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false
      ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]
      ~device:"" ~currently_attached:false
  in
  let file = Printf.sprintf "/tmp/%s_test.xva" __MODULE__ in
  (* Export the VM and its disks. *)
  vm_export rpc session_id vm ~output:file ;
  let destroy_all vm vdi snapshot =
    C.VM.destroy ~rpc ~session_id ~self:vm ;
    let destroy_vdi vdi = C.VDI.destroy ~rpc ~session_id ~self:vdi in
    destroy_vdi vdi ; destroy_vdi snapshot
  in
  (* Destroy the VM and disks. *)
  destroy_all vm vdi snapshot ;
  (* Import the VM and its disks. *)
  let vm =
    let vms = vm_import rpc session_id sr ~input:file in
    exactly_one vms "Unexpected number of imported VMs."
  in
  let find_vdi name =
    let vdis = C.VDI.get_by_name_label ~rpc ~session_id ~label:name in
    exactly_one vdis
      (Printf.sprintf "Unexpected number of VDIs named %s (possibly 0)." name)
  in
  (* Find our imported VDIs by name. *)
  let vdi, snapshot = (find_vdi "foo", find_vdi "bar") in
  (* Check that the snapshot is linked correctly. *)
  let link = C.VDI.get_snapshot_of ~rpc ~session_id ~self:snapshot in
  if link = Ref.null || link <> vdi then
    Alcotest.fail
      (Printf.sprintf "Unexpected snapshot_of: %s" (Ref.string_of link)) ;
  (* Destroy everything. *)
  destroy_all vm vdi snapshot ;
  Unix.unlink file

let tests () =
  let open Qt_filter in
  [("import_vdi_refs", `Slow, import_vdi_refs_test)]
  |> conn
  |> sr SR.(all |> allowed_operations [`vdi_create])
  |> vm_template Qt.VM.Template.other
