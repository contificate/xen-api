== Bring server up
  $ trap 'killall $(jobs -p) 2> /dev/null' EXIT
  $ ./test_server.exe &
  Fatal error: exception Unix.Unix_error(Unix.EADDRINUSE, "bind", "")
  $ sleep 0.1

== Normal
  $ ./test_client.exe --perf > /dev/null

== Expect to log after a closed connection
  $ ./test_client.exe --logerr > result
  $ grep "ECONNRESET" result -c
  1
  $ grep "backtrace" result -c
  11
  $ grep "Called from" result -c
  8
