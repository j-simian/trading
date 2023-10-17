open! Core
open! Async

let start_server_command = 
    Command.async_or_error
        ~summary:"Starts running the server"
        (let%map_open.Command () = return () in 
        let open Deferred.Or_error.Let_syntax in
        fun() -> return ())


let command = 
    Command.group 
        ~summary:"Trading exchange simulator server"
        ["start", start_server_command]

let () = Command_unix.run command
