open! Core
open! Async

let authorise = 
    Command.async_or_error
        ~summary:"Registers and authorises with the server"
        (let%map_open.Command () = return () in 
        let open Deferred.Or_error.Let_syntax in
        fun() -> return ())


let command = 
    Command.group 
        ~summary:"Master client commander"
        ["authorise", authorise]

let () = Command_unix.run command
