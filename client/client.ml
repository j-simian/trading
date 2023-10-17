open! Core
open! Async
open Esx_kernel

let authorise = 
    Command.async_or_error
        ~summary:"Registers and authorises with the server"
        (let%map_open.Command () = return () 
        and _server = 
            flag 
                "-server" 
                (required Command.Param.host_and_port)
                ~doc:"IP Address of the server to connect to" in
        let open Deferred.Or_error.Let_syntax in
        let _pipe = 
                Async_rpc_kernel.Rpc.Pipe_rpc.create 
                    ~name:"ESX_server" 
                    ~version:1
                    ~bin_query:[%bin_type_class: Message.t]
                    ~bin_response:[%bin_type_class: Message.t Or_error.t]
                    ~bin_error:[%bin_type_class: unit]
                    ()
        in
        fun() -> return ())


let command = 
    Command.group 
        ~summary:"Master client commander"
        ["authorise", authorise]

let () = Command_unix.run command
