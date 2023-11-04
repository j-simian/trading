open! Core
open! Async
open! Esx_kernel

let authorise = 
  Command.async_or_error
    ~summary:"Registers and authorises with the server"
    (let%map_open.Command () = return () 
     and server = 
       flag 
         "-server" 
         (required Command.Param.host_and_port)
         ~doc:"IP Address of the server to connect to" in
     let open Deferred.Or_error.Let_syntax in
     let message: Message.t = { message_id = 0; message_type = Message.Message_type.Connect "jsimian" } in
     let start_client () =
       Tcp.with_connection
         (Tcp.Where_to_connect.of_host_and_port server)
         (fun _ reader writer ->
            let reader, writer = Reader.pipe reader, Writer.pipe writer in
            let%bind.Deferred () = 
              Pipe.write writer (Message.to_string message) 
            in
            let%bind.Deferred line = Pipe.read reader in
            (match line with 
             | `Eof -> Log.Global.info_s [%message "Got EOF"];
             | `Ok line -> 
               let message = Message.of_string line in
               Or_error.iter message ~f:(fun message -> 
                   Log.Global.info_s [%message (message : Message.t)]
                 ););
            return ())
     in
     fun() -> 
       start_client ()
    )


let command = 
  Command.group 
    ~summary:"Master client commander"
    ["authorise", authorise]

let () = Command_unix.run command
