open! Core
open! Async
open! Esx_kernel
open Message_handler

let start_server port = 
  let state = State.create () in
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun addr reader writer -> 
         let writer, reader = Writer.pipe writer, Reader.pipe reader in
         let%bind () = 
           Pipe.iter reader
             ~f:(fun line ->
                 let%bind message = parse_message line |> return in
                 match message with 
                 | Ok message -> handle_message ~addr ~writer ~state message 
                 | Error _ -> return ()
               ) 
         in
         return ()
      )
  in
  ignore host_and_port;
  ()

let start_server_command = 
  Command.async_or_error
    ~summary:"Starts running the server"
    (let%map_open.Command () = return ()
     and port = flag "-port" (optional_with_default 44780 int) ~doc:"INT Port to listen on (default 44780)"
     in 
     start_server port;
     fun() -> 
       Deferred.never ())


let command = 
  Command.group 
    ~summary:"Trading exchange simulator server"
    ["start", start_server_command]

let () = Command_unix.run command
