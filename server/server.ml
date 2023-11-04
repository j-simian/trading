open! Core
open! Async
open! Esx_kernel

module State = struct
  module Clients = struct
    type t = string Map.M(String).t

    let empty = String.Map.empty
  end

  type t = {
    mutable clients : Clients.t
  }

  let create = 
    { 
      clients = Clients.empty
    }
end

let handle_message ~writer ~addr ~(state:State.t) message = 
  Log.Global.info_s [%message (message : Message.t)];
  let { message_id; message_type }: Message.t = message in
  ignore message_id;
  let () = 
    match message_type with
    | Message.Message_type.Connect identifier -> 
      (match  Map.add state.clients ~key:(Socket.Address.Inet.to_string addr) ~data:identifier with
       | `Ok result -> state.clients <- result
       | `Duplicate -> failwith "Client already exists");
      let ack_message: Message.t = 
        { message_id = 0; message_type = Message.Message_type.Connect_ack } 
      in
      Writer.write writer (Message.to_string ack_message)
    | _ -> () 
  in
  return ()

let parse_message message = 
  Message.of_string message

let start_server_command = 
  Command.async_or_error
    ~summary:"Starts running the server"
    (let%map_open.Command () = return ()
     and port = flag "-port" (optional_with_default 44780 int) ~doc:"INT Port to listen on (default 44780)"
     in 
     let state = State.create in
     let host_and_port =
       Tcp.Server.create
         ~on_handler_error:`Raise
         (Tcp.Where_to_listen.of_port port)
         (fun addr reader writer -> 
            let reader = Reader.pipe reader in
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
     fun() -> 
       Deferred.never ())


let command = 
  Command.group 
    ~summary:"Trading exchange simulator server"
    ["start", start_server_command]

let () = Command_unix.run command
