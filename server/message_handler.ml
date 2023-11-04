open! Core
open! Async
open! Esx_kernel

module State = struct
  module Clients_by_address = struct
    type t = string String.Map.t

    let create () = String.Map.empty
  end

  module Clients_data = struct
    module Client_data = struct
      type t = { balance: int }

      let create () = { balance = 10000 }
    end

    type t = Client_data.t String.Map.t

    let create () = String.Map.empty
  end

  type t = 
    { mutable clients_by_address : Clients_by_address.t
    ; mutable clients_data : Clients_data.t
    }

  let create () = 
    { clients_by_address = Clients_by_address.create ()
    ; clients_data = Clients_data.create ()
    }
end

let handle_connect ~writer ~addr ~(state : State.t) identifier = 
  let address = (Socket.Address.Inet.to_host_and_port addr) |> Host_and_port.host in
  (match 
     Map.add state.clients_by_address ~key:address ~data:identifier,
     Map.add 
       state.clients_data
       ~key:identifier 
       ~data:(State.Clients_data.Client_data.create ())
   with
   | `Duplicate, _ -> Log.Global.error_s [%message "Failed to add to clients - Client already connected:" address identifier]
   | _,  `Duplicate -> Log.Global.error_s [%message "Failed to add to clients - Identifier already in use" address identifier]
   | `Ok addresses, `Ok data -> 
     state.clients_by_address <- addresses; 
     state.clients_data <- data; 
     Log.Global.info_s [%message "Added to clients" address];
     let ack_message: Message.t = 
       { message_id = 0; message_type = Message.Message_type.Connect_ack } 
     in
     Writer.write writer (Message.to_string ack_message))

let handle_message ~writer ~addr ~(state:State.t) message = 
  Log.Global.info_s [%message (message : Message.t)];
  let { message_id; message_type }: Message.t = message in
  ignore message_id;
  let () = 
    match message_type with
    | Message.Message_type.Connect_ack -> 
      Log.Global.error_s [%message "Received Connect_ack from client."] 
    | Message.Message_type.Connect identifier -> 
      handle_connect ~writer ~addr ~state identifier
  in
  return ()

let parse_message message = 
  Message.of_string message
