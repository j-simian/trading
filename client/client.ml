open! Core
open! Async
open! Esx_kernel

module State = struct
  type t = 
    { identifier : string
    ; reader : string Pipe.Reader.t
    ; writer : string Pipe.Writer.t
    }
end

let option_to_error opt = 
  match opt with
  | Some x -> Ok x
  | None -> 
    Error (Error.create_s [%message "Received None value instead of Some"])

let string_to_int_or_error x = 
  Int.of_string_opt x |> option_to_error

let write string = 
  Writer.write
    (Lazy.force Writer.stdout) 
    string


let _order ~dir ~quantity ~price ~symbol ~(state:State.t) =
  let order : Order.t = 
    { dir
    ; quantity
    ; price
    ; symbol
    ; party=state.identifier 
    } in
  let message : Message.t =
    { message_id = 0; message_type = Message.Message_type.Order order }
  in
  let%bind () = 
    Pipe.write state.writer (Message.to_string message) 
  in
  let%bind line = Pipe.read state.reader in
  (match line with
   | `Eof -> Log.Global.error_s [%message "Server disconnected unexpectedly"]
   | `Ok line -> 
     (match (Message.of_string line) with
      | Ok ({ message_type = Order_ack; message_id = _ }) -> 
        Log.Global.info_s 
          [%message "Order accepted by server" (order : Order.t)]
      | Ok ({ message_type = Order_reject reason; message_id = _ }) -> 
        Log.Global.info_s 
          [%message "Order rejected by server" (order : Order.t) (reason)]
      | Ok message -> 
        Log.Global.error_s 
          [%message "Unexpected response from server" (message : Message.t)]
      | Error e -> 
        Log.Global.error_s 
          [%message "Failed to parse server response" (e:Error.t)]));
  return ()

let parse_param ~key ?abbrev params ~of_string = 
  let pair = 
    List.find params 
      ~f:(fun word -> 
          (String.is_prefix word ~prefix:(String.append key "=") ||
           (match abbrev with 
            | None -> false 
            | Some abbrev -> 
              String.is_prefix word ~prefix:(String.append abbrev "=")))) 
  in
  match pair with
  | None -> Error (Error.create_s [%message "Couldn't find parameter"])
  | Some pair -> 
    match String.split ~on:'=' pair with
    | [ _; value ] -> value |> of_string
    | _ -> Error (Error.create_s [%message "Badly formatted key/value pair"])


let parse_command command ~(state : State.t)= 
  let words = String.split ~on:' ' command in
  let quantity = 
    parse_param 
      ~key:"quantity" 
      ~abbrev:"q" 
      ~of_string:string_to_int_or_error
      words
  in
  let price = 
    parse_param 
      ~key:"price" 
      ~abbrev:"p" 
      ~of_string:string_to_int_or_error 
      words
  in
  let symbol = 
    parse_param 
      ~key:"symbol" 
      ~abbrev:"s" 
      ~of_string:(Symbol.of_string) 
      words 
  in
  match words with
  | "buy"::_ | "sell"::_ -> 
    (match quantity, price, symbol with
     | Ok quantity, Ok price, Ok symbol -> 
       let message : Message.t = 
         { message_id = 0
         ; message_type = 
             Message.Message_type.Order 
               { dir = 
                   if String.equal (List.hd_exn words) "buy" 
                   then Buy 
                   else Sell
               ; party=state.identifier
               ; quantity
               ; price
               ; symbol 
               }
         } 
       in
       Some message
     | _, _, _ ->
       None)
  | unknown::_ -> 
    Log.Global.error_s [%message "Unknown command" unknown]; 
    None
  | _ -> 
    Log.Global.error_s [%message "Badly formatted command" command]; 
    None

let rec read_from_user ~(state : State.t) = 
  let%bind command = Reader.read_line (Lazy.force Reader.stdin) in
  let%bind () = 
    match command with
    | `Eof -> 
      return ()
    | `Ok cmd-> 
      (match parse_command cmd ~state with
       | Some message -> 
         Pipe.write state.writer (Message.to_string message) 
       | None -> 
         Log.Global.error_s [%message "Unable to parse command."]; 
         return ())
  in
  read_from_user ~state

let rec read_from_server ~(state : State.t) = 
  let%bind line = Pipe.read state.reader in
  let%bind () = 
    match line with
    | `Eof -> failwith "Unexpectedly disconnected from server!"
    | `Ok line -> 
      match Message.of_string line with
      | Error e -> 
        Log.Global.error_s 
          [%message "Failed to parse message from the server!" (e : Error.t)];
        return ()
      | Ok message -> 
        (write [%string "%{message#Message}\n"]);
        return ()
  in
  read_from_server ~state


let connect ~identifier ~server = 
  Tcp.with_connection
    (Tcp.Where_to_connect.of_host_and_port server)
    (fun _ reader writer ->
       let reader, writer = Reader.pipe reader, Writer.pipe writer in
       let state : State.t = 
         { 
           identifier;
           reader;
           writer 
         } in
       let message: Message.t = 
         { message_id = 0
         ; message_type = Message.Message_type.Connect "jsimian" 
         }
       in
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
       Deferred.both (read_from_user ~state) (read_from_server ~state)
    )

let start_command = 
  Command.async_or_error
    ~summary:"Registers and authorises with the server"
    (let%map_open.Command () = return () 
     and server = Flags.server_flag 
     and identifier = Flags.identifier_flag in
     fun() -> 
       let open Deferred.Or_error.Let_syntax in
       let%bind.Deferred _ = connect ~server ~identifier in
       return ()
    )


let command = 
  Command.group 
    ~summary:"Master client commander"
    ["start", start_command]

let () = Command_unix.run command
