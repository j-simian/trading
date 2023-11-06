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
      type t = { balance: int
               ; positions: int Symbol.Map.t
               }

      let create () = { balance = 10000
                      ; positions = Symbol.Map.empty}
    end

    type t = Client_data.t String.Map.t

    let create () = String.Map.empty
  end

  type t = 
    { mutable clients_by_address : Clients_by_address.t
    ; mutable clients_data : Clients_data.t
    ; mutable order_book: Order_book.t
    }

  let create () = 
    { clients_by_address = Clients_by_address.create ()
    ; clients_data = Clients_data.create ()
    ; order_book = Order_book.create ()
    }
end

(* Helper function to match orders at a specific price level *)
let rec match_orders remaining_quantity matched_fills (orders : Order.t list) =
  match orders with
  | [] -> (matched_fills, remaining_quantity, [])  
  | order::rest ->
    if order.quantity <= remaining_quantity then
      (* Fully fill the order and continue matching *)
      let fill = 
        { Fill.buyer = order.party
        ; seller = order.party
        ; price = order.price
        ; quantity = order.quantity
        ; symbol = order.symbol 
        } in
      match_orders 
        (remaining_quantity - order.quantity) 
        (fill::matched_fills) 
        rest
    else
      (* Partially fill the order and stop matching *)
      let partial_fill = 
        { Fill.buyer = order.party
        ; seller = order.party
        ; price = order.price
        ; quantity = remaining_quantity
        ; symbol = order.symbol 
        } in
      (partial_fill::matched_fills, 
       0, 
       {order with quantity = order.quantity - remaining_quantity}::rest)

let collect_matching_orders 
    ~(symbol_orders : Order_book.Symbol_orders.t) 
    ~(order: Order.t) =
  let order_map = match order.dir with
    | Buy -> symbol_orders.asks
    | Sell -> symbol_orders.bids
  in
  let matching_price_levels = Map.filter_keys order_map ~f:(fun price ->
      match order.dir with
      | Buy -> price <= order.price
      | Sell -> price >= order.price
    ) in
  let fills, remaining_quantity, new_order_map = 
    Map.fold matching_price_levels ~init:([], order.quantity, order_map)
      ~f:(fun ~key:price ~data:orders (acc_fills, acc_qty, acc_map) ->
          if acc_qty > 0 then
            let new_fills, new_qty, new_orders = 
              match_orders acc_qty acc_fills orders 
            in
            let new_map = Map.set acc_map ~key:price ~data:new_orders in
            (new_fills, new_qty, new_map)
          else
            (acc_fills, acc_qty, acc_map)
        )
  in
  let new_symbol_orders = match order.dir with
    | Buy -> {symbol_orders with asks = new_order_map}
    | Sell -> {symbol_orders with bids = new_order_map}
  in
  (fills, remaining_quantity, new_symbol_orders)

let update_order_book 
    ~(state: State.t) 
    ({ dir; price; quantity = _; symbol; party = _ } as order : Order.t) =
  let symbol_orders = 
    Map.find_exn state.order_book symbol 
  in
  let updated_symbol_orders = 
    match dir with 
    | Buy -> 
      { symbol_orders with bids = Map.add_multi 
                               symbol_orders.bids 
                               ~key:price 
                               ~data:order }
    | Sell -> 
      { symbol_orders with asks = Map.add_multi 
                               symbol_orders.asks 
                               ~key:price 
                               ~data:order }
  in
  Map.set state.order_book ~key:order.symbol ~data:updated_symbol_orders

let handle_order ~writer ~(state : State.t) (order : Order.t) = 
  let symbol_orders = Map.find_exn state.order_book order.symbol in
  let fills, remaining_quantity, symbol_orders = 
    collect_matching_orders ~symbol_orders ~order 
  in
  (* Send any fills generated, and update the state *)
  let%bind () = Deferred.List.iter ~how:`Sequential fills ~f:(fun fill -> 
      state.clients_data <- Map.update state.clients_data fill.buyer ~f:(fun party_data -> 
          match party_data with
          | None -> Log.Global.error_s [%message "Failed to find party's data" (order.party)];
            State.Clients_data.Client_data.create ()
          | Some party_data -> 
            let new_positions = 
              Map.update party_data.positions fill.symbol ~f:(fun symbol_position -> 
                  match symbol_position with
                  | None -> fill.quantity
                  | Some quantity -> quantity + fill.quantity
                ) in
            { balance = party_data.balance - fill.price * fill.quantity; positions = new_positions }
        );
      state.clients_data <- Map.update state.clients_data fill.seller ~f:(fun party_data -> 
          match party_data with
          | None -> Log.Global.error_s [%message "Failed to find seller data" (order.party)];
            State.Clients_data.Client_data.create ()
          | Some party_data -> 
            let new_positions = 
              Map.update party_data.positions fill.symbol ~f:(fun symbol_position -> 
                  match symbol_position with
                  | None -> -fill.quantity
                  | Some quantity -> quantity - fill.quantity
                ) in
            { balance = party_data.balance + fill.price * fill.quantity; positions = new_positions }
        );
      let fill_message = 
        { Message.message_id = 0
        ; message_type = Message.Message_type.Fill fill 
        } 
      in
      let%bind () = Pipe.write writer (Message.to_string fill_message) in
      Log.Global.info_s [%message "Order filled" (order : Order.t) (fill : Fill.t)];
      return ()
    ) in
  (* Update the order book by removing any orders filled. *)
  state.order_book <- 
    Map.set state.order_book ~key:order.symbol ~data:symbol_orders;
  (* Add an order if the order was not completely filled. *)
  (if remaining_quantity > 0 then
     let remaining_order : Order.t = 
       { order with quantity = remaining_quantity } 
     in
     let order_book = update_order_book ~state remaining_order in
     state.order_book <- order_book);
  let ack_message : Message.t = 
    {message_id =0; message_type = Message.Message_type.Order_ack} in
  let%bind () = Pipe.write writer (Message.to_string ack_message) in
  return ()

(* let handle_book_request ~writer ~(state : State.t) = *) 


let handle_connect ~writer ~addr ~(state : State.t) identifier = 
  let address = 
    (Socket.Address.Inet.to_host_and_port addr) |> Host_and_port.host 
  in
  (match 
     Map.add state.clients_by_address ~key:address ~data:identifier,
     Map.add 
       state.clients_data
       ~key:identifier 
       ~data:(State.Clients_data.Client_data.create ())
   with
   | `Duplicate, _ -> 
     Log.Global.error_s 
       [%message 
         "Failed to add to clients - Client already connected:" 
           address identifier];
     return ()
   | _,  `Duplicate -> 
     Log.Global.error_s 
       [%message 
         "Failed to add to clients - Identifier already in use" 
           address identifier];
     return ()
   | `Ok addresses, `Ok data -> 
     state.clients_by_address <- addresses; 
     state.clients_data <- data; 
     Log.Global.info_s [%message "Added to clients" address];
     let ack_message: Message.t = 
       { message_id = 0; message_type = Message.Message_type.Connect_ack } 
     in
     Pipe.write writer (Message.to_string ack_message))
;;

let handle_message ~writer ~addr ~(state:State.t) message = 
  Log.Global.info_s [%message (message : Message.t)];
  let { message_id; message_type }: Message.t = message in
  ignore message_id;
  let%bind () = 
    match (message_type : Message.Message_type.t) with
    | Connect_ack | Order_ack | Order_reject _ | Fill _ -> 
      Log.Global.error_s 
        [%message 
          "Received unexpected message from client" 
            (message : Message.t)]; 
      return ()
    | Connect identifier -> 
      handle_connect ~writer ~addr ~state identifier
    | Order order -> handle_order ~writer ~state order
    | _ -> return ()
    (* | Get_book -> handle_book_request ~writer ~state *)
  in
  Log.Global.info_s [%message (state.order_book : Order_book.t)];
  return ()

let parse_message message = 
  Message.of_string message
