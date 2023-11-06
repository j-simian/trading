open! Core
open! Async

module Direction = struct
  type t = Buy | Sell [@@deriving sexp, yojson, equal]
end

module Symbol = struct
  module T = struct
    type t = Cash [@@deriving sexp, yojson, enumerate, compare]

    let to_string t = sexp_of_t t |> Sexp.to_string
    let of_string str = 
      try 
        Ok (t_of_sexp (Sexp.of_string str))
      with _ -> Error (Error.create_s [%message "Unable to parse symbol"])
  end

  module Map = struct
    type key = T.t
    include Map.Make(T)
  end
  include T

  let arg_type = Command.Arg_type.enumerated ~case_sensitive:false (module T)
end

module Order = struct
  type t = 
    { dir: Direction.t
    ; quantity: int
    ; price: int
    ; symbol: Symbol.t
    ; party: string
    } [@@deriving sexp, yojson]
end

module Fill = struct
  type t = {
    buyer: string
  ; seller : string
  ; price: int
  ; quantity: int
  ; symbol : Symbol.t
  } [@@deriving sexp, yojson]
end

module Order_book = struct
  module Symbol_orders = struct
    type t = {
      bids: Order.t list Int.Map.t
    ; asks: Order.t list Int.Map.t
    } [@@deriving sexp]

    let create () : t = { bids = Int.Map.empty; asks = Int.Map.empty }
  end

  type t = Symbol_orders.t Symbol.Map.t [@@deriving sexp]

  let create () = 
    let empty = Symbol.Map.empty in
    List.fold Symbol.all ~init:empty ~f:(fun acc symbol -> 
        Map.add_exn acc ~key:symbol ~data:(Symbol_orders.create ()))
end

module Message = struct
  module Message_type = struct
    type t = Connect of string 
           | Connect_ack
           | Order of Order.t
           | Order_ack 
           | Order_reject of string 
           | Fill of Fill.t
           | Get_book
    [@@deriving sexp, yojson]

  end

  type t = 
    { message_id: int;
      message_type: Message_type.t} 
  [@@deriving sexp, yojson]

  let to_string t = (to_yojson t |> Yojson.Safe.to_string) ^ "|"

  let of_string string = 
    let message = String.take_while string ~f:(fun ch -> not (Char.equal ch '|')) in
    try 
      let json =  Yojson.Safe.from_string message  in
      let result = of_yojson json in 
      match result with
      | Ok message -> Ok message
      | Error error -> Error (Error.create_s [%message "Failed to parse json" string error])
    with 
    | Yojson.Json_error msg -> Error (Error.create_s [%message "Failed to parse json" string msg])
    | exn -> Error (Error.create_s [%message "Unexpected exception" (Exn.to_string exn)])
end
