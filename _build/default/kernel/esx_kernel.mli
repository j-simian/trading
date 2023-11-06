open! Core
open! Async

module Direction : sig
  type t = Buy | Sell [@@deriving sexp, yojson, equal]
end

module Symbol : sig
  type t = Cash [@@deriving sexp, yojson, enumerate]

  val of_string : string -> t Or_error.t

  module Map : sig
    type key = t
    include Map.S with type Key.t := key
  end

  val arg_type : t Command.Arg_type.t
end

module Order : sig
  type t = 
    { dir: Direction.t
    ; quantity: int
    ; price: int
    ; symbol: Symbol.t
    ; party: string
    } [@@deriving sexp, yojson]
end

module Fill : sig
  type t = {
    buyer: string
  ; seller : string
  ; price: int
  ; quantity: int
  ; symbol : Symbol.t
  } [@@deriving sexp, yojson]
end

module Order_book : sig
  module Symbol_orders : sig
    type t = {
      bids: Order.t list Int.Map.t
    ; asks: Order.t list Int.Map.t
    } [@@deriving sexp]

    val create : unit -> t 
  end

  type t = Symbol_orders.t Symbol.Map.t [@@deriving sexp]

  val create : unit -> t
end


module Message : sig 
  module Message_type : sig
    type t = 
      | Connect of string
      | Connect_ack
      | Order of Order.t
      | Order_ack
      | Order_reject of string
      | Fill of Fill.t
      | Get_book
    [@@deriving sexp, yojson]
  end

  type t =  
    { message_id : int
    ; message_type : Message_type.t }
  [@@deriving sexp, yojson]

  val to_string : t -> string
  val of_string : string -> t Or_error.t

end
