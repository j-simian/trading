open! Core
open! Async
open! Esx_kernel

module State : sig
  module Clients_by_address : sig
    type t = string String.Map.t

    val create : unit -> t
  end

  module Clients_data : sig
    module Client_data : sig
      type t = { balance: int }

      val create : unit -> t
    end

    type t = Client_data.t String.Map.t

    val create : unit -> t
  end

  type t = 
    { mutable clients_by_address : Clients_by_address.t
    ; mutable clients_data : Clients_data.t
    }

  val create : unit -> t
end

val handle_connect : writer:Writer.t -> addr:Socket.Address.Inet.t -> state:State.t -> string -> unit

val handle_message : writer:Writer.t -> addr:Socket.Address.Inet.t -> state:State.t -> Message.t -> unit Deferred.t  

val parse_message : string -> Message.t Or_error.t
