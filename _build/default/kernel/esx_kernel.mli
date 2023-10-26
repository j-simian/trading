open! Core
open! Async

module Message : sig 
  module Message_type : sig
    type t = 
      | Handshake_initiate of string
    [@@deriving bin_io, sexp]
  end

  type t =  
    { message_id : int
    ; message_type : Message_type.t }
  [@@deriving bin_io, sexp]
end
