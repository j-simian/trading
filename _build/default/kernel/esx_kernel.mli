open! Core
open! Async

module Message : sig 
  module Message_type : sig
    type t = 
      | Connect of string
      | Connect_ack
    [@@deriving bin_io, sexp, yojson]
  end

  type t =  
    { message_id : int
    ; message_type : Message_type.t }
  [@@deriving bin_io, sexp, yojson]

  val to_string : t -> string
  val of_string : string -> t Or_error.t

end
