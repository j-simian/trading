open! Core
open! Async

module Handshake_initiate : sig 
    type t

    val create : string -> t
end
