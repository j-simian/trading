open! Core
open! Async

module Message : sig 
    type t = 
        | Handshake_initiate of string
    [@@deriving bin_io, sexp]
end
