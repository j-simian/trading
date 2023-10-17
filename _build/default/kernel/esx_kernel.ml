open! Core
open! Async

module Message = struct
    type t = 
        | Handshake_initiate of string
    [@@deriving bin_io, sexp]
end
