open! Core
open! Async

module Handshake_initiate = struct
    type t = 
        Identity of string

    let create identity = Identity identity
end
