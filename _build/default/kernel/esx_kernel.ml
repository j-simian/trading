open! Core
open! Async

module Message = struct
  module Message_type = struct
    type t = Handshake_initiate of string 
    [@@deriving bin_io, sexp]
  end

  type t = 
    { message_id: int;
      message_type: Message_type.t} 
  [@@deriving bin_io, sexp]
end
