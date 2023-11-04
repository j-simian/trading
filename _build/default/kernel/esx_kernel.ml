open! Core
open! Async

module Message = struct
  module Message_type = struct
    type t = Connect of string 
           | Connect_ack
    [@@deriving bin_io, sexp, yojson]
  end

  type t = 
    { message_id: int;
      message_type: Message_type.t} 
  [@@deriving bin_io, sexp, yojson]

  let to_string t = to_yojson t |> Yojson.Safe.to_string

  let of_string string = 
    let result = Yojson.Safe.from_string string |> of_yojson in 
    match result with
    | Ok message -> Ok message
    | Error error -> Error (Error.create_s [%message "Failed to parse json" string error])
end
