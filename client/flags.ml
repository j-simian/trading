open! Core
open! Esx_kernel
open! Command.Param

let server_flag = 
  flag 
    "-server" 
    (required Command.Param.host_and_port)
    ~doc:"IP Address of the server to connect to" 

let quantity_flag = 
  flag
    "-quantity"
    (required int)
    ~doc:"The quantity to place for the order"

let price_flag =
  flag 
    "-price"
    (required int)
    ~doc:"The price to place the order at"



let symbol_flag = 
  flag 
    "-symbol"
    (required Symbol.arg_type)
    ~doc:"The symbol to place an order for"

let identifier_flag = 
  flag 
    "-identifier"
    (required string)
    ~doc:"A unique identifier for you"
