extern crate hex_to_base64;
extern crate fixed_xor;

use hex_to_base64::byte_utils;
use fixed_xor::xor;

use std::env;

fn main() {
    let args : Vec<String> = env::args().collect();
    println!("{}",
             byte_utils::bytes_to_hex(
                 &xor::xor_vec(
                     &byte_utils::hex_to_bytes(&args[1]).expect("bad input"),
                     &byte_utils::hex_to_bytes(&args[2]).expect("bad input")
                 )));
}


