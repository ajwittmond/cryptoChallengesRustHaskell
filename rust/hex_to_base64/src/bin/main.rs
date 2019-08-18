use std::env;

extern crate hex_to_base64;

use hex_to_base64::byte_utils;




fn main() {
    let args: Vec<String> = env::args().collect();
    let input = &args[1];
    //while index + 3 <= input.len(){
    //}
    println!("{}",byte_utils::bytes_to_base64(
            &byte_utils::hex_to_bytes(input).expect("malformed input")
        ));
    ()
}
