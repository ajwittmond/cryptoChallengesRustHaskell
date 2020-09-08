extern crate crypto;

use crypto::vigenere::byte_utils;
use crypto::vigenere::byte_utils::{HEX,Decodable,Encodable};
use crypto::vigenere;
use std::env;

const cypher_text : &str = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736";

fn main() -> std::io::Result<()>{
    let bytes: Vec<u8> = cypher_text.chars().decode(&HEX).collect();
    let path = env::current_dir()?;
    println!("the current dir is {}", path.display());
    let english_freqs = vigenere::load_frequency_csv("../../englishFreq2_rust.csv")?;
    let keys = vigenere::keys_ranked(&english_freqs, (0..=255).map(|x| (x..=x)), 2.0, &bytes[..] );
    println!("here's the best guess:");
    println!("key {}",keys[0].0[0]);
    println!("plain text {:?}",keys[0].1);
    Ok(())
}
