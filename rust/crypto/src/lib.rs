#[macro_use]
extern crate lazy_static;
extern crate num;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
pub mod utils {
    use std::iter::FromIterator;
    use std::iter::Iterator;
    use std::collections::HashMap;
    use std::ops::Shr;
    use std::ops::Shl;
    use std::ops::BitOr;
    use std::mem::size_of;
    use std::{cmp::max, convert::TryInto};



    fn to_ascii(word: &Vec<u8>) -> Vec<char> {
        word.clone().into_iter().map(|x| x as char).collect()
    }

    fn from_ascii(character: &Vec<char>) -> Vec<u8> {
        character.clone().into_iter().map(|x| x as u8).collect()
    }

    struct Encoding {
        to: HashMap<u8,char>,
        from: HashMap<char,u8>
    }

    fn encoding_from_list(pair: Vec<(u8,char)>) -> Encoding {
        Encoding {
            to: HashMap::from_iter(pair.clone().into_iter()),
            from: pair.clone().into_iter().map(|p| (p.1,p.0)).collect()
        }
    }

    lazy_static! { static ref HEX: Encoding = encoding_from_list(
                            (0..15).zip((48u8..58).chain(97..103).map(|x| x as char))
                            .collect());
    }

    lazy_static! { static ref B64: Encoding = encoding_from_list(
                            (0..63).zip((0x41u8..0x5B)
                            .chain(0x61..0x7B).chain(0x30..0x3A)
                            .chain(0x2B..=0x2B).chain(0x2F..=0x2F).map(|x| x as char))
                            .collect());
    }

    fn in_encoding(encoding: &Encoding, character: &char) -> bool
    {
        encoding.from.keys().any(|c| c == character)
    }

    fn char_to_integral<T>(encoding: &Encoding, character: &char) -> T
    where T: From<u8>
    {
        T::from(encoding.from[character])
    }

    fn get_byte<T,S>(shiftable: T,byte: S) -> u8
    where u64: From<T>,
          T: Shr<S, Output = T>
    {
       u64::from(shiftable >> byte) as u8
    }

    fn to_bytes<T>(val: T) -> Vec<u8>
    where u64: From<T>,
          T: Shr<u32, Output = T>,
          T: Copy
    {
        (0..size_of::<T>() as u32).map(|i| get_byte(val, i)).collect()
    }

    //Takes list of bytes and returns them as a single number
    fn from_bytes<T>(bytes: &Vec<u8>) -> T
    where T: From<u8>,
          T: Shl<u8, Output = T>,
          T: Copy,
          T: BitOr<T, Output = T>,
          T: num::Zero
    {
        let mut v = bytes.clone();
        v.reverse();
        (0u8..).step_by(8).zip(v)
        .map(|x| (T::from(x.1) << x.0))
        .fold(num::Zero::zero(), |acc, x| acc | x)
    }

    fn get_num_of_bits(word:u32) -> u32
    {
        (word as f32).log2().ceil() as u32
    }

    fn encode_block<T>(e: &Encoding, block: T) -> Vec<char>
    where T: TryInto<u8>,
          T: Into<u32>,
          T: Shl<u32, Output = T>,
          T: Shr<u32, Output = T>,
          T: Copy,
          <T as std::convert::TryInto<u8>>::Error : std::fmt::Debug
    {
        let num_bits_in_endcoding = get_num_of_bits(e.from.len() as u32) as f32;
        let mut out : Vec<char> = Vec::new();
        let length_of_block = size_of::<T>() as f32;
        for i in (0..((length_of_block/num_bits_in_endcoding).ceil() as u32)).step_by(num_bits_in_endcoding as usize)
        {
            let mut bits_out = block << i;
            bits_out = bits_out >> (size_of::<T>() as u32) - (num_bits_in_endcoding as u32);
            out.push(e.to[&bits_out.try_into().expect("truncation error that should be impossible")]);
        }
        out
    }

    fn decode_block<T>(encoding: &Encoding, encoded: &Vec<char>) -> T
    where T: From<u8>,
          T: Shl<u32, Output = T>,
          T: Shr<u32, Output = T>,
          T: Copy,
          T: BitOr<T, Output = T>,
          T: num::Zero
    {
        let num_bits_in_encoding = get_num_of_bits(encoding.from.len() as u32);
        let zeroes = (num_bits_in_encoding * encoded.len() as u32) % 8;
        let mut bytes = encoded.iter().map(|c| encoding.from[&c]).rev();
        let mut out: T = num::Zero::zero();
        let mut position = 0;
        for b in bytes
        {
            let x = T::from(b) << position;
            position = position + 8;
            out = out | x;
        }
        out >> zeroes
    }

    fn hex_to_bytes(s: &Vec<char>) -> Vec<u8>
    {
        if s.len() % 2 != 0 {
            panic!("not a valid hex string")
        }
        s.chunks(2).map(|ls| decode_block(&HEX,&(ls.to_vec()))).collect()
    }

    fn base64_to_bytes(s: &Vec<char>) -> Vec<u8>
    {
        let filtered : Vec<char> = s.clone().into_iter().filter(|x|*x!='=').collect();
        filtered.chunks(4).flat_map(|ls|
            {
                let l = ls.len();
                let drops = match l
                {
                    4 => 1,
                    3 => 1,
                    2 => 2,
                    1 => 1,
                    _ => panic!("Nani!")
                };
                let block: u32 = decode_block(&B64,&(ls.to_vec()));
                to_bytes(block).into_iter().rev().skip(drops)
            }).collect()
    }

    fn bytes_to_base64(b: &Vec<u8>) -> Vec<char>
    {
        //Number of bytes given
        let size: usize = b.len() * 4 / 3;
        //Break bytes into chunks of three, i.e. one block, pad with zeros, and convert to u32
        //Recall that each b64 is 6 bits, and each byte is 8 bits. lcm(6,8) = 24
        let mut blocks:  Vec<char> = b.clone().chunks(3).into_iter()
                    .map(|chungus| chungus.into_iter().cloned().chain(std::iter::repeat(0)).take(4).collect())
                    .flat_map(|chungus| encode_block(&B64,from_bytes::<u32>(&chungus)))
                    .collect();
        let tail = if size % 4 != 0 { 4 - (size % 4) }else {0};
        let mut tailList : Vec<char> = std::iter::repeat('=').take(tail).collect();
        blocks.append(&mut tailList);
        blocks
    }

    fn bytes_to_hex(b: Vec<u8>) -> Vec<char>
    {
        b.clone().into_iter().flat_map(|byte| encode_block(&HEX,byte)).collect()
    }

    //Xors each byte. If lists are different lengths, it returns a list the length of the shortest list
    fn blockXor(bytes1: Vec<u8>, bytes2: Vec<u8>) -> Vec<u8>
    {
        bytes1.into_iter().zip(bytes2.into_iter()).map(|(b1,b2)| b1 ^ b2).collect()
    }

    fn popcount(n: u8) -> u8
    {
        let mut p: u8 = 0;
        for i in 0..8
        {
            p += (n >> i) & 1;
        }
        p
    }

    fn hammingDistance(bytes1: Vec<u8>, bytes2: Vec<u8>) -> usize
    {
        bytes1.into_iter().zip(bytes2.into_iter())
            .fold(0, |acc, (b1,b2)| acc + popcount(b1^b2)) as usize
    }

    fn isBase64(character: &char) -> bool
    {
        in_encoding(&B64, character)
    }

    


}

#[cfg(test)]
mod tests {
    const test_string : str = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";

    const base64_result : str = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=";

    const hex_result : str = "4d616e2069732064697374696e677569736865642c206e6f74206f6e6c792062792068697320726561736f6e2c2062757420627920746869732073696e67756c61722070617373696f6e2066726f6d206f7468657220616e696d616c732c2077686963682069732061206c757374206f6620746865206d696e642c20746861742062792061207065727365766572616e6365206f662064656c6967687420696e2074686520636f6e74696e75656420616e6420696e6465666174696761626c652067656e65726174696f6e206f66206b6e6f776c656467652c2065786365656473207468652073686f727420766568656d656e6365206f6620616e79206361726e616c20706c6561737572652e";

    const hex : str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";

    const base64 : str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    #[test]
    fn hex_to_bytes_test(){
        assert_eq!(base64,bytes_to_base64(hex_to_bytes(hex.into_string()))
    }
    
}
