
/// This module contains functions for encoding a decoding strings.
use std::iter::FromIterator;
use std::iter::Iterator;
use std::collections::HashMap;
use std::ops::Shr;
use std::ops::Shl;
use std::ops::{Mul, BitOr, BitAnd};
use std::mem::size_of;
use std::convert::TryInto;
use num::Integer;


/// This struct represents a way of encoding binary data as text.
/// For example, it might represent base64 or hex.
/// Two hashmaps are included for rapid encoding and decoding
pub struct Encoding {
    bits_per_char: u32,
    to: HashMap<u8,char>,
    from: HashMap<char,u8>,
    padding: char,
    lcm: u32
}

/// Consumes a list of key value pairs to generate an encoding
pub fn encoding_from_list(pair: Vec<(u8,char)>, padding: char) -> Encoding {
    let bits_per_char = get_num_of_bits(pair.len() as u32);
    let lcm = 8.lcm(&bits_per_char);
    if lcm > 64{
        panic!("invalid encoding size, lcm(8,bits_per_char) must be < 64")
    }else{
        Encoding {
            bits_per_char,
            to: HashMap::from_iter(pair.clone().into_iter()),
            from: pair.clone().into_iter().map(|p| (p.1,p.0)).collect(),
            padding,
            lcm
        }
    }
}

lazy_static! { pub static ref HEX: Encoding = encoding_from_list(
    (0..=15).zip(('0'..='9').chain('a'..='f'))
        .collect(),'=');
}

lazy_static! { pub static ref B64: Encoding = encoding_from_list(
    (0..=63).zip(('A'..='Z')
                 .chain('a'..='z').chain('0'..='9').chain(vec!['+','/']))
        .collect(),'=');
}

/// Tests if a char is a possible character value in the encoding.
/// For example: in_encoding(HEX,';') == False
pub fn in_encoding(encoding: &Encoding, character: &char) -> bool
{
    encoding.from.keys().any(|c| c == character)
}

/// Gives the integral value of a character using a encoding in a polymophic way.
/// So we can do
/// let x: u32 = char_to_integral(HEX, 'a');
/// and
/// let y : u16 = char_to_integral(HEX, 'b');
pub fn char_to_integral<T>(encoding: &Encoding, character: &char) -> T
where T: From<u8>
{
    T::from(encoding.from[character])
}

///gets the byte of an integral value at a given position
///get_byte(512,1)
pub fn get_byte<T,S>(shiftable: T,byte: S) -> u8
where T: Shr<S, Output = T> + Copy + TryInto<u8> + BitAnd<Output = T> + From<u8>,
      S: Mul<Output = S>,
      S: From<u8>
{
    match ((shiftable >> byte * S::from(8)) & T::from(255)).try_into() {
        Ok(a) => a,
        _ => unreachable!(),
    }
}


///Takes list of bytes and returns them as a single number in a polymorphic way.
///from_bytes(vec![1,0]) = 512
pub fn from_bytes<I,T>(bytes: I) -> (T,u32)
where T: From<u8>,
      T: Shl<u8, Output = T>,
      T: Copy,
      T: BitOr<T, Output = T>,
      T: num::Zero,
      I: IntoIterator<Item = u8>
{
    let block_size : u8 = match size_of::<T>().try_into(){
        Ok(x) => x,
        _  => panic!("invalid block size")
    };
    (0u8..).step_by(8).zip(bytes)
        .map(|x| (T::from(x.1) << 8 * (block_size - 1) - x.0))
        .fold((num::Zero::zero(), 0), |(acc,i), x| (acc | x , i + 1))
}

///returns the number of bits needed to store the passed value
pub fn get_num_of_bits(word:u32) -> u32
{
    (word as f32).log2().ceil() as u32
}

///Converts some integral value to its representation in the passed encoding
fn encode_block<T>(e: & 'static Encoding, block: T) -> impl Iterator<Item = char>
where T: TryInto<u8>,
      T: Shl<u32, Output = T>,
      T: Shr<u32, Output = T>,
      T: Copy,
<T as std::convert::TryInto<u8>>::Error : std::fmt::Debug
{
    let bits_in_block = ( size_of::<T>()  * 8 ) as u32;
    (0..e.lcm)
        .step_by(e.bits_per_char as usize)
        .zip(std::iter::repeat(block)).map(move |(i,block)|{
            let mut bits_out = block <<   i;
            bits_out = bits_out >> bits_in_block - (e.bits_per_char as u32);
            e.to[&bits_out.try_into().expect("truncation error that should be impossible")]
        })
}

///Takes a string and an encoding a puts the resulting bits into the desired integral value
fn decode_block<T,S>(encoding: &Encoding, encoded: S) -> (T,u32)
where T: From<u8>,
      T: Shl<u32, Output = T>,
      T: Shr<u32, Output = T>,
      T: Copy,
      T: BitOr<T, Output = T>,
      T: num::Zero,
      S: Iterator<Item = char>
{
    let mut padding = 0;
    let bytes = encoded.map(|c| {
        if c == encoding.padding {
            padding +=1;
            0
        } else {
            encoding.from[&c]
        }
    });
    let mut out: T = num::Zero::zero();
    let mut read = 0;
    for b in bytes
    {
        out = out << encoding.bits_per_char;
        out = out | T::from(b);
        read += 1;
    }
    (out ,read - padding)
}

///An iterator that buffers bytes into an integral value
///to encodes bytes into text in encodings that have uneven
///bit sizes
pub struct Encoder<J> {
    bytes_to_take: u32,
    position: u32,
    end: u32,
    buffer: [char ; 64],
    encoding : & 'static Encoding,
    byte_iterator: J 
}

impl<J> Iterator for Encoder<J>
where J: Iterator<Item = u8>
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.end {
            self.position += 1;
            Some(self.buffer[( self.position - 1 ) as usize])
        }else{
            let (compacted,read) : (u64,u32) =
                from_bytes(self.byte_iterator
                           .by_ref()
                           .take(self.bytes_to_take as usize));
            if read == 0 {
                None
            }else{
                let mut i :u32 = 0;
                let bits = self.encoding.bits_per_char;
                for c in encode_block(self.encoding,compacted)
                    .take(( ( read*8 + bits - 1 )/bits ) as usize)
                {
                    self.buffer[i as usize] = c;
                    i += 1;
                }
                let count = self.encoding.lcm/self.encoding.bits_per_char; //extra space
                while i < count {
                    self.buffer[i as usize] = self.encoding.padding;
                    i += 1;
                }
                self.position = 1;
                self.end = i;
                Some(self.buffer[0])
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (min, max) = self.byte_iterator.size_hint();
        let f = |x| (x*8 + self.encoding.bits_per_char as usize - 1)
            / self.encoding.bits_per_char as usize;
        (f(min), max.map(f))
    }
}

impl<J> ExactSizeIterator for Encoder<J>
where J: Iterator<Item = u8> + ExactSizeIterator{}

pub struct Decoder<J> {
    chars_to_take: u32,
    position: u32,
    end: u32,
    buffer: [u8 ; 8],
    encoding : & 'static Encoding,
    char_iterator: J 
}

impl<J> Iterator for Decoder<J>
where J: Iterator<Item = char> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position < self.end {
            self.position += 1;
            Some(self.buffer[( self.position - 1 ) as usize])
        } else {
            let (bytes,chars_read) : (u64, u32) =
                decode_block(self.encoding,
                             self.char_iterator
                             .by_ref()
                             .take(self.chars_to_take as usize));
            if chars_read == 0 {
                None
            } else {

                let drop = 8 - (self.encoding.lcm)/ 8;
                let mut i = 0;
                let takes = chars_read * self.encoding.bits_per_char / 8;
                for c in bytes.to_be_bytes().iter()
                    .skip(drop as usize)
                    .take(takes as usize)
                {
                    self.buffer[i] = *c;
                    i+=1;
                }
                self.position = 1;
                self.end =  i  as u32;
                Some(self.buffer[0])
            }
        }
    }
}

pub trait Encodable{
    type Iterator;

    fn encode(self,encoding: & 'static Encoding) -> Encoder<Self::Iterator>;
}

impl<T,K> Encodable for T
where T : IntoIterator<IntoIter = K, Item = u8>,
K : Iterator<Item = u8>
{
    type Iterator = K;
    fn encode(self,encoding: & 'static Encoding) -> Encoder<Self::Iterator> {
        Encoder{
            bytes_to_take: 8.lcm(&encoding.bits_per_char)/8,
            position: 0,
            end: 0,
            buffer: ['\0' ; 64],
            encoding,
            byte_iterator: self.into_iter()
        }
    }

}

pub trait Decodable{
    type Iterator;

    fn decode(self,encoding: & 'static Encoding) -> Decoder<Self::Iterator> ;
}

impl<T,K> Decodable for T
where T : IntoIterator<IntoIter = K, Item = char>,
K : Iterator<Item = char>
{
    type Iterator = K;
    fn decode(self,encoding: & 'static Encoding) -> Decoder<Self::Iterator> {
        Decoder{
            chars_to_take: 8.lcm(&encoding.bits_per_char)/encoding.bits_per_char,
            position: 0,
            end: 0,
            buffer: [0,0,0,0,0,0,0,0],
            encoding,
            char_iterator: self.into_iter()
        }
    }
}

//Xors each byte with the passed key. The key is repeated if the
//bytes passed are not as long as the key.
pub fn block_xor<I,J>(key: I, bytes: J) -> impl Iterator<Item = u8> 
where I : IntoIterator<Item =  u8> ,
J : IntoIterator<Item =  u8> ,
<I as IntoIterator>::IntoIter : Clone
{
    key.into_iter().cycle().zip(bytes.into_iter()).map(|(b1,b2)| b1 ^ b2)
}


/// returns the number of positive bits
fn pop_count(n: u8) -> u8
{
    let mut p: u8 = 0;
    for i in 0..8
    {
        p += (n >> i) & 1;
    }
    p
}

/// calculates the hamming distance between two bytestrings
/// truncating the longer string
pub fn hamming_distance<'a,I,J>(bytes1: I, bytes2: J) -> usize
where I : IntoIterator<Item = u8> + 'a,
J : IntoIterator<Item = u8> + 'a
{
    bytes1.into_iter().zip(bytes2.into_iter())
        .fold(0, |acc, (b1,b2)| acc + pop_count(b1^b2)) as usize
}

/// checks if character is a valid base64 character
pub fn is_base64(character: &char) -> bool
{
    in_encoding(&B64, character)
}





#[cfg(test)]
mod tests {
    use super;
    use super::*;


    const test_string : &[u8] = b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";

    const base64_result : &str = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=";

    const hex_result : &str = "4d616e2069732064697374696e677569736865642c206e6f74206f6e6c792062792068697320726561736f6e2c2062757420627920746869732073696e67756c61722070617373696f6e2066726f6d206f7468657220616e696d616c732c2077686963682069732061206c757374206f6620746865206d696e642c20746861742062792061207065727365766572616e6365206f662064656c6967687420696e2074686520636f6e74696e75656420616e6420696e6465666174696761626c652067656e65726174696f6e206f66206b6e6f776c656467652c2065786365656473207468652073686f727420766568656d656e6365206f6620616e79206361726e616c20706c6561737572652e";

    const hex : &str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d";

    const base64 : &str = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";

    #[test]
    fn hex_to_bytes_test(){
        let bytes : Vec<u8> = hex_result.chars().decode(&HEX).collect();
        assert_eq!(test_string, &bytes[..]);
    }

    #[test]
    fn base64_to_bytes_test(){
        let bytes : Vec<u8> = base64_result.chars().decode(&B64).collect();
        assert_eq!(test_string, &bytes[..]);
    }

    #[test]
    fn hex_to_base_64(){
         assert_eq!(*base64,hex.chars().decode(&HEX).encode(&B64).collect::<String>()[..]);
    }

    #[test]
    fn bytes_to_base64_test(){
        assert_eq!(*base64_result,test_string.iter().copied().encode(&B64).collect::<String>()[..])
    }

    const repeatingKeyString : &str = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal";
    const repeatingKeyResult : &str = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f";

    #[test]
    fn repeatingKeyTest(){
        assert_eq!(*repeatingKeyResult,
                    block_xor(
                        repeatingKeyString.bytes(),
                        "ICE".bytes().cycle()
                    ).encode(&HEX).collect::<String>()[..]);
    }

    #[test]
    fn hammingDistanceTest(){
        assert_eq!(37, hamming_distance("this is a test".bytes(), "wokka wokka!!!".bytes()));
    }
}
