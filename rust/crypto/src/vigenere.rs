
pub mod byte_utils;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io;
use std::io::{Error,ErrorKind};
use std::io::prelude::*;


///Loads a frequency table from a csv file
///Expects table to formatted as:
/// | a | 2.3 |
/// | b | 0.4 |
/// ...
pub fn load_frequency_csv(path: & str) -> io::Result<HashMap<char,f64>>{
    let mut f = File::open(path)?;
    let mut buffer: String = String::new();
    f.read_to_string(&mut buffer)?;
    let mut table = buffer.split('\n').filter(|s| ! (s.chars().all(|s| s.is_whitespace())) ).map(|s|{
        //collect to handle commas in quotes
        let mut vals:  Vec<&str> = s.split(',').collect();
        let mut buffer_s = String::new(); // for concatenation of comma char
        if vals.len() < 2 {
            Err(Error::new(ErrorKind::Other,format!("failed to parse csv: row not properly formatted: {}", s)))
        }else{
            let (char_s,val_s) =
                if vals.len() == 2{
                    (vals[0],vals[1])
                }else{
                    buffer_s.clear();
                    buffer_s.push_str(vals[0]);
                    buffer_s.push_str(vals[1]);
                    (&buffer_s[..],vals[2])
                };
            match (char_s[1..2].parse::<char>(), val_s.parse::<f64>()){
                (Ok(k),Ok(v)) => Ok((k,v)),
                (Err(e1),_) => Err(Error::new(ErrorKind::Other, format!( "failed to parse csv char: {} ", e1.to_string() ))),
                (_,Err(e2)) => Err(Error::new(ErrorKind::Other, format!( "failed to parse csv val: {} ", e2.to_string() )))
            }
        }
    }).try_fold(HashMap::new(),|mut hmap,pair| pair.map(|(k,v)|{
        hmap.insert(k,v);
        hmap
    }))?;
    let total = table.iter().fold(0.0,|acc,(_,v)| *v + acc);
    for v in table.values_mut(){
        *v /= total;
    }
    Ok(table)
}

/// Finds the empirical distribution of a string
pub fn frequency_from_chars< I>(chars : I) -> HashMap<char,f64>
where I: IntoIterator<Item =  char>{
    let mut frequency_table = HashMap::new();
    let mut total = 0.0;
    for c in chars.into_iter(){
        *frequency_table.entry(c).or_insert(0.0) += 1.0;
        total += 1.0;
    }
    for v in frequency_table.values_mut(){
        *v /= total;
    }
    frequency_table
}

/// Finds the empirical distribution of a byte string
pub fn frequency_from_bytes<I>(bytes : I) -> HashMap<u8,f64>
where I : IntoIterator<Item = u8>{
    let mut frequency_table = HashMap::with_capacity(256);
    let mut total = 0.0;
    for b in bytes.into_iter(){
        *frequency_table.entry(b).or_insert(0.0) += 1.0;
        total += 1.0;
    }
    for v in frequency_table.values_mut(){
        *v /= total;
    }
    frequency_table
}

///finds the lp norm between two frequency tables filling in zero
///for values not shared between them
pub fn lp_norm(a: &HashMap<char,f64>, b: &HashMap<char,f64>, p: f64)->f64{
    let key_set : HashSet<char> = a.keys().chain(b.keys()).copied().collect();
    let mut norm = 0.0;
    for c in key_set{
        norm += (( a.get(&c).unwrap_or(&0.0) - b.get(&c).unwrap_or(&0.0) ).abs()).powf(p);
    }
    norm.powf(1.0/p)
}

///Finds the best key out of the trial keys past given
///the lp norm for the given p
///Keys that do not produce valid utf-8 are given the highest possible norm
pub fn keys_ranked<'a,I,J>(frequency_table: &HashMap<char,f64>,
                    keys: I,
                    p:f64,
                    cypher_text: &[u8] ) -> Vec<(Vec<u8>,String,f64)>
where I : IntoIterator<Item = J >,
J : IntoIterator<Item = u8>,
<J as IntoIterator>::IntoIter : Clone
{
    let mut out : Vec<(Vec<u8>,String,f64)> = keys.into_iter().map(|key|{
        let key_iter = key.into_iter();
        let plain_text_result =
            String::from_utf8(byte_utils::block_xor(key_iter.clone(),
                                                    cypher_text.iter().copied()).collect());
        match plain_text_result {
            Ok(plain_text) => 
                (key_iter.collect::<Vec<u8>>(),
                 plain_text.clone(),
                 lp_norm(frequency_table,&frequency_from_chars(plain_text.chars()),p)
                ),
            Err(_) =>
                (key_iter.collect::<Vec<u8>>(),
                 String::new(),
                 f64::MAX
                )
        }
    }).collect::<>();
    out.sort_by(|(_,_,rank1),(_,_,rank2)| (*rank1).partial_cmp(rank2).unwrap_or(std::cmp::Ordering::Equal) );
    out
}

///ranks the key sizes based on the ammount of repetition in blocks of that size using the empirical entropy
///max is not inclusive
pub fn best_key_size<I>(cypher_text: I,min: usize, max:usize) -> Vec<(usize,f64)>
where I : IntoIterator<Item = u8>,
<I as IntoIterator>::IntoIter : {
    let mut iterator = cypher_text.into_iter();
    let mut outputs : Vec<(usize,f64)> = Vec::with_capacity(max-min);
    for i in min..max {
        let mut entropy = 0.0;
        for j in 0..max-min {
            let frequency = frequency_from_bytes(iterator.by_ref().skip(j).step_by(max-min));
            entropy += frequency.values().fold(0.0, |acc, p| acc +  p * p.ln())
        }
        outputs.push((i,entropy))
    }
    outputs.sort_by(|(_,rank1),(_,rank2)| (*rank1).partial_cmp(rank2).unwrap_or(std::cmp::Ordering::Equal));
    outputs
}








#[cfg(test)]
mod tests {
    use rand::prelude::*;
    use super::{byte_utils::*, *};
    
    const test_string : &[u8] = b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.";

    #[test]
    fn best_key_test(){
        let key_size : usize = 9;
        let mut rng = rand::thread_rng();
        let key : Vec<u8> = (0..key_size).map(|_| rng.gen()).collect();
        let bytes : Vec<u8> = block_xor(key,test_string.into_iter().copied()).collect();
        let key_sizes = best_key_size(bytes,4,20);
        assert_eq!(key_size,key_sizes[0].0)
    }



}
