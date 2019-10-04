#[macro_use]
extern crate lazy_static;

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
                            (0..15).zip((48u8..58).chain(97u8..103).map(|x| x as char))
                            .collect());
    }
    
}
