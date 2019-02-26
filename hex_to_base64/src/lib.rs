
pub mod byte_utils{
    use std::io::Error;
    use std::io::ErrorKind;
    pub fn hex_to_bytes(str: &String) -> Result<Vec<u8>,Error>{
       let mut vec: Vec<u8> = Vec::new();
        for i in (0..str.len()/2).map(|x| 2 * x) {
            vec.push(u8::from_str_radix(&str[i..i+2],16)
                    .expect("malformed hex string")
            );
        }
        if str.len() % 2 > 0 {
                Err(Error::new(ErrorKind::Other, "malformed hex string"))
        }else{
                Ok(vec)
        }
    }

    pub fn bytes_to_base64(vec: &Vec<u8>) -> String{
        let mut output = String::new();
        for i in (0..vec.len()/3).map(|x| x * 3) {
            let mut num = u32::from_le_bytes(dbg!([vec[i+2],vec[i+1],vec[i],0]));
            let mut temp = String::new();
            for _ in 0..4 {
                let mask = 0o000000077;
                temp.push(num_to_base64_char((num & mask) as u8));
                num = num >> 6;
            }
            temp = temp.chars().rev().collect();
            output.push_str(temp.as_str());
        }
        output
    }
 
    pub fn num_to_base64_char(x: u8) -> char{
        if x < 26 {
            (x + 65) as char
        }else if x < 52{
            (x - 26 + 97) as char
        }else if x < 62{
            (x - 52 + 48) as char
        }else if x == 62 {
            '+'
        }else{
            '/'
        }
    }

    pub fn bytes_to_hex<'a>(vec: &'a Vec<u8>) -> String{
        vec.iter()
            .map(|x| format!("{:0x}",x))
            .fold(String::new(), |acc, x| acc + &x )
    }
}
