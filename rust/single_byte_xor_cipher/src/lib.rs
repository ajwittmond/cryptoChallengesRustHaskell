

pub mod freq{
    //creates a byte histogram
    pub fn histogram<I>(text: I) -> [u32; 256]
    where I: IntoIterator<Item = u8>{
        let mut out = [0; 256];
        for c in text.into_iter(){
            out[c as usize] += 1;
        }
        out
    }


    //frequency of the occurrence of English letters as percentages
    pub const ENGLISH_LETTER_FREQUENCY :[f32; 26] =
        [0.37025,6.7875e-2,0.123575,0.19685,
         0.5478,0.105,9.2325e-2,0.269875,0.33295,
         4.7e-3,3.1425e-2,0.181325,0.119025,0.31665,
         0.350075,8.29e-2,5.125e-3,0.274425,0.28625,
         0.414675,0.13115,5.0475e-2,9.5475e-2,7.875e-3,
         9.6325e-2,3.2e-3];

    pub fn lp_norm<I,J,K>(x: I , y: I, p: K) -> K
    where I : Iterator<Item = K>,
          K : num::Float
    {
        x.zip(y).fold(num::Zero::zero(),|acc : K, (x,y)| acc + (x-y).powf(p)).powf(p.recip())
    }
}
