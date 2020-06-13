module BinUtilsTest (binUtilsTest) where


import qualified Data.ByteString as B
import Data.String
import Test.HUnit
import Break.Utils
import Data.Word
import Data.List
import Control.Arrow

testString = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

testByteString :: B.ByteString
testByteString = fromString testString

base64Result = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="
hexResult = "4d616e2069732064697374696e677569736865642c206e6f74206f6e6c792062792068697320726561736f6e2c2062757420627920746869732073696e67756c61722070617373696f6e2066726f6d206f7468657220616e696d616c732c2077686963682069732061206c757374206f6620746865206d696e642c20746861742062792061207065727365766572616e6365206f662064656c6967687420696e2074686520636f6e74696e75656420616e6420696e6465666174696761626c652067656e65726174696f6e206f66206b6e6f776c656467652c2065786365656473207468652073686f727420766568656d656e6365206f6620616e79206361726e616c20706c6561737572652e"

hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
base64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
hexToBase64Test :: Test
hexToBase64Test = "Hex To Base 64" ~: TestCase (
                     assertEqual ("for: toBase64 $ hexToBytes "++hex)
                                 base64
                                 (toBase64 $ hexToBytes hex))

hexToBytesTest :: Test
hexToBytesTest =  "Hex To Bytes" ~: testByteString ~=? hexToBytes hexResult

bytesToHexTest :: Test
bytesToHexTest = "Bytes To Hex" ~: hexResult ~=? toHex testByteString



bytesToBase64Test :: Test
bytesToBase64Test = "Bytes To Base64" ~: base64Result ~=? toBase64 testByteString

repeatingKeyString = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
repeatingKeyResult = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
repeatingKeyTest = repeatingKeyResult ~=? toHex (B.pack $ (fromAscii "ICE") `blockXor`
                                                  (fromAscii repeatingKeyString) )

hammingDistanceTest :: Test
hammingDistanceTest = "Hamming Distance" ~: 37 ~=? hammingDist (B.unpack $ fromString "this is a test")
                                                               (B.unpack $ fromString "wokka wokka!!!")

toAsciiTest :: Test
toAsciiTest = "Ascii Test" ~: TestList ["toAscii" ~: testString ~=? toAscii (B.unpack testByteString),
                                        "fromAscii" ~: (B.unpack testByteString) ~=? fromAscii testString]

base64toBytesTest :: Test
base64toBytesTest = "Base64 to bytes" ~:  testByteString ~=? base64ToBytes base64Result

padTest :: Test
padTest = "Pad test" ~: "YELLOW SUBMARINE\x04\x04\x04\x04" ~=?
                         toAscii (pad 20 $ B.unpack $ fromString "YELLOW SUBMARINE" )

binUtilsTest :: Test
binUtilsTest = TestList [bytesToHexTest,
                         bytesToBase64Test,
                         hexToBytesTest,
                         hexToBase64Test,
                         repeatingKeyTest,
                         hammingDistanceTest,
                         base64toBytesTest,
                         padTest]
