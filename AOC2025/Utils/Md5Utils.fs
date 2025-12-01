module Md5Utils

open System
open System.Security.Cryptography
open System.Text

let md5HashInHex (inputString: string) : string =
    use md5 = MD5.Create()
    let inputBytes = Encoding.UTF8.GetBytes(inputString)
    let hashBytes = md5.ComputeHash(inputBytes)
    Convert.ToHexString(hashBytes)