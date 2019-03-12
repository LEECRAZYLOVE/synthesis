module Synthesis

open System

let abelar a = a > 12 && a < 3097 && a % 12 = 0
failwith "Not implemented"

let area Base height = match Base >= 0.0 && height >= 0.0 with
   | true -> 1.0/2.0 * Base * height
   | false -> failwith "Not implemented"

let zollo input = match input >= 0 with
   | true -> input * 2
   | false -> input*(-1)
failwith "Not implemented"

let min a b = match a < b with
   | true -> a
   | false -> b
failwith "Not implemented"

let max a b = match a < b with
   | true -> b
   | false -> a
failwith "Not implemented"

let ofTime hour min sec = hour * 3600 + min * 60 + sec
failwith "Not implemented"

let toTime input = 
    let hours = input / 3600
    let min = (input / 60) % 60
    let sec = (input - (hours * 3600)) % 60
    match input > 0 with
    | true -> (hours, min, sec) 
    | false -> (0,0,0)
failwith "Not implemented"

let digits d =
    let rec countDigits num acc =
        match num/10 = 0 with
        | true -> acc
        | _ -> countDigits(num/10) (acc + 1)
    countDigits d 1
failwith "Not implemented"

let minmax (a,b,c,d) = 
    min (min a b) (min c d), max (max a b) (max c d) 
failwith "Not implemented"

let isLeap year = 
    match year >= 1582 with
        | false -> failwith "shouldFail"
        | true -> (year%4 = 0 && year%100 <> 0) || (year%400 = 0)            

let month a =   match a < 1 || a > 12 with
    | true -> failwith "Not implemented"
    | false -> match a with
        | 1 -> ("January", 31)
        | 2 -> ("February", 28)
        | 3 -> ("March", 31)
        | 4 -> ("April", 30)
        | 5 -> ("May", 31)
        | 6 -> ("June", 30)
        | 7 -> ("July", 31)
        | 8 -> ("August", 31)
        | 9 -> ("September", 30)
        | 10 -> ("October", 31)
        | 11 -> ("November", 30)
        | 12 -> ("December", 31)
 
let toBinary a = match a < 0 with
    | true -> failwith "Not implemented"
    | _ -> match a = 0 with
        | true -> "0"
        | false -> 
            let rec inside b acc = 
                match b = 0 && acc <> "" with
                | true -> acc
                | false -> match b%2 = 0 with
                    | true -> inside (b/2) ("0" + acc) 
                    | false -> inside (b/2) ("1" + acc)
            inside a ""
   

let bizFuzz a = 
    let rec times (b,c,d) acc = 
        match a >= acc && a <> 1 with 
        | false -> b,c,d
        | true -> match acc%3 = 0 && acc/3 > 0,acc%5 = 0 && acc/5 > 0,(acc%3 = 0 && acc%5 = 0 && acc/3 > 0 && acc/5 > 0) with
            | true,true,true -> times (b+1,c+1,d+1) (acc+1)
            | true,false,true -> times (b+1,c,d+1) (acc+1)
            | true, true,false -> times (b+1,c+1,d) (acc+1)
            | true,false,false -> times (b+1,c,d) (acc+1)
            | false,true,true -> times (b,c+1,d+1) (acc+1)
            | false,false,true -> times (b,c,d+1) (acc+1)
            | false,false,false -> times (b,c,d) (acc+1)
            | false,true,false -> times (b,c+1,d) (acc+1)
    times (0,0,0) 0
failwith "Not implemented"

let monthDay d year = 
    let rec get a b acc =
        let name, days = month acc
        match a - b < b with
        | true -> month acc
        | false -> match acc,isLeap year with
            | 2,false | _,false -> get (a - days) days (acc + 1) 
            | 2,true -> get (a - (days+1)) days (acc + 1)
    get d  0


let coord _ =
    failwith "Not impl
    emented"