open System

type System.String with
    member this.Lower = this.ToLower()
    member this.First = this.Lower.[0]
    member this.Last = this.Lower.[this.Length - 1] 
    member this.Successor (other: String) = this.First = other.Last

type City =
    | None
    | Used       of string
    | Existing   of string
    | NotExisted of string

let input (text: string) =
    Console.Write(text)
    Console.ReadLine()

let getCity usedCities knownCities =
    let contains (list: string list) (value: string) =
        list
        |> List.map (fun x -> x.ToLower())
        |> List.contains (value.ToLower())

    match input "> " with
    | ""   -> None
    | city -> if   usedCities  |> contains <| city then Used city
              elif knownCities |> contains <| city then Existing city
              else NotExisted city

let citiesGame = fun availableCities ->
    let rec loop = fun lastCity usedCities allCities ->

        let tryAgain() = loop lastCity usedCities allCities

        match getCity usedCities allCities with
        | Existing city ->
            if city.Successor lastCity
            then loop city (city.Lower::usedCities) allCities
            else printfn "Letters doesn't match"             ; tryAgain()
        | None            -> printfn "Empty input"           ; tryAgain()
        | Used city       -> printfn "%s is used" city       ; tryAgain() 
        | NotExisted city -> printfn "%s never existed" city ; tryAgain() 

    match availableCities with
    | [] | [_]       -> printfn "Too few cities"
    | first :: other -> loop first [first] other

[<EntryPoint>]
let main argv =
    citiesGame ["Novosibirsk"; "Kemerovo"; "Orenburg"]
    0 // return an integer exit code
