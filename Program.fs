open System

type Valid(content: string) =
    member __.Str = content.ToLower()
    member this.First = this.Str.[0]
    member this.Last = this.Str.[content.Length - 1] 
    member this.Successor (other: Valid) = this.First = other.Last

type City =
    | None
    | Used       of string
    | Existing   of Valid
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
              elif knownCities |> contains <| city then Existing <| Valid city
              else NotExisted city

let citiesGame = fun availableCities ->
    let rec loop = fun (lastCity: Valid) usedCities allCities ->

        let nextTurn() = loop lastCity usedCities allCities

        match getCity usedCities allCities with
        | Existing city ->
            if city.Successor lastCity
            then loop city (city.Str::usedCities) allCities
            else printfn "Letters doesn't match"             ; nextTurn()
        | None            -> printfn "Empty input"           ; nextTurn()
        | Used city       -> printfn "%s city is used"  city ; nextTurn() 
        | NotExisted city -> printfn "%s never existed" city ; nextTurn() 

    match availableCities with
    | [] | [_]       -> printfn "Empty cities list"
    | first :: other -> loop (Valid first) [first] other

[<EntryPoint>]
let main argv =
    citiesGame ["Novosibirsk"; "Kemerovo"; "Orenburg"]
    0 // return an integer exit code
