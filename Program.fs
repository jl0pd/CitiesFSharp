open System

type City =
    | None
    | Used        of string
    | Existing    of string
    | NotExisting of string

let lowerCase (str: string) =
    str.ToLower()

let titleCase (str: string) =
    (Char.ToUpper str.[0]).ToString() + str.Substring(1)

let firstLetter (str: string) =
    str.[0]

let lastLetter (str: string) =
    str.[str.Length - 1]

let startsWith (letter: char) (str: string) =
    str.StartsWith letter

let successor s1 s2 =
    match s1, s2 with
    | city, lastCity when firstLetter city = lastLetter lastCity -> true
    | _ -> false

type CitiesList = string list

type Order =
    | Player
    | Computer
    | Ended

type GameState = {
    Move: Order
    UsedCities: CitiesList
    KnownCities: CitiesList
    LastCity: string }

let initState = function
    | [] | [_] ->
        printfn "Too few cities"
        { Move = Ended
          UsedCities = []
          KnownCities = []
          LastCity = "" }
    | (first: string) :: other ->
        titleCase first |> printfn "%s"
        { Move = Player
          UsedCities = [ lowerCase first ]
          KnownCities = List.map lowerCase other
          LastCity = lowerCase first }
    
let playerInput (text: string) =
    Console.Write(text)
    lowerCase <| Console.ReadLine()

let getCity usedCities knownCities =
    let contains list value =
        list |> List.contains value

    match playerInput "> " with
    | ""   -> None
    | city -> if   usedCities  |> contains <| city then Used city
              elif knownCities |> contains <| city then Existing <| city
              else NotExisting city

let useCity city state =
    let { UsedCities = usedCities
          KnownCities = allCities } = state
    { state with
            LastCity = city
            UsedCities = city::usedCities
            KnownCities = List.filter ((<>)city) allCities }

let rec playerMove state =
    let { LastCity = lastCity
          UsedCities = usedCities  
          KnownCities = allCities } = state
    
    let tryAgain() = playerMove state
    match getCity usedCities allCities with
        | Existing city ->
            if successor city lastCity
            then { useCity city state with Move = Computer }
            else printfn "Letters doesn't match"             ; tryAgain()
        | None             -> printfn "Empty input"          ; tryAgain()
        | Used city        -> printfn "%s is used" city      ; tryAgain() 
        | NotExisting city -> printfn "%s not existing" city ; tryAgain() 

let computerMove state =
    let { KnownCities = allCities
          LastCity = lastCity
        } = state

    match List.tryFind (startsWith <| lastLetter lastCity) allCities with
    | Some city ->
        titleCase city |> printfn "%s"
        { useCity city state with Move = Player }
    | option.None ->
        printfn "No citites left for this letter"
        { state with Move = Ended }

let citiesGame = fun availableCities ->
    let rec loop = fun state ->
        match state with
        | { Move = Player } -> 
            loop <| playerMove state
        | { Move = Computer } ->
            loop <| computerMove state
        | { Move = Ended } ->
            ()
    initState availableCities |> loop

[<EntryPoint>]
let main argv =
    System.IO.File.ReadAllLines "cities.txt"
    |> List.ofArray
    |> citiesGame
    0 // return an integer exit code
