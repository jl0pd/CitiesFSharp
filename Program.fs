open System

type System.String with
    member this.Lower = this.ToLower()
    member this.Title = (Char.ToUpper this.[0]).ToString() + this.Substring(1)
    member this.First = this.Lower.[0]
    member this.Last = this.Lower.[this.Length - 1] 
    member this.Successor (other: String) = this.First = other.Last

type City =
    | None
    | Used       of string
    | Existing   of string
    | NotExisted of string

let str = function
    | None -> ""
    | Used city -> city
    | Existing city -> city
    | NotExisted city -> city

type CitiesList = string list

type Order =
    | Player
    | Computer
    | Ended

type GameState = {
    Move: Order;
    UsedCities: CitiesList;
    KnownCities: CitiesList;
    LastCity: City }

let initState = function
    | [] | [_] ->
        printfn "Too few cities"
        { Move = Ended
          UsedCities = []
          KnownCities = []
          LastCity = None }
    | (first: string) :: other ->
        printfn "%s" first.Title
        { Move = Player
          UsedCities = [ first.Lower ]
          KnownCities = List.map (fun (x:string) -> x.Lower) other
          LastCity = Existing first.Lower }
    
let playerInput (text: string) =
    Console.Write(text)
    Console.ReadLine()

let getCity usedCities knownCities =
    let contains (list: string list) (value: string) =
        List.contains (value.ToLower()) list

    match playerInput "> " with
    | ""   -> None
    | city -> if   usedCities  |> contains <| city then Used city
              elif knownCities |> contains <| city then Existing city
              else NotExisted city

let rec playerMove (state: GameState) :GameState =
    let usedCities = state.UsedCities
    let allCities = state.KnownCities
    let lastCity = state.LastCity

    let tryAgain() = playerMove state
    match getCity usedCities allCities with
        | Existing city ->
            if city.Successor <| str lastCity
            then { state with
                    Move=Computer
                    LastCity=Existing city
                    UsedCities=(city::usedCities)
                    KnownCities=List.filter ((<>)city) allCities }
            else printfn "Letters doesn't match"             ; tryAgain()
        | None            -> printfn "Empty input"           ; tryAgain()
        | Used city       -> printfn "%s is used" city       ; tryAgain() 
        | NotExisted city -> printfn "%s never existed" city ; tryAgain() 

let computerMove (state: GameState) :GameState =
    let usedCities = state.UsedCities
    let allCities = state.KnownCities
    let lastCity = state.LastCity
    
    match List.tryFind (fun (x: string) -> x.StartsWith((str lastCity).Last)) allCities with
    | Some city ->
        printfn "%s" city.Title
        { state with
            Move = Player
            LastCity = Existing city
            UsedCities = city::usedCities
            KnownCities = List.filter ((<>)city) allCities }
    | option.None ->
        printfn "No citites left for this letter"
        { state with Move=Ended }


let citiesGame = fun (availableCities: string list) ->
    let rec loop = fun (state: GameState) ->
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
