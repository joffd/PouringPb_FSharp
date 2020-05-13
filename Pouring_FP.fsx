#r "packages\\FSharpx.Collections\\lib\\netstandard2.0\\FSharpx.Collections.dll"

open System
open FSharpx.Collections

module Helper =
    let newList (index, value) (list: List<'T>) =
        list
        |> List.mapi (fun i v -> if i = index then value else v)

    let newArray (index, value) array =
        array
        |> Array.mapi (fun i v -> if i = index then value else v)    

module Solver =
    
    open Helper

    type Glass = int
    
    type Setup =
    | CapacityAllEmpty of int array
    | CapacityInitStatus of (int * int) array

    type Capacity = int array
    
    type Status =  int array

    type Problem = {
        Setup: Setup
        Target: int
    }

    type Action =
    | Fill of Glass
    | Empty of Glass
    | Pour of Glass * Glass

    type InternalData = {
        Pb: Problem
        Capacity: int[]
        InitStatus: int[]
        Actions: List<Action>
    }

    type Path = {
        History: List<Action>
        EndStatus: Status
    }

    type Paths = Set<Path>


    let nbGlasses pb = 
        match (pb.Setup) with
        | CapacityAllEmpty arr -> Array.length arr
        | CapacityInitStatus arr -> Array.length arr

    let capacity (pb: Problem) =
        match pb.Setup with
        | CapacityAllEmpty arr -> arr
        | CapacityInitStatus arr -> (Array.unzip >> fst) arr

    let initStatus (pb: Problem) =
        match pb.Setup with
        | CapacityAllEmpty arr -> arr |> Array.map (fun _ -> 0)
        | CapacityInitStatus arr -> (Array.unzip >> snd) arr

    let change (data: InternalData) (status: Status) (action: Action) : Status =
        match action with
        | Fill(g) -> newArray (g, data.Capacity.[g]) status
        | Empty(g) -> newArray (g, 0) status
        | Pour(g1, g2) ->
            let amount = min (status.[g1]) (data.Capacity.[g2] - status.[g2])
            status
            |> newArray (g1, status.[g1] - amount)
            |> newArray (g2, status.[g2] + amount)


    let createActions (range: int[]) =   
        seq {
        for g in range do yield Action.Fill(g)               
        for g in range do yield Action.Empty(g)
        for g1 in range do
            for g2 in range do
                if g1 <> g2 then
                    yield Action.Pour(g1, g2) }  



    let pbToInternalPb (pb: Problem) =
        let range =  [|0..(nbGlasses pb - 1)|]
        {
            Pb = pb
            Capacity = capacity pb
            InitStatus = initStatus pb
            Actions = (createActions range) |> Seq.toList
        }

    let extendPath (action: Action) (history: List<Action>) =
        action :: history

    let addAction (data: InternalData) (action: Action) (path: Path) =
        {
            History = action :: path.History
            EndStatus = change data path.EndStatus action
        }

    let rec build (data: InternalData) (paths: Set<Path>) (explored : Set<Status>) : LazyList<Set<Path>> =
        if (Set.isEmpty paths) then LazyList.empty<Set<Path>>
        else
            let more = 
                seq {
                    for path in paths do
                        for action in data.Actions do
                            let next = addAction data action path
                            if not (explored |> Set.contains next.EndStatus) then
                                yield next
                } |> Set.ofSeq
            LazyList.cons paths (build data more (Set.union (Set.map (fun x -> x.EndStatus) more) explored))


    let solve (pb: Problem) =
        let data = pbToInternalPb pb
        let initPath = 
            {
                History = []
                EndStatus = data.InitStatus
            }
        let pathsets =
            build data (Set.ofList [initPath]) (Set.ofList [initPath.EndStatus])
        seq {
             for pathset in pathsets do
                 for path in pathset do
                     if (Array.contains pb.Target path.EndStatus) then
                         yield path
        }

module Printer =
    
    open Solver

    let printer (pb: Problem) (path: Path) =
        let init = initStatus pb
        let history = path.History |> List.rev
        let internalPb = pbToInternalPb pb
        printfn "Initial Status: %A" init
        printfn "Final Status: %A" path.EndStatus
        printfn "Number of actions: %i" (List.length history)
        printfn "\n*** HISTORY ***"

        let stringAction (action: Action) =
            match action with
            | Fill g -> "Fill #" + (g.ToString())
            | Empty g -> "Empty #" + (g.ToString())
            | Pour (f, t) -> "Pour from #" + (f.ToString()) + " to #" + (t.ToString())


        let rec print (actions: List<Action>) (status: Status) =
            match actions with
            | [] -> printfn "DONE!\n\n"
            | h :: t ->
                let newStatus = change internalPb status h
                printfn "%A  <=== %s" newStatus (stringAction h)
                print t newStatus

        print history init

    let solvePrint (pb: Problem) =
        solve pb
        |> Seq.toList
        |> List.groupBy (fun path -> List.length path.History)
        |> function
            | [] -> printfn "NO SOLUTION!"
            | list ->
                list
                |> List.head
                |> snd
                |> List.iter (printer pb)
      
// TEST
        
open Solver
open Printer

let pb1 = {
    Setup = CapacityInitStatus [|(7, 2) ; (9, 0) ; (5, 0)|]
    Target = 8
}

solvePrint pb1

let pb2 = {
    Setup = CapacityAllEmpty [|3 ; 7|]
    Target = 8
}

solvePrint pb2


let pb3 = {
    Setup = CapacityAllEmpty [|3 ; 7|]
    Target = 5
}

solvePrint pb3



let ipb = Model.pbToInternalPb pb

let action1 = Fill(1)
let action2 = Pour(1, 0)
let status1 = change ipb ipb.InitStatus action1
change ipb status1 action2

Model.createActions [|0 .. pb.Capacity.Length|]
|> Seq.toList