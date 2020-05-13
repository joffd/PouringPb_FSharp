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

    type Status = int[]

    type Action =
        abstract member Change: Status -> Status
    
    type Empty(glass: Glass) =
        interface Action with
            member __.Change(status: Status) =
                newArray (glass, 0) status

    type Fill(glass: Glass, capacity: int) =
        interface Action with
            member __.Change(status: Status) =
                newArray (glass, capacity) status

    type Pour(fromGlass: Glass, toGlass: Glass, capacityTo: int) =
        interface Action with
            member __.Change(status: Status) =
                let amount = min (status.[fromGlass]) (capacityTo - status.[toGlass])
                status
                |> newArray (toGlass, status.[toGlass] - amount)
                |> newArray (toGlass, status.[toGlass] + amount)

    type PouringPb(setup: Setup) =
        // Capacity
        let capacity = 
            match setup with
            | CapacityAllEmpty arr -> 
                arr
            | CapacityInitStatus arr ->
                (Array.unzip >> fst) arr
        
        // Initial Status
        let initStatus =
            match setup with
            | CapacityAllEmpty arr -> 
                arr
                |> Array.map (fun _ -> 0)
            | CapacityInitStatus arr ->
                (Array.unzip >> snd) arr
                

        let range = [|0 .. (Array.length capacity - 1) |]
        
        let actions : seq<Action> =
            seq {
                for g in range do yield Fill(g, capacity.[g])               
                for g in range do yield Empty(g)
                for g1 in range do
                    for g2 in range do
                        if g1 <> g2 then
                            yield Pour(g1, g2, capacity.[g2]) }  


        member __.Capacity = capacity
        member __.InitStatus = initStatus

open Solver

let pb1 = CapacityAllEmpty [|3 ; 7|]
let pouring1 = PouringPb(pb1)

