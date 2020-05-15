#r "packages\\FSharpx.Collections\\lib\\netstandard2.0\\FSharpx.Collections.dll"

open System
open FSharpx.Collections

module OO =
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

        type Capacity = int[]
    
        type Setup =
        | CapacityAllEmpty of int array
        | CapacityInitStatus of (int * int) array

        type Status = int[]

        [<AbstractClass>]
        type Action() =
            abstract member Change: Capacity -> Status -> Status
    
        type Empty(glass: Glass) =
            inherit Action()
                member __.Glass = glass

                override __.Change _ (status: Status) =
                    newArray (glass, 0) status

                override __.ToString() =
                    "Empty #" + (glass.ToString())

                override __.Equals (obj) =
                    match obj with
                    | :? Empty as empty ->
                        (glass) = (empty.Glass)
                    | _ -> false

        type Fill(glass: Glass) =
            inherit Action()
                member __.Glass = glass

                override __.Change (capacity: Capacity) (status: Status)  =
                    newArray (glass, capacity.[glass]) status

                override __.ToString() =
                    "Fill #" + (glass.ToString())

                override __.Equals (obj) =
                    match obj with
                    | :? Fill as fill ->
                        glass = fill.Glass
                    | _ -> false

        type Pour(fromGlass: Glass, toGlass: Glass) =
            inherit Action()
                member __.FromGlass = fromGlass
                member __.ToGlass = toGlass
                                
                override __.Change (capacity: Capacity) (status: Status) =
                    let amount = min (status.[fromGlass]) (capacity.[toGlass] - status.[toGlass])
                    status
                    |> newArray (fromGlass, status.[fromGlass] - amount)
                    |> newArray (toGlass, status.[toGlass] + amount)

                override __.ToString() =
                    "Pour from #" + (fromGlass.ToString()) + " to #" + (toGlass.ToString())

                override __.Equals (obj) =
                    match obj with
                    | :? Pour as pour ->
                        (fromGlass, toGlass) = (pour.FromGlass, pour.ToGlass)
                    | _ -> false

        type Path(history: List<Action>, capacity: Capacity, endStatus: Status) =
            
            member __.EndStatus = endStatus  

            member __.History = history

            member __.Capacity = capacity

            member __.Extend(action: Action) =
                let newEndStatus = action.Change capacity endStatus
                new Path(action :: history, capacity, newEndStatus)

            override __ .GetHashCode() =
                hash (history, capacity, endStatus)

            override this.Equals (obj) =
                match obj with
                | :? Path as path ->
                    (history, capacity, endStatus) = (path.History, path.Capacity, path.EndStatus)
                | _ -> false

            interface IComparable<Path> with
                member this.CompareTo (other: Path) =
                    other.GetHashCode() - this.GetHashCode()

            interface IComparable with
                override this.CompareTo other =
                    (this :> IComparable<Path>).CompareTo (other :?> Path)

            member __.Print(initStatus: Status) =
                let init = initStatus
                let history = history |> List.rev
                printfn "Initial Status: %A" init
                printfn "Final Status: %A" endStatus
                printfn "Number of actions: %i" (List.length history)
                printfn "\n*** HISTORY ***"

                let stringAction (action: Action) =
                    match action with
                    | :? Fill as fill -> (fill.ToString())
                    | :? Empty as empty -> (empty.ToString())
                    | :? Pour as pour -> (pour.ToString())
                    | _ -> raise <| System.SystemException()


                let rec print (actions: List<Action>) (status: Status) =
                    match actions with
                    | [] -> printfn "DONE!\n\n"
                    | h :: t ->
                        let newStatus = h.Change capacity status
                        printfn "%A  <=== %s" newStatus (stringAction h)
                        print t newStatus

                print history init
            
            

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
                
            // Define the range of glasses
            let range = [|0 .. (Array.length capacity - 1) |]
        
            // List of potential actions
            let actions : seq<Action> =
                seq {
                    for g in range do yield Fill(g)               
                    for g in range do yield Empty(g)
                    for g1 in range do
                        for g2 in range do
                            if g1 <> g2 then
                                yield Pour(g1, g2) }

            // Build a LazyList of Set of Paths. The point is to create paths that are unique
            // to avoid circular paths
            let rec build (paths: Set<Path>) (explored : Set<Status>) : LazyList<Set<Path>> =
                if (Set.isEmpty paths) then LazyList.empty<Set<Path>>
                else
                    let more = 
                        seq {
                            for path in paths do
                                for action in actions do
                                    let next = path.Extend action
                                    if not (Set.contains (next.EndStatus) explored) then
                                        yield next
                        } |> Set.ofSeq
                    let newExplored = Set.map (fun (x: Path) -> x.EndStatus) more
                    LazyList.cons paths (build more (newExplored + explored))

            let initPath = new Path([], capacity, initStatus)

            let solve (target: int) =
                let pathSets =
                    build (Set.ofList [initPath]) (Set.ofList [initPath.EndStatus])

                seq {
                    for pathSet in pathSets do
                        for path in pathSet do
                            if (Array.contains target path.EndStatus) then
                                yield path
                }


            let printSolve(paths: seq<Path>) =
                paths
                |> Seq.toList
                |> List.groupBy (fun path -> List.length path.History)
                |> List.sortBy fst
                |> function
                    | [] -> printfn "NO SOLUTION!"
                    | list ->
                        list
                        |> List.head
                        |> snd
                        |> List.iter (fun p -> (p.Print(initStatus)))

            member __.Solve(target: int) = 
                solve target

            member __.SolveAndPrint(target: int) =
                target
                |> solve
                |> printSolve

            member __.Actions = actions



let pb1 = OO.Solver.CapacityAllEmpty [|4;3 ; 7|]
let pouring1 = OO.Solver.PouringPb(pb1)
pouring1.SolveAndPrint(5)
pouring1.Solve 5
|>Seq.toList

pouring1.Actions
|> Seq.toList

let actions1 : List<OO.Solver.Action>= [OO.Solver.Pour(1,0)]
let actions2 : List<OO.Solver.Action>= [OO.Solver.Pour(1,0)]
let status1 = [|0;0|]
let status2 = [|0;1|]
let status3 = [|0;0|]
let capacity1 = [|3 ; 7|]
let capacity2 = [|3 ; 7|]

let path1 = new OO.Solver.Path(actions1, capacity1, status1)
let path2 = new OO.Solver.Path(actions2, capacity2, status2)

let set1 = Set.singleton path1
let set2 = set1.Add path2
set2.Count
actions1 = actions2
path1.Equals(path2)


