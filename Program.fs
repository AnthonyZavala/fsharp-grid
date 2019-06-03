open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Coordinates =
    { X: int 
      Y: int }          

module Utils =
    let boolGrid grid = grid |> Array2D.map (fun x -> match x with 1 -> true | _ -> false)

module Iterative =

    let isContainedCell coordinates islandCell =
        let xRange = [| (coordinates.X - 1) .. (coordinates.X + 1) |]
        let yRange = [| (coordinates.Y - 1) .. (coordinates.Y + 1) |]
        xRange |> Array.contains islandCell.X 
            && yRange |> Array.contains islandCell.Y
            && coordinates <> islandCell
    
    let getIslands (grid: bool[,]) =
        let mutable islands = []
        for x = 0 to (grid |> Array2D.length1) - 1 do
            for y = 0 to (grid |> Array2D.length2) - 1 do
                let coordinates = { X = x; Y = y }
                if grid.[x, y] then 
                    let appendedIsland = coordinates :: (islands |> List.fold (fun newIsland island -> if (island |> List.exists (isContainedCell coordinates)) then newIsland @ island else newIsland) [])
                    let otherIslands = islands |> List.filter (fun island -> not (island |> List.exists (isContainedCell coordinates)))
                    islands <- appendedIsland :: otherIslands
        islands

    let getIslandCount (grid: bool[,]) =
        grid |> getIslands |> List.length


module Recursive =

    let getNewSurroundingCoordinates grid startingCoordinates currentIsland = 
        let minX = max 0 (startingCoordinates.X - 1)
        let maxX = min ((grid |> Array2D.length1) - 1) (startingCoordinates.X + 1)
        let minY = max 0 (startingCoordinates.Y - 1)
        let maxY = min ((grid |> Array2D.length2) - 1)  (startingCoordinates.Y + 1)
        seq { for x in minX .. maxX do
                for y in minY .. maxY do
                    let coordinates = { X = x; Y = y }
                    if grid.[x, y] && not (currentIsland |> Seq.contains coordinates) then yield coordinates
            }

    let rec buildIsland grid coordinates currentIsland =
        let newSurroundingLandCoordinates = currentIsland |> getNewSurroundingCoordinates grid coordinates
        currentIsland |> Seq.append (seq { for x in newSurroundingLandCoordinates do yield! buildIsland grid x currentIsland })
        

    //let unionIsland existingIslands grid coordinates =
        
        

    let getIslands (grid: bool[,]) =
        let mutable islands = Seq.empty
        grid |> Array2D.iteri(fun x y isLand-> if isLand then islands <- buildIsland grid { X = x; Y = y } Seq.empty)
        islands

    let getIslandCount (grid: bool[,]) =
        grid |> getIslands |> Seq.length

open Utils
open Iterative
open Recursive

type GridCountBenchmark () =
    let mutable emtpyGrid = array2D []
    let mutable blackoutGrid = array2D []
    let mutable grid = array2D [ [ 1; 0; 1; 0; 0; 1; 0; 0; ]
                                 [ 0; 1; 0; 0; 1; 1; 1; 0; ]
                                 [ 1; 0; 1; 0; 0; 1; 0; 0; ]
                                 [ 0; 0; 0; 0; 0; 0; 0; 0; ]
                                 [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                                 [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                                 [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                                 [ 1; 1; 1; 1; 1; 1; 1; 1; ] ]

    [<Params (1, 10, 100)>] 
    member val public GridSize = 0 with get, set

    [<GlobalSetup>]
    member self.GlobalSetupData() =
        emtpyGrid <- Array2D.create self.GridSize self.GridSize false
        blackoutGrid <- Array2D.create self.GridSize self.GridSize true
        
    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Iterative")>]
    member self.IterativeEmpty () = emtpyGrid |> getIslandCount
    [<Benchmark>]
    [<BenchmarkCategory("Iterative")>]
    member self.IterativeBlackout () = blackoutGrid |> getIslandCount

[<EntryPoint>]
let main argv =    
    //let summary = BenchmarkRunner.Run typeof<GridCountBenchmark>
    let grid = array2D [ [ 1; 0; 1; 0; 0; 1; 0; 0; ]
                         [ 0; 1; 0; 0; 1; 1; 1; 0; ]
                         [ 1; 0; 1; 0; 0; 1; 0; 0; ]
                         [ 0; 0; 0; 0; 0; 0; 0; 0; ]
                         [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                         [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                         [ 1; 1; 1; 1; 1; 1; 1; 1; ]
                         [ 1; 1; 1; 1; 1; 1; 1; 1; ] ]

    grid |> boolGrid |> getIslandCount |> printfn "%i"

    0 // return an integer exit code
