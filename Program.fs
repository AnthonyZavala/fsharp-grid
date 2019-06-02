open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Coordinates =
    { X: int 
      Y: int }          

module Utils =
    let boolGrid grid = grid |> Array2D.map (fun x -> match x with 1 -> true | _ -> false)

    let getSurroundingGrid (startingCoordinates: Coordinates) (grid: bool[,]) = 
        let minX = max 0 (startingCoordinates.X - 1)
        let maxX = min ((grid |> Array2D.length1) - 1) (startingCoordinates.X + 1)
        let minY = max 0 (startingCoordinates.Y - 1)
        let maxY = min ((grid |> Array2D.length2) - 1)  (startingCoordinates.Y + 1)
        grid.[minX..maxX, minY..maxY]

module Iterative =
    let isContainedCell coordinates islandCell =
        let xRange = [| (coordinates.X - 1) .. (coordinates.X + 1) |]
        let yRange = [| (coordinates.Y - 1) .. (coordinates.Y + 1) |]
        if xRange |> Array.contains islandCell.X 
            && yRange |> Array.contains islandCell.Y
            && coordinates <> islandCell
        then true
        else false
    
    let getIslandCount (grid: bool[,]) =
        let mutable islands = []
        for x = 0 to (grid |> Array2D.length1) - 1 do
            for y = 0 to (grid |> Array2D.length2) - 1 do
                let coordinates = { X = x; Y = y }
                if grid.[x, y] then 
                    let appendedIsland = coordinates :: (islands |> List.fold (fun newIsland island -> if (island |> List.exists (isContainedCell coordinates)) then newIsland @ island else newIsland) [])
                    let otherIslands = islands |> List.filter (fun island -> not (island |> List.exists (isContainedCell coordinates)))
                    islands <- appendedIsland :: otherIslands
        islands.Length

module Tree =
    type BinaryTree =
        { Coordinates: Coordinates
          Left: BinaryTree option
          Right: BinaryTree option }

    let rec coordinatesExist coordinates binaryTree =
        let coordinatesExist = coordinatesExist coordinates
        if binaryTree.Coordinates = coordinates then true
        else (binaryTree.Left.IsSome && binaryTree.Left.Value |> coordinatesExist)
            || (binaryTree.Right.IsSome && binaryTree.Right.Value |> coordinatesExist)

    let getIslandCount (grid: bool[,]) =
        let root = { Coordinates = { X = 0; Y = 0 }; Left = None; Right = None }
        0

open Utils
open Iterative

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

    [<Params (1, 10, 100, 1000, 10000)>] 
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
    let summary = BenchmarkRunner.Run typeof<GridCountBenchmark>
    0 // return an integer exit code
