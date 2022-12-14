module BM25Tests.Utils

open System


type Doc = {
  Path: string
  Content: string
}

let getAllDocuments = 
  System.IO.Directory.GetFiles
  >> Array.map (fun e -> {
      Path=e
      Content = System.IO.File.ReadAllText e
    })
  >> Array.toList

// helper function to set the console collor and automatically set it back when disposed
let consoleColor (fc : ConsoleColor) = 
    let current = Console.ForegroundColor
    Console.ForegroundColor <- fc
    { new IDisposable with
          member x.Dispose() = Console.ForegroundColor <- current }

// printf statements that allow user to specify output color
let cprintf color str = Printf.kprintf (fun s -> use c = consoleColor color in printf "%s" s) str
let cprintfn color str = Printf.kprintf (fun s -> use c = consoleColor color in printfn "%s" s) str