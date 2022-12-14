module BM25Tests.Program
open BM25Tests.Search
open BM25Tests.Utils
open BM25.LucenePlus

type TestCase = { 
  Query: string 
  Expect: string
}

type TestGroup = {
  Cases: TestCase list 
  Data: string
}

module SearchResult = 
  let toReadable (x:SearchResult) = $"{x.Score}: {x.Path}"


module TestCase = 
  let toReadable (x:TestCase) = $"Query: {x.Query}\nExpected: {x.Expect}"


let printSad = cprintfn System.ConsoleColor.Red
let printHappy = cprintfn System.ConsoleColor.Green

let runTest (tests: TestGroup) = 
  let doc = getAllDocuments tests.Data
  let similarity = LuceneSimilarities.BM25PlusSimilarity()
  let directory = index similarity doc
  
  let mutable sucessCount = 0
  for test in tests.Cases do
    let result = search similarity directory 10 test.Query 
    let pass = 
      result 
      |> Seq.tryHead 
      |> Option.map (fun x -> x.Path.Contains(test.Expect)) |> Option.defaultValue false
    
    test |> TestCase.toReadable |> printfn "%s"
    
    if pass then printHappy "Test passed" else printSad "Test failed"
    
    result
    |> Seq.map SearchResult.toReadable
    |> String.concat "\n"
    |> printfn "%s"
    
    sucessCount <- sucessCount + (if pass then 1 else 0)
  
  printfn "Text Group: %s" tests.Data
  printfn "Success rate: %d/%d" sucessCount (Seq.length tests.Cases)
  printfn "---------" 

  
[<EntryPoint>]
let main args =
  let tests = [
    {
      Data = "./doc/not-boring-podcast"
      Cases = [ 
        {
          Query = "3 approaches of Capital"
          Expect = "capital-and-taste"
        }
        {
          Query = "a controversy when they sold Muslim prayer mats as fun home decor"
          Expect = "shein-the-tik-tok-ecomerce"
        }
        {
          Query = "Extending excellent taste"
          Expect = "capital-and-taste"
        }
        {
          Query = "combining usability with flexibility is both incredibly difficult and incredibly rewarding"
          Expect = "excel-never-dies"
        }
        {
          Query = "Investing in Ethereum"
          Expect = "own-the-internet"
        }
        {
          Query = "best practices for DAOs"
          Expect = "the-web3-debate.txt"
        }
        {
          Query = "companies which Not Boring Captial has invested in"
          Expect = "web3-usecase-of-the-future"
        }
        {
          Query = "Which blockchain ecosystem is thriving and has a lot of offer dApp developer"
          Expect = "solana-summer"
        }
      ]
    }
    {
      Data = "./doc/making-sense"
      Cases = [
        {
          Query = "Sam talks about a scale of suffering to flourishing. His guest has seems to evaluate things from suffering to zero, Sam Harris seems to evaluate things from suffering to zero to flourishing."
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "His guest has seems to evaluate things from suffering to zero, Sam Harris seems to evaluate things from suffering to zero to flourishing."
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "Oh yeah, this reminds me of some Sammy podcast where he speaks to someone who has the view that life is a scale like: 1  0 as in theres suffering or theres not. Where as Sammy was viewing more like: 1  0  1 where there’s flourishing to be had."
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "suffering scale"
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "Sam Harris suffering scale"
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "suffering flourishing scale"
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "guest evaluates things from suffering to zero, Sam evaluates things from suffering to zero to flourishing"
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
        {
          Query = "suffering to zero to flourishing"
          Expect =  "Making_Sense_107_Is_Life_Actually_Worth_Living_Full_7-6-22"
        }
      ]
    }
  ]
  
  for test in tests do runTest test
  
  0 // return an integer exit code

