module Library.Tests

open NUnit.Framework

type TestCase = { 
  Query: string 
  Expect: string
}

[<TestFixture>]
type Tests = 
  [<DefaultValue>] val mutable Rank: Ranker

  [<SetUp>]
  member this.Setup () =
    this.Rank <-
      "doc/not-boring-podcast/"
      |> System.IO.Directory.GetFiles
      |> Array.map (fun e -> {
        Path = e
        Content = System.IO.File.ReadAllText e
      })
      
    ()

  [<Test>]
  member this.Test1 () =
    let tests = [
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
    
    tests |> List.map (fun e ->
      let result = this.Rank
      Assert.That(result.Head.Path, Is.EqualTo(e.Expect))) 
    |> ignore
    ()
    
