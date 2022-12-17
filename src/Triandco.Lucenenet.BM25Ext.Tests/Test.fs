module BM25Tests.Cases
open NUnit.Framework
open BM25Tests.Search
open BM25Tests.Utils

[<TestFixture>]
type Tests () = 
  [<Test>]
  member this.VerifyMatchingOkapiConfiguration () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()
    Assert.That(fsharp.Param.B, Is.EqualTo(csharp.B), "Expect B to be equal")
    Assert.That(fsharp.Param.K1, Is.EqualTo(csharp.K1), "Expect K1 to be equal")

   
  [<Test>]
  member this.VerifyMatchingNormComputation () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()

    let collectionStats = Lucene.Net.Search.CollectionStatistics("blah", 10, 8, 20, 4)
    Assert.That(BM25.Core.avgDocumentLength collectionStats.SumTotalTermFreq collectionStats.MaxDoc, Is.EqualTo(csharp.AvgFieldLength(collectionStats)), "Expect AvgFieldLength to be equal")
    
    let t = 10f
    Assert.That(BM25.Lucene.Base.Norm.encode t 5, Is.EqualTo(csharp.EncodeNormValue(t, 5)), "Expect AvgFieldLength to be equal")
    
    for i in [0..255] do
      Assert.That(BM25.Lucene.Base.Norm.table[i], Is.EqualTo(LuceneSimilarities.BM25Similarity.NORM_TABLE[i]), $"Expect norm table at index {i} is equal")
    
    let testByte = 5uy
    Assert.That(BM25.Lucene.Base.Norm.decode testByte, Is.EqualTo(csharp.DecodeNormValue(testByte)), "Expect decode norm value is the equal")
    
    let state = Lucene.Net.Index.FieldInvertState("fun", 5, 3, 4, 3, 2f)
    Assert.That(fsharp.ComputeNorm state, Is.EqualTo(csharp.ComputeNorm(state)))

    let complexState = Lucene.Net.Index.FieldInvertState("Some field",100,20,50,40,20f)
    Assert.That(fsharp.ComputeNorm complexState, Is.EqualTo(csharp.ComputeNorm(complexState)))

 
  [<Test>]
  member this.VerifyMatchingCacheComputation () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()

    let avgdl = 0.5f
    let fsharpWeightCache = fsharp.getCache avgdl
    let nativeWeightCache = csharp.getCache avgdl

    for i in [0..255] do 
      Assert.That(fsharpWeightCache[i], Is.EqualTo(nativeWeightCache[i]))
  

  [<Test>]
  member this.VerifyMatchingExplanation () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()

    let byteRefHello = Lucene.Net.Util.BytesRef("hello")
    let byteRefWorld = Lucene.Net.Util.BytesRef("world")
    let termStat = [|
      Lucene.Net.Search.TermStatistics(byteRefHello, 5, 20)
      Lucene.Net.Search.TermStatistics(byteRefWorld, 2, 24)
    |]
    let collectionStats = Lucene.Net.Search.CollectionStatistics("blah", 10, 8, 20, 4)

    let explanationCsharp = csharp.IdfExplain(collectionStats, termStat[0])
    let explanationFsharp = BM25.Lucene.Base.Explain.idfs collectionStats [| termStat[0] |] BM25.Core.Okapi.idf
    Assert.That(explanationFsharp.Value, Is.EqualTo(explanationCsharp.Value), "Expect single term value to be equal")

    let explanationsCsharp = csharp.IdfExplain(collectionStats, termStat)
    let explanationsFsharp = BM25.Lucene.Base.Explain.idfs collectionStats termStat BM25.Core.Okapi.idf
    Assert.That(explanationsFsharp.Value, Is.EqualTo(explanationsCsharp.Value), "Expect single term value to be equal")

    let cSharpDetails = explanationsCsharp.GetDetails()
    let fSharpDetails = explanationsFsharp.GetDetails()

    Assert.That(fSharpDetails[0].Value, Is.EqualTo(cSharpDetails[0].Value), "Expect hello term is equal")
    Assert.That(fSharpDetails[1].Value, Is.EqualTo(cSharpDetails[1].Value), "Expect world term is equal")


  [<Test>]
  member this.VerifyComputeSimWeight () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()

    let byteRefHello = Lucene.Net.Util.BytesRef("Hello")
    let byteRefWorld = Lucene.Net.Util.BytesRef("world")
    let byteRefhello = Lucene.Net.Util.BytesRef("hello")

    let termStats = [|
      Lucene.Net.Search.TermStatistics(byteRefHello, 6, 20)
      Lucene.Net.Search.TermStatistics(byteRefWorld, 2, 24)
      Lucene.Net.Search.TermStatistics(byteRefhello, 3, 24)
    |]

    let collectionStats = Lucene.Net.Search.CollectionStatistics("blah", 10, 8, 20, 4)

    let csharpSimWeight = csharp.ComputeWeight(0f, collectionStats, termStats[0], termStats[1], termStats[2])
    let fsharpSimWeight = fsharp.ComputeWeight(0f, collectionStats, termStats[0], termStats[1], termStats[2])

    Assert.That(fsharpSimWeight.GetValueForNormalization(), Is.EqualTo(csharpSimWeight.GetValueForNormalization()), "Expect compute weight to be fsharpSimWeight")

  [<Test>]
  member this.VerifyIdf () = 
    let csharp = LuceneSimilarities.BM25Similarity()
    let rand = new System.Random()
    let numDoc = rand.Next(500, 600)
    let freq = rand.Next(5, 20)

    Assert.That(BM25.Core.Okapi.idf numDoc freq, Is.EqualTo(csharp.Idf(numDoc, freq)), "Expect idf to be equal")
    

  [<Test>]
  member this.VerifyMatchingOkapiResultSimple () =
    let csharp = Lucene.Net.Search.Similarities.BM25Similarity()
    let fsharp = BM25.Lucene.Okapi.Okapi()

    let docs = getAllDocuments "./doc/sample"
    let query = "hello"
    let count = 1
  
    let csharpIndex = index csharp docs
    let csharpResult = search csharp csharpIndex count query

    let fsharpIndex = index fsharp docs
    let fsharpResult = search fsharp fsharpIndex count query
    
    Assert.That(fsharpResult[0].Path, Is.EqualTo(csharpResult[0].Path), "Expect to found one result")
    Assert.That(fsharpResult[0].Score, Is.EqualTo(csharpResult[0].Score), "Expect score to be equal")
    ()

  [<Test>]
  member this.VerifyScoring () =
    let docs = getAllDocuments "./doc/sample-2"
    let query = "suffering scale"
    let count = Seq.length docs
    let luceneOkapi = Lucene.Net.Search.Similarities.BM25Similarity()
    let index1 = index luceneOkapi docs
    let luceneResult = search luceneOkapi index1 count query

    let triandcoOkapi = BM25.Lucene.Okapi.Okapi()
    let index2 = index triandcoOkapi docs
    let triandcoResult = search triandcoOkapi index2 count query

    for i in [0] do
      Assert.That(triandcoResult[i].Path, Is.EqualTo(luceneResult[i].Path), "Expect path in batch to be equal")
      Assert.That(triandcoResult[i].Score, Is.EqualTo(luceneResult[i].Score), "Expect scoring in batch to be equal")