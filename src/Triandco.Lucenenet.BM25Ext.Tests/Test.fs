module BM25Tests.Cases
open NUnit.Framework
open BM25Tests.Search
open BM25Tests.Utils

[<TestFixture>]
type Tests () = 
  [<Test>]
  member this.VerifyMatchingOkapiConfiguration () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharpBM5 = BM25Control.BM25Sim()
    Assert.That(fsharpBM5.B, Is.EqualTo(csharp.B), "Expect B to be equal")
    Assert.That(fsharpBM5.K1, Is.EqualTo(csharp.K1), "Expect K1 to be equal")

   
  [<Test>]
  member this.VerifyMatchingOkapiComputation () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharpBM5 = BM25Control.BM25Sim()

    let collectionStats = Lucene.Net.Search.CollectionStatistics("blah", 10, 8, 20, 4)
    Assert.That(BM25.Core.avgDocumentLength collectionStats.SumTotalTermFreq collectionStats.MaxDoc, Is.EqualTo(csharp.AvgFieldLength(collectionStats)), "Expect AvgFieldLength to be equal")
    
    let t = 10f
    Assert.That(BM25.Lucene.Norm.encode t 5, Is.EqualTo(csharp.EncodeNormValue(t, 5)), "Expect AvgFieldLength to be equal")
    
    for i in [0..255] do
      Assert.That(BM25.Lucene.Norm.table[i], Is.EqualTo(LuceneSimilarities.BM25Similarity.NORM_TABLE[i]), $"Expect norm table at index {i} is equal")
    
    let testByte = 5uy
    Assert.That(BM25.Lucene.Norm.decode testByte, Is.EqualTo(csharp.DecodeNormValue(testByte)), "Expect decode norm value is the equal")
    
    let state = Lucene.Net.Index.FieldInvertState("fun", 5, 3, 4, 3, 2f)
    Assert.That(fsharpBM5.ComputeNorm state, Is.EqualTo(csharp.ComputeNorm(state)))

    let avgdl = 0.5f
    let fsharpWeightCache = fsharpBM5.getCache avgdl
    let nativeWeightCache = csharp.getCache avgdl

    for i in [0..255] do 
      Assert.That(fsharpWeightCache[i], Is.EqualTo(nativeWeightCache[i]))
  
    let docFreq = 10
    let numDocs = 20
    Assert.That(BM25.Core.Okapi.idf docFreq numDocs, Is.EqualTo(csharp.Idf(docFreq, numDocs)), "Expect idf calculation to match")

    let byteRefHello = Lucene.Net.Util.BytesRef("hello")
    let byteRefWorld = Lucene.Net.Util.BytesRef("world")
    let termStat = [|
      Lucene.Net.Search.TermStatistics(byteRefHello, 5, 20)
      Lucene.Net.Search.TermStatistics(byteRefWorld, 2, 24)
    |]
    let explanationCsharp = csharp.IdfExplain(collectionStats, termStat[0])
    let explanationFsharp = BM25.Lucene.Explain.idfs collectionStats [| termStat[0] |] BM25.Core.Okapi.idf
    Assert.That(explanationFsharp.Value, Is.EqualTo(explanationCsharp.Value), "Expect single term value to be equal")

    let explanationsCsharp = csharp.IdfExplain(collectionStats, termStat)
    let explanationsFsharp = BM25.Lucene.Explain.idfs collectionStats termStat BM25.Core.Okapi.idf
    Assert.That(explanationsFsharp.Value, Is.EqualTo(explanationsCsharp.Value), "Expect single term value to be equal")

    let cSharpDetails = explanationsCsharp.GetDetails()
    let fSharpDetails = explanationsFsharp.GetDetails()

    Assert.That(fSharpDetails[0].Value, Is.EqualTo(cSharpDetails[0].Value), "Expect hello term is equal")
    Assert.That(fSharpDetails[1].Value, Is.EqualTo(cSharpDetails[1].Value), "Expect world term is equal")


  [<Test>]
  member this.VerifyMatchingOkapiResult () =
    let csharp = LuceneSimilarities.BM25Similarity()
    let fsharpBM5 = BM25Control.BM25Sim()

    let docs = getAllDocuments "D:/Developer/triandco/blau/prototypes/lucene.net/src/Triandco.Lucenenet.BM25Ext.Tests/doc/sample"
    let query = "hello"
    let count = 1
  
    let index1 = index csharp docs
    let bm25NativeResult = search csharp index1 count query

    let index2 = index fsharpBM5 docs
    let fsharpBM5Result = search fsharpBM5 index2 count query
    
    Assert.That(fsharpBM5Result[0].Path, Is.EqualTo(bm25NativeResult[0].Path), "Expect to found one result")
    Assert.That(fsharpBM5Result[0].Score, Is.EqualTo(bm25NativeResult[0].Score), "Expect score to be equal")
    ()

  // [<Test>]
  // member this.VerifyScoring () =
  //   let docs = getAllDocuments "D:/Developer/triandco/blau/prototypes/lucene.net/src/Triandco.Lucenenet.BM25Ext.Tests/doc/sample-2"
  //   let query = "suffering scale"
  //   let count = Seq.length docs
  //   let luceneOkapi = LuceneSimilarities.BM25Similarity()
  //   let index1 = index luceneOkapi docs
  //   let luceneResult = search luceneOkapi index1 count query

  //   let triandcoOkapi = BM25.LuceneOkapi.BM25Okapi()
  //   let index2 = index triandcoOkapi docs
  //   let triandcoResult = search triandcoOkapi index2 count query

  //   for i in [0] do
  //     Assert.That(triandcoResult[i].Path, Is.EqualTo(luceneResult[i].Path), "Expect path in batch to be equal")
  //     Assert.That(triandcoResult[i].Score, Is.EqualTo(luceneResult[i].Score), "Expect scoring in batch to be equal")