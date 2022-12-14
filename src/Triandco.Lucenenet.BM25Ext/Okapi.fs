// This is a straight forward translation of the BM25Similarity implemented by LuceneDotnet in Fsharp for reference
module BM25.LuceneOkapi

open System
open Lucene.Net.Search
open Lucene.Net.Index
open BM25.Core
open BM25.Lucene
open BM25.Ext

type SimWeight = Similarities.Similarity.SimWeight
type SimScorer = Similarities.Similarity.SimScorer
type TunningParam = Okapi.TunningParam

module Explain =

  let score(param: TunningParam) (doc: int) (freq: Explanation) (stats: BM25Stats) (norms: NumericDocValues): Explanation  =
    let result = new Explanation()
    result.Description <- $"score(doc={doc}freq={freq}), product of:"

    let boostExpl = new Explanation(stats.QueryBoost * stats.TopLevelBoost, "boost")
    if boostExpl.Value <> 1.0f then result.AddDetail(boostExpl)

    result.AddDetail(stats.Idf)

    let tfNormExpl = new Explanation()
    tfNormExpl.Description <- "tfNorm, computed from:"
    tfNormExpl.AddDetail(freq)
    if norms = null then
      tfNormExpl.AddDetail(new Explanation(0f, "parameter b (norms omitted for field)"))
      tfNormExpl.Value <- Okapi.scoreWithNorm param.K1 1f freq.Value param.K1
    else
      let docLength = Norm.decode(byte <| norms.Get(doc))
      tfNormExpl.AddDetail(new Explanation(param.B, "parameter b"))
      tfNormExpl.AddDetail(new Explanation((float32 stats.Avgdl), "avgFieldLength"))
      tfNormExpl.AddDetail(new Explanation(docLength, "fieldLength"))
      tfNormExpl.Value <- Okapi.computeDocumentNorm param.K1 param.B docLength (float32 stats.Avgdl)
      
    
    result.AddDetail(tfNormExpl)
    result.Value <- boostExpl.Value * stats.Idf.Value * tfNormExpl.Value
    result


type Scorer(param: TunningParam, stats: BM25Stats, norms: NumericDocValues) =
  inherit BM25DocScorer()

  override this.Score(doc:int, freq:float32): float32 = 
    let norm =
      if norms = null then param.K1 else
      float32 <| stats.Cache[int (sbyte <| (norms.Get(doc) &&& 0xFF))]
    
    Okapi.scoreWithNorm param.K1 stats.Weight freq norm
  
  override this.Explain(doc: int, freq: Explanation): Explanation = 
    Explain.score param doc freq stats norms


type BM25Okapi =
  inherit BM25
  val param: TunningParam
  
  new (param) = { 
    inherit BM25() 
    param = param
  }

  new () = {
    inherit BM25() 
    param = Okapi.TunningParam.defaultValue () 
  }

  override this.GetSimScorer(weight: SimWeight, context: AtomicReaderContext) : SimScorer = 
    let w = 
      match weight with 
      | :? BM25Stats as stat -> stat
      | _ -> raise <| InvalidCastException("Expected to receive BM25Stats but received stranger.")
    
    let n = context.AtomicReader.GetNormValues(w.Field)
    new Scorer(this.param, w, n)

  member this.getCache avgdl : single array= 
      [0..255]
      |> Seq.map (fun i -> 
        let docLength = Norm.decode(byte i)
        Okapi.computeDocumentNorm this.param.K1 this.param.B docLength avgdl
      )
      |> Seq.toArray

  override this.ComputeWeight(queryBoost: float32, collectionStats: CollectionStatistics, [<System.ParamArray>] termStats: TermStatistics array): SimWeight =
    let idfValue = Explain.idfs collectionStats termStats Okapi.idf
    let avgdl = avgDocumentLength collectionStats.SumDocFreq collectionStats.MaxDoc
    let cache = this.getCache avgdl
    new BM25Stats(collectionStats.Field, idfValue, queryBoost, avgdl, cache)