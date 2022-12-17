// This is a straight forward translation of the BM25Similarity implemented by LuceneDotnet in Fsharp for reference
module BM25.Lucene.Plus


open System
open Lucene.Net.Search
open Lucene.Net.Index
open BM25.Core
open BM25.Lucene.Base
open BM25.Ext

type SimWeight = Similarities.Similarity.SimWeight
type SimScorer = Similarities.Similarity.SimScorer

type TunningParam = Plus.TunningParam

module Explain =

  let score(param: TunningParam) (doc: int) (freq: Explanation) (stats: BM25Stats) (norms: NumericDocValues option): Explanation  =
    let result = new Explanation()
    result.Description <- $"score(doc={doc}freq={freq}), product of:"

    let boostExpl = new Explanation(float32 <| stats.QueryBoost * stats.TopLevelBoost, "boost")
    if boostExpl.Value <> 1.0f then
      result.AddDetail(boostExpl)

    result.AddDetail(stats.Idf)

    let tfNormExpl = new Explanation()
    tfNormExpl.Description <- "tfNorm, computed from:"
    tfNormExpl.AddDetail(freq)
    tfNormExpl.AddDetail(new Explanation(param.K1, "parameter k1"))
    tfNormExpl.AddDetail(new Explanation(param.B, "parameter b"))
    tfNormExpl.AddDetail(new Explanation(param.Delta, "parameter delta"))
    match norms with
    | None ->
      tfNormExpl.AddDetail(new Explanation(0f, "parameter b (norms omitted for field)"))
      tfNormExpl.Value <- Plus.scoreWithNorm param.K1 param.Delta 1f freq.Value param.K1
    | Some n ->
      let doclen = Norm.decode(byte <| n.Get(doc))
      tfNormExpl.AddDetail(new Explanation(param.B, "parameter b"))
      tfNormExpl.AddDetail(new Explanation((float32 stats.Avgdl), "avgFieldLength"))
      tfNormExpl.AddDetail(new Explanation(doclen, "fieldLength"))
      tfNormExpl.Value <- Plus.computeDocumentNorm param.K1 param.B doclen (float32 stats.Avgdl)
    
    result.AddDetail(tfNormExpl)
    result.Value <- boostExpl.Value * stats.Idf.Value * tfNormExpl.Value
    result


type Scorer(param: TunningParam, stats: BM25Stats, norms: NumericDocValues option) =
  inherit BM25DocScorer()

  override this.Score(doc:int, freq:float32): float32 = 
    let norm =
      match norms with 
      | None -> param.K1
      | Some n -> float32 <| stats.Cache[int (sbyte <| (n.Get(doc) &&& 0xFF))]
    
    Plus.scoreWithNorm param.K1 param.Delta stats.Weight freq norm
  
  override this.Explain(doc: int, freq: Explanation): Explanation = 
    Explain.score param doc freq stats norms


type BM25Plus =
  inherit BM25
  val param: TunningParam
  
  new (param) = { 
    inherit BM25() 
    param = param
  }

  new () = {
    inherit BM25() 
    param = Plus.TunningParam.defaultValue () 
  }
  

  override this.GetSimScorer(weight: SimWeight, context: AtomicReaderContext) : SimScorer = 
    let w = 
      match weight with 
      | :? BM25Stats as stat -> stat
      | _ -> raise <| InvalidCastException("Expected to receive BM25Stats but received stranger.")
    
    let n = Option.optional <| context.AtomicReader.GetNormValues(w.Field)
    new Scorer(this.param, w, n)


  override this.ComputeWeight(queryBoost: float32, collectionStats: CollectionStatistics, [<System.ParamArray>] termStats: TermStatistics[]): SimWeight =
    let idf = Explain.idfs collectionStats termStats Plus.idf
    let avgdl = avgDocumentLength collectionStats.SumDocFreq collectionStats.MaxDoc

    let cache =
      [0..256]
      |> Seq.map (fun i ->
        let docLength = Norm.decode((byte i))
        Plus.computeDocumentNorm this.param.K1 this.param.Delta docLength avgdl
      )
      |> Seq.toArray

    new BM25Stats(collectionStats.Field, idf, queryBoost, avgdl, cache)
