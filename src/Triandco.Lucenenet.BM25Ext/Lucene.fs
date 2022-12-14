// This is a straight forward translation of the BM25Similarity implemented by LuceneDotnet in Fsharp for reference
module BM25.Lucene

open System
open Lucene.Net.Search

type SmallSingle = Lucene.Net.Util.SmallSingle
type SimWeight = Similarities.Similarity.SimWeight
type FieldInvertState = Lucene.Net.Index.FieldInvertState
type TunningParam = Core.Okapi.TunningParam

module Explain =
  let idfs (collectionStats: CollectionStatistics) (termStats: TermStatistics array) idfFunc= 
    if termStats.Length = 1 then 
      let df = termStats[0].DocFreq
      let max = collectionStats.MaxDoc
      let idf = idfFunc df max
      new Explanation(idf |> float32, $"idf(docFreq={df}, maxDocs={max})")
    else
      let max = collectionStats.MaxDoc
      let mutable idfVal = 0f
      let exp = new Explanation()
      for stat in termStats do
        let df = stat.DocFreq
        let termIdf = idfFunc df max
        exp.AddDetail(new Explanation(termIdf |>float32, $"idf(docFreq={df}, maxDocs={max})"));
        idfVal <- idfVal + termIdf;
      
      exp.Value <- idfVal
      exp

module Norm = 
  let table : single array = 
    [0..255]
    |> Seq.map sbyte
    |> Seq.map SmallSingle.SByte315ToSingle
    |> Seq.map (fun x -> 1f / (x * x))
    |> Seq.toArray

  let encode (boost: single) (fieldLength: int): byte = 
    boost / (single <| Math.Sqrt(fieldLength))
    |> SmallSingle.SingleToByte315

  let decode (b: byte) = table[(int b) &&& 0xFF]
   

type BM25Stats(field:string,  idf: Explanation, queryBoost:single,  avgdl:single, cache: single array) =
  inherit Similarities.Similarity.SimWeight()

  member this.Idf = idf
  member this.Avgdl = avgdl
  member this.QueryBoost = queryBoost
  member this.Cache = cache
  member this.Field = field

  member val TopLevelBoost = 0f with get, set
  member val Weight = 0f with get,set

  override this.GetValueForNormalization () : single  = 
    let queryWeight = (single this.Idf.Value) * this.QueryBoost
    queryWeight * queryWeight

  // we don't normalize with queryNorm at all, we just capture the top-level boost
  override this.Normalize(_:float32, topLevelBoost:float32) : unit =
    this.TopLevelBoost <- topLevelBoost
    this.Weight <- this.Idf.Value * this.QueryBoost * this.TopLevelBoost


[<AbstractClass>]
type BM25DocScorer () =
  inherit Similarities.Similarity.SimScorer()

  override this.ComputeSlopFactor(distance:int) : single = 
    1f / (distance + 1 |> single)

  override this.ComputePayloadFactor(_, _, _, _) = 1f // The default implementation returns 1

[<AbstractClass>]
type BM25 () =
  inherit Similarities.Similarity ()
  member val DiscountOverlaps: bool = true with get,set
       
  override this.ComputeNorm(state: FieldInvertState): int64 =
    let numTerms = if this.DiscountOverlaps then state.Length - state.NumOverlap else state.Length
    int64 <| Norm.encode state.Boost numTerms