module BM25.Lucene.Okapi


type AtomicReaderContext = Lucene.Net.Index.AtomicReaderContext
type BytesRef = Lucene.Net.Util.BytesRef
type FieldInvertState = Lucene.Net.Index.FieldInvertState
type NumericDocValues = Lucene.Net.Index.NumericDocValues
type SmallSingle = Lucene.Net.Util.SmallSingle
type CollectionStatistics = Lucene.Net.Search.CollectionStatistics
type TermStatistics = Lucene.Net.Search.TermStatistics
type Explanation = Lucene.Net.Search.Explanation
type SimWeight = Lucene.Net.Search.Similarities.Similarity.SimWeight
type SimScorer = Lucene.Net.Search.Similarities.Similarity.SimScorer
type Single = System.Single

type BM25Stats = BM25.Lucene.Base.BM25Stats
type TunningParam = BM25.Core.Okapi.TunningParam


type ExplainScore = int * Explanation * BM25Stats * NumericDocValues -> Explanation
type SloppyFreq = int -> Single
type ScorePayLoad = (int * int * int * BytesRef) -> float32

type BM25Scorer (
  param: TunningParam, 
  stats: BM25Stats, 
  norms: NumericDocValues) =
  inherit SimScorer()

  override this.Score(doc: int, freq: single) : single = 
    let norm = 
      if norms = null then param.K1
      else 
       let t = sbyte <| norms.Get(doc)
       let d = (int t) &&& 0xFF
       float32 <| stats.Cache[d]

    BM25.Core.Okapi.scoreWithNorm param.K1 stats.Weight freq norm

  
  override this.Explain(doc: int, freq:Explanation) : Explanation = 
    let result = new Explanation()
    result.Description <- $"score(doc={doc},freq={freq})"
    let boostExplanation = new Explanation(stats.QueryBoost * stats.TopLevelBoost, "boost")
    if boostExplanation.Value <> 1f then result.AddDetail(boostExplanation)
    result.AddDetail(stats.Idf)
    let tfNormExpl = new Explanation()
    tfNormExpl.Description <- "tfNorm, computed from:"
    tfNormExpl.AddDetail(freq)
    if norms = null then 
      tfNormExpl.AddDetail(new Explanation(0f, "parameter b (norms ommited for field"))
      // norm = k1 ; idf = 1
      tfNormExpl.Value <- BM25.Core.Okapi.scoreWithNorm param.K1 1f freq.Value param.K1 
    else 
      let doclen = BM25.Lucene.Base.Norm.decode (byte <| norms.Get(doc))
      tfNormExpl.AddDetail(new Explanation(float32<| param.B, "parameter b"))
      tfNormExpl.AddDetail(new Explanation(float32 <| stats.Avgdl, "avgFieldLength"))
      tfNormExpl.AddDetail(new Explanation(float32 <| doclen, "fieldLength"))
      tfNormExpl.Value <-  BM25.Core.Okapi.score param 1f freq.Value doclen stats.Avgdl // idf = 1
      result.AddDetail(tfNormExpl)
      result.Value <- boostExplanation.Value * stats.Idf.Value * tfNormExpl.Value
    
    result

  override this.ComputeSlopFactor (distance: int) = 1.0f / (distance + 1 |> single)
  override this.ComputePayloadFactor (doc: int, start: int, ``end``: int, payload: BytesRef) = 1f


type Okapi = 
  inherit BM25.Lucene.Base.BM25
  val Param: TunningParam

  new (param: TunningParam) = {
    inherit BM25.Lucene.Base.BM25()
    Param = param
  }

  new () = {
    inherit BM25.Lucene.Base.BM25()
    Param = {
      K1 = 1.2f
      B =0.75f
    }
  }

  override this.ToString () = $"BM25(k1={this.Param.K1},b={this.Param.B})"
  
  member this.getCache avgdl : single array = 
      [0..255]
      |> Seq.map (fun i -> 
        let t = BM25.Lucene.Base.Norm.decode (byte i)
        BM25.Core.Okapi.computeDocumentNorm this.Param.K1 this.Param.B t avgdl)
      |> Seq.toArray
  
  
  override this.ComputeWeight (queryBoost: float32, collectionStats: CollectionStatistics, [<System.ParamArray>] termStats: TermStatistics array) : SimWeight = 
    let idfValue = BM25.Lucene.Base.Explain.idfs collectionStats termStats BM25.Core.Okapi.idf
    let avgdl = BM25.Core.avgDocumentLength collectionStats.SumTotalTermFreq collectionStats.MaxDoc
    let cache = this.getCache avgdl
    
    new BM25Stats(collectionStats.Field, idfValue, queryBoost, avgdl, cache)

  override this.GetSimScorer (stats: SimWeight, context: AtomicReaderContext) =
    let bm25Stats = 
      match stats with 
      | :? BM25Stats as t -> t 
      | _ -> raise <| System.ArgumentException("Expected BM25Stats as SimWeight") 

    new BM25Scorer(this.Param, bm25Stats, context.AtomicReader.GetNormValues(bm25Stats.Field))


  

  