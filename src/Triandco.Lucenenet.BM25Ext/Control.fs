module BM25Control


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


type BM25Stats (f:string, i:Explanation, qb: single, avg: single, c: single array)= 
  inherit SimWeight()
  member this.Idf: Explanation = i
  member this.Avgdl: single = avg
  member this.QueryBoost: single = qb
  member val TopLevelBoost: single = 0f with get, set 
  member val Weight: single = 0f with get,set
  member this.Field: string = f
  member this.Cache: single array = c
  

  override this.GetValueForNormalization() = 
    let queryWeight = this.Idf.Value * this.QueryBoost
    queryWeight * queryWeight

  override this.Normalize (queryNorm: single, topLevelBoost: single) = 
    this.TopLevelBoost <- topLevelBoost
    this.Weight <- this.Idf.Value * this.QueryBoost * topLevelBoost


type ExplainScore = int * Explanation * BM25Stats * NumericDocValues -> Explanation
type SloppyFreq = int -> Single
type ScorePayLoad = (int * int * int * BytesRef) -> float32

type BM25Scorer (k1: single, explainScore: ExplainScore, sloppyFreq: SloppyFreq, scorePayLoad: ScorePayLoad, sta: BM25Stats, n: NumericDocValues) =
  inherit SimScorer()
  member this.stats: BM25Stats = sta
  member this.weightValue: float32 =  sta.Weight * (k1 + 1f)
  member this.norms: NumericDocValues = n
  member this.cache: single array = sta.Cache


  override this.Score(doc: int, freq: single) : single = 
    let norm = 
      if this.norms = null then k1
      else 
       let t = sbyte <| this.norms.Get(doc)
       let d = (int t) &&& 0xFF
       float32 <| this.cache[d]
    this.weightValue * freq / (freq + norm)

  override this.Explain(doc: int, freq: Explanation): Explanation = 
    explainScore(doc, freq, sta, n)

  override this.ComputeSlopFactor (distance: int) = sloppyFreq(distance)

  override this.ComputePayloadFactor (doc: int, start: int, ``end``: int, payload: BytesRef) =
    scorePayLoad(doc, start, ``end``, payload)


type BM25Sim = 
  inherit Lucene.Net.Search.Similarities.Similarity
  val K1: Single
  val B: Single

  new (k1: Single, b: Single) = {
    inherit Lucene.Net.Search.Similarities.Similarity()
    K1 = k1
    B = b
  }

  new () = {
    inherit Lucene.Net.Search.Similarities.Similarity()
    K1 = 1.2f
    B = 0.75f
  }

  member this.Idf (docFreq: int64, numDocs: int64): float32 = 
    let numDocsF32 = float numDocs
    let docFreqF32 = float docFreq
    let value = 1. + (numDocsF32 - docFreqF32 + 0.5) / (docFreqF32 + 0.5)
    value |> float |> System.Math.Log |> float32

  member this.SloppyFreq (distance: int) : Single = 1.0f / (distance + 1 |> single)
  member this.ScorePayLoad(_, _, _, _) = 1.0f
  member this.AvgFieldLength(collectionStats: CollectionStatistics) : Single = 
    let sumTotalTermFreq = collectionStats.SumTotalTermFreq
    if sumTotalTermFreq <= 0 then 1f else 
    (float32 sumTotalTermFreq) / (float32 collectionStats.MaxDoc) |> single

  member this.EncodeNormValue (boost: Single, fieldLength: int) = 
    SmallSingle.SingleToByte315(boost / (single <| System.Math.Sqrt(fieldLength)))

  override this.ToString () = $"BM25(k1={this.K1},b={this.B})"

  member this.NORM_TABLE = this.LoadNormTable ()
  member this.LoadNormTable () : Single array = 
    [0..255]
    |> Seq.map sbyte
    |> Seq.map SmallSingle.SByte315ToSingle
    |> Seq.map (fun x -> 1f / (x * x))
    |> Seq.toArray

  member this.DecodeNormValue (b: byte) = this.NORM_TABLE[((int32 b) &&& 0xFF)]
  member this.DiscountOverlaps:bool = true 
  
  
  override this.ComputeNorm(state: FieldInvertState): int64 = 
    let numTerms = if this.DiscountOverlaps then state.Length - state.NumOverlap else state.Length
    int64 <| this.EncodeNormValue(state.Boost, numTerms) 

  member this.IdfExplain(collectionStats: CollectionStatistics, termStats: TermStatistics) = 
    let df = termStats.DocFreq
    let max = collectionStats.MaxDoc
    let idf = this.Idf(df, max)
    new Explanation(idf, $"idf(docFreq={df},maxDocs={max})")

  member this.IdfExplain(collectionStats: CollectionStatistics, termStats: TermStatistics array) = 
    let max = collectionStats.MaxDoc
    let mutable idfValue: single = 0.0f
    let exp = new Explanation()
    exp.Description <- "idf(), sum of:";
    for stat in termStats do 
      let df = stat.DocFreq
      let termIdf = this.Idf(df, max)
      exp.AddDetail(new Explanation(float32 <| termIdf, $"idf(docFreq={df},maxDocs={max})"))
      idfValue <- idfValue + termIdf
    exp.Value <- idfValue 
    exp

  member this.getCache avgdl : single array= 
      [0..255]
      |> Seq.map (fun i -> 
        let t = this.DecodeNormValue(byte i)
        this.K1 * ((1f - this.B) + this.B * t / avgdl))
      |> Seq.toArray
  
  
  override this.ComputeWeight (queryBoost: float32, collectionStats: CollectionStatistics, [<System.ParamArray>] termStats: TermStatistics array) : SimWeight = 
    let idfValue = if termStats.Length = 1 then this.IdfExplain(collectionStats, termStats[0]) else this.IdfExplain(collectionStats, termStats)
    let avgdl = this.AvgFieldLength(collectionStats)
    let cache = this.getCache avgdl
    
    new BM25Stats(collectionStats.Field, idfValue, queryBoost, avgdl, cache)

  override this.GetSimScorer (stats: SimWeight, context: AtomicReaderContext) =
    let bm25Stats = 
      match stats with 
      | :? BM25Stats as t -> t 
      | _ -> raise <| System.ArgumentException("Expected BM25Stats as SimWeight") 

    new BM25Scorer(this.K1, this.ExplainScore, this.SloppyFreq, this.ScorePayLoad, bm25Stats, context.AtomicReader.GetNormValues(bm25Stats.Field))


  member this.ExplainScore(doc: int, freq:Explanation, stats:BM25Stats, norms: NumericDocValues) = 
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
      tfNormExpl.Value <- freq.Value * ((float32 this.K1)+ 1f) / (freq.Value + (float32 this.K1))
    else 
      let doclen = this.DecodeNormValue(byte <| norms.Get(doc))
      tfNormExpl.AddDetail(new Explanation(float32<|this.B, "parameter b"))
      tfNormExpl.AddDetail(new Explanation(float32 <| stats.Avgdl, "avgFieldLength"))
      tfNormExpl.AddDetail(new Explanation(float32 <| doclen, "fieldLength"))
      tfNormExpl.Value <- ((float32 freq.Value) * ((float32 this.K1) + 1f)/ ((float32 freq.Value) + (float32 this.K1) * (1f - (float32 this.B)+ (float32 this.B) * (float32 doclen) / (float32 stats.Avgdl))))

      result.AddDetail(tfNormExpl)
      result.Value <- boostExplanation.Value * stats.Idf.Value * tfNormExpl.Value
    
    result

  

