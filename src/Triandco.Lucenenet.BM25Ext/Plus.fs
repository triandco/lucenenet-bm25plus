module BM25.Lucene.Plus

type AtomicReaderContext = Lucene.Net.Index.AtomicReaderContext
type NumericDocValues = Lucene.Net.Index.NumericDocValues
type CollectionStatistics = Lucene.Net.Search.CollectionStatistics
type TermStatistics = Lucene.Net.Search.TermStatistics
type Explanation = Lucene.Net.Search.Explanation
type SimWeight = Lucene.Net.Search.Similarities.Similarity.SimWeight
type TunningParam = BM25.Core.Plus.TunningParam

type Scorer (
  param: TunningParam, 
  stats: Base.BM25Stats, 
  norms: NumericDocValues option) =
  inherit Base.BM25DocScorer()

  override this.Score(doc: int, freq: single) : single = 
    Base.BM25DocScorer.normOrDefault stats doc param.K1 norms
    |> BM25.Core.Plus.scoreWithNorm param.K1 param.Delta stats.Weight freq

  
  override this.Explain(doc: int, freq:Explanation) : Explanation = 
    let result = new Explanation()
    result.Description <- $"score(doc={doc},freq={freq})"
    let boostExplanation = new Explanation(stats.QueryBoost * stats.TopLevelBoost, "boost")
    if boostExplanation.Value <> 1f then result.AddDetail(boostExplanation)
    result.AddDetail(stats.Idf)
    let tfNormExpl = new Explanation()
    tfNormExpl.Description <- "tfNorm, computed from:"
    tfNormExpl.AddDetail(freq)
    match norms with 
    | None -> 
      tfNormExpl.AddDetail(new Explanation(0f, "parameter b (norms ommited for field"))
      // norm = k1 ; idf = 1
      tfNormExpl.Value <- BM25.Core.Plus.scoreWithNorm param.K1 param.Delta 1f freq.Value param.K1
    | Some n ->
      let doclen = Base.Norm.decode (byte <| n.Get(doc))
      tfNormExpl.AddDetail(new Explanation(float32 <| param.B, "parameter b"))
      tfNormExpl.AddDetail(new Explanation(float32 <| stats.Avgdl, "avgFieldLength"))
      tfNormExpl.AddDetail(new Explanation(float32 <| doclen, "fieldLength"))
      tfNormExpl.Value <-  BM25.Core.Plus.score param 1f freq.Value doclen stats.Avgdl // idf = 1
      result.AddDetail(tfNormExpl)
      result.Value <- boostExplanation.Value * stats.Idf.Value * tfNormExpl.Value
    
    result


type Similarity = 
  inherit Base.BM25
  val Param: TunningParam

  new (param: TunningParam) = {
    inherit Base.BM25()
    Param = param
  }

  new () = {
    inherit Base.BM25()
    Param = {
      K1 = 1.2f
      B = 0.75f
      Delta = 1f
    }
  }

  override this.ToString () = $"BM25(k1={this.Param.K1},b={this.Param.B},delta={this.Param.Delta})"
  
  member this.getCache avgdl : single array = 
    [0..255]
    |> Seq.map (fun i -> 
      let t = Base.Norm.decode (byte i)
      BM25.Core.Plus.computeDocumentNorm this.Param.K1 this.Param.B t avgdl)
    |> Seq.toArray
  
  
  override this.ComputeWeight (queryBoost: float32, collectionStats: CollectionStatistics, [<System.ParamArray>] termStats: TermStatistics array) : SimWeight = 
    let idfValue = Base.Explain.idfs collectionStats termStats BM25.Core.idf
    let avgdl = BM25.Core.avgDocumentLength collectionStats.SumTotalTermFreq collectionStats.MaxDoc
    let cache = this.getCache avgdl
    new Base.BM25Stats(collectionStats.Field, idfValue, queryBoost, avgdl, cache)

  override this.GetSimScorer (stats: SimWeight, context: AtomicReaderContext) =
    let bm25Stats = Base.BM25Stats.unsafeParse stats
    let norms = Base.tryGetNorms context bm25Stats
    new Scorer(this.Param, bm25Stats, norms)
