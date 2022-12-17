module BM25.Core 

let ln =  System.Math.Log 

let avgDocumentLength (sumTotalTermFreq: int64) (maxDoc: int64): single = 
    if sumTotalTermFreq <= 0 then 1f else 
    (single sumTotalTermFreq) / (single maxDoc)

module Okapi = 
  type TunningParam = {
    K1: single
    B: single
  }

  module TunningParam =
    let defaultValue () = {
      K1 = 1.2f
      B = 0.75f
    }

  let idf (frequencies: int64) (documentCount: int64) : single =  
    let docf = double frequencies
    let nDocs = double documentCount
    1. + (nDocs -  docf + 0.5) / (docf + 0.5)
    |> float
    |> ln
    |> single
    

  // split computation of score in half so norms can be cached by downstream
  let computeDocumentNorm (k1:single) (b:single) (docLength: single) (avgdl: single) = 
    k1 * ((1f - b) + b * docLength / avgdl)

  let scoreWithNorm (k1:single) (idf:single) (tftd:single) (norm:single) =
    idf * (k1 + 1f) * tftd / (tftd + norm)

  // https://en.wikipedia.org/wiki/Okapi_BM25
  let score (param:TunningParam) (idf: float32) (tftd:float32) (docLength: float32) (avgdl:float32)= 
    // idf * tftd * (k1 + 1f) / (k1 * (1f - b + b * docLength / avgdl) +  tftd)
    let norm = computeDocumentNorm param.K1 param.B docLength avgdl
    scoreWithNorm param.K1 idf tftd norm
  

module Plus = 
  type TunningParam = {
    K1: float32
    B: float32
    Delta: float32
  }
  module TunningParam = 
    let defaultValue (): TunningParam = {
      K1 = 1.5f
      B = 0.75f
      Delta = 1f
    }

  let idf (frequencies:int64) (documentCount: int64): single =  
    let docf = float32 frequencies
    let nDocs = float32 documentCount
    (nDocs + 1f) / docf 
    |> float
    |> ln
    |> single

  // split computation of score in half so norms can be cached by downstream
  let computeDocumentNorm k1 b (docLength: float32) (avgdl: float32) = 
    k1 * (1f - b + b * docLength / avgdl)

  let scoreWithNorm (k1:float32) (delta:float32) (idf:float32) (tftd:float32) (norm:float32) =
    idf * (delta + (tftd * (k1 + 1f)) / norm + tftd)

  // http://www.cs.otago.ac.nz/homepages/andrew/papers/2014-2.pdf
  let score (param:TunningParam) (idf: float32) (tftd:float32) (docLength: float32) (avgdl:float32) =
    // idf * (delta + (tftd * (k1 + 1f)) / (k1 * (1f - b + b * docLength / avgdl) + tftd))
    let norm = computeDocumentNorm param.K1 param.B docLength avgdl
    scoreWithNorm param.K1 param.Delta idf tftd norm