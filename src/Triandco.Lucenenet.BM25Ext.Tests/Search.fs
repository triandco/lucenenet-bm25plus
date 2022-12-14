module BM25Tests.Search

open Lucene.Net.Analysis
open Lucene.Net
open BM25Tests.Utils

type SearchResult = {
  Path: string
  Score: float32
}

module Encoder = 
  module LuceneKey = 
    [<Literal>]
    let Content = "Content"
    
    [<Literal>]
    let Path = "Path"

let index (similarity:Search.Similarities.Similarity) (doc: Doc list) = 
  let version = Util.LuceneVersion.LUCENE_48
  use analyzer = new Standard.StandardAnalyzer(version)

  let directory = new Store.RAMDirectory()
  let config = new Index.IndexWriterConfig(version, analyzer)
  config.Similarity <- similarity
  use writer = new Index.IndexWriter(directory, config)
  
  doc 
  |> List.map (fun e -> 
    let doc = new Documents.Document()
    let field1 = new Documents.Field(Encoder.LuceneKey.Content, e.Content, Documents.TextField.TYPE_STORED)
    doc.Add(field1)
    let field2 = new Documents.Field(Encoder.LuceneKey.Path, e.Path, Documents.TextField.TYPE_STORED)
    doc.Add(field2)
    doc) 
  |> List.iter writer.AddDocument 

  directory


let search (similarity:Search.Similarities.Similarity) (directory:Store.RAMDirectory) (count:int) (query: string)= 
  let version = Util.LuceneVersion.LUCENE_48
  use analyzer = new Standard.StandardAnalyzer(version)

  use reader = Index.DirectoryReader.Open(directory)
  let searcher = new Search.IndexSearcher(reader)

  searcher.Similarity <- similarity
  let parser = new QueryParsers.Classic.QueryParser(version, Encoder.LuceneKey.Content, analyzer)
  let query = parser.Parse(query)
  
  searcher.Search(query, null, count).ScoreDocs
  |> Array.map (fun e -> 
    let t = searcher.Doc(e.Doc)
    { 
      Path = t.Get(Encoder.LuceneKey.Path)
      Score = e.Score
    })
  