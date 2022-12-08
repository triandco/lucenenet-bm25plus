module Library.LuceneRanking

open Lucene.Net.Analysis
open Lucene.Net

type Doc = {
  Path: string
  Content: string
}

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

let buildDirectory (doc: Doc list) = 
  let version = Util.LuceneVersion.LUCENE_48
  let analyzer = new Standard.StandardAnalyzer(version)

  let directory = new Store.RAMDirectory()
  let config = new Index.IndexWriterConfig(version, analyzer)
  use iwriter = new Index.IndexWriter(directory, config)
  
  doc 
  |> List.map (fun e -> 
    let doc = new Documents.Document()
    let field1 = new Documents.Field(Encoder.LuceneKey.Content, e.Content, Documents.TextField.TYPE_STORED)
    doc.Add(field1)
    let field2 = new Documents.Field(Encoder.LuceneKey.Path, e.Path, Documents.TextField.TYPE_STORED)
    doc.Add(field2)
    doc
  ) 
  |> List.iter iwriter.AddDocument 

  directory

let rank (directory:Store.RAMDirectory) (query: string) (count:int)= 
  let version = Util.LuceneVersion.LUCENE_48
  let analyzer = new Standard.StandardAnalyzer(version)

  use ireader = Index.DirectoryReader.Open(directory)
  let isearcher = new Search.IndexSearcher(ireader)
  let parser = new Lucene.Net.QueryParsers.Classic.QueryParser(version, Encoder.LuceneKey.Content, analyzer)
  let query = parser.Parse(query)
  
  isearcher.Search(query, null, count).ScoreDocs
  |> Array.map (fun e -> 
    let t = isearcher.Doc(e.Doc)
    { 
      Path = t.Get(Encoder.LuceneKey.Path)
      Score = e.Score
    }
  )
  
  