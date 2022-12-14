module BM25.Ext 

module Option = 
  let optional = 
    function
    | null -> None 
    | t -> Some t