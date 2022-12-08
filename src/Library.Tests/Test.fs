module Library.Tests

open NUnit.Framework
open Library

[<SetUp>]
let Setup () =
  ()

[<Test>]
let Test1 () =
  let input = "Dog"
  let expected = "Hello Dog"
  let value = Say.hello input
  Assert.That(value, Is.EqualTo(expected))
