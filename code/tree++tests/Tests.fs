namespace treetests

open System
open System.Text.RegularExpressions
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open AST

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.SizeCanBeLeftOut () =
        let input = "fall: 2 oak, 1 pine, 5 maple"
        let expected = {season = Fall; 
            forest = [{num = 2; kind = Oak; size = 1.0};
                {num = 1; kind = Pine; size = 1.0};
                {num = 5; kind = Maple; size = 1.0}]}
        let result = parse input
        match result with
        | Some landsc ->
            Assert.AreEqual(expected, landsc)
        | None ->
            Assert.IsTrue(false)
    
    [<TestMethod>]
    member this.CorrectNumOfTrees () =
        let substringCount (substr: string) (str: string) =
            Regex.Matches(str, Regex.Escape substr).Count
             
        let input = {season = Fall; 
            forest = [{num = 2; kind = Oak; size = 1.0};
                {num = 1; kind = Pine; size = 1.0};
                {num = 5; kind = Maple; size = 1.0}]}
        let expected = 8
        let treeStr = "<g transform="
        let result = eval input
        let treeCount = substringCount treeStr result
        if treeCount = expected then Assert.IsTrue(true)
        else Assert.IsTrue(false)
