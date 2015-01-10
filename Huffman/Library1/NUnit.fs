namespace Test
 
open Huffman
open NUnit.Framework
 
[<TestFixture>]
module Test = 
    [<Test>]
    let TestMakeTree () = 
        let input = "abacdcba" 
        let ans = Fork (Leaf ('a',3), Fork (Leaf ('b',2),Fork (Leaf ('d',1),Leaf ('c',2),['d'; 'c'],3), ['b'; 'd'; 'c'],5),['a'; 'b'; 'd'; 'c'],8)
        Assert.AreEqual((createCodeTree input), ans)

    [<Test>]
    let TestEncode () = 
        let input = createCodeTree "abacdcba"
        let ans = [0; 1; 0; 0; 1; 1; 1; 1; 1; 0; 1; 1; 1; 1; 0; 0]
        Assert.AreEqual((encode input ("abacdcba" |> stringzchars)), ans)

    [<Test>]
    let TestDecode () = 
        let input = [0; 1; 0; 0; 1; 1; 1; 1; 1; 0; 1; 1; 1; 1; 0; 0]
        let ans = ['a'; 'b'; 'a'; 'c'; 'd'; 'c'; 'b'; 'a']
        Assert.AreEqual((decode (createCodeTree "abacdcba") input ), ans)
