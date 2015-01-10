namespace Tests
 
open Format
open NUnit.Framework
 
[<TestFixture>]
module Test =
    [<Test>]
    let TestConvertToLine () =
        let input = List.map convertToWord ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]
        let ans = [Line [Word ("hello",5); Space 1; Word ("world",5)];
                   Line [Word ("hacker",6); Space 1];
                   Line [Word ("green",5); Space 1; Word ("army",4); Space 1];
                   Line [Word ("I",1); Space 1; Word ("ya",2); Space 1; Word ("u",1); Space 1; Word ("mami",4)];
                   Line [Word ("programmist",11)]]
        Assert.AreEqual((convertToLine input 11), ans)

    [<Test>]
    let TestConvertToLine2 () = 
        let input = List.map convertToWord ["slovo"; "ne"; "vorobey"; "слово"; "не"; "воробей"; "lol123"]
        let ans = [Line [Word ("slovo",5); Space 1]; Line [Word ("ne",2); Space 1];
                   Line [Word ("vorobey",7)]; Line [Word ("слово",5); Space 1];
                   Line [Word ("не",2); Space 1]; Line [Word ("воробей",7)];
                   Line [Word ("lol123",6); Space 1]]   
        Assert.AreEqual((convertToLine input 7), ans)

    [<Test>]
    let TestLeftFormat () = 
        let input = convertToLine(List.map convertToWord ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]) 11
        let ans = [Line [Word ("hello",5); Space 1; Word ("world",5)];
                   Line [Word ("hacker",6); Space 5];
                   Line [Word ("green",5); Space 1; Word ("army",4); Space 1];
                   Line [Word ("I",1); Space 1; Word ("ya",2); Space 1; Word ("u",1); Space 1; Word ("mami",4)]; 
                   Line [Word ("programmist",11)]]
        Assert.AreEqual((format input 11 Left), ans)

    [<Test>]
    let TestRightFormat () =
        let input = convertToLine(List.map convertToWord ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]) 11
        let ans = [Line [Word ("hello",5); Space 1; Word ("world",5)];
                   Line [Space 5; Word ("hacker",6)];
                   Line [Space 1; Word ("green",5); Space 1; Word ("army",4)];
                   Line [Word ("I",1); Space 1; Word ("ya",2); Space 1; Word ("u",1); Space 1; Word ("mami",4)];
                   Line [Word ("programmist",11)]]
        Assert.AreEqual((format input 11 Right), ans)

    [<Test>]
    let TestCenterFormat () =
        let input = convertToLine(List.map convertToWord ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]) 11
        let ans = [Line [Space 0; Word ("hello",5); Space 1; Word ("world",5)];
                   Line [Space 2; Word ("hacker",6); Space 3];
                   Line [Space 0; Word ("green",5); Space 1; Word ("army",4); Space 1];
                   Line [Space 0; Word ("I",1); Space 1; Word ("ya",2); Space 1; Word ("u",1); Space 1; Word ("mami",4)];
                   Line [Space 0; Word ("programmist",11)]]
        Assert.AreEqual((format input 11 Center), ans)

    [<Test>]
    let TestWidthFormat () =
        let input = convertToLine(List.map convertToWord ["hello"; "world"; "hacker"; "green"; "army";"I"; "ya"; "u"; "mami"; "programmist" ]) 11
        let ans = [Line [Word ("hello",5); Space 1; Word ("world",5)];
                   Line [Space 2; Word ("hacker",6); Space 3];
                   Line [Word ("green",5); Space 2; Word ("army",4)];
                   Line [Word ("I",1); Space 1; Word ("ya",2); Space 1; Word ("u",1); Space 1; Word ("mami",4)];
                   Line [Word ("programmist",11)]]
        Assert.AreEqual((format input 11 Width), ans)

    