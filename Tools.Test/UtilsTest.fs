module UtilsTest

open System
open System.Collections.Generic

let pcg =
    test "pcg" {
        test "string roundtrip" {
            let! stream = Gen.int.[..100]
            let! seed = Gen.uint64
            let expected = PCG(stream,seed)
            let actual = PCG.TryParse(string expected) |> Option.get
            Test.equal actual.Stream expected.Stream "stream"
            Test.equal actual.State expected.State "state"
        }
        // from the github https://github.com/imneme/pcg-c-basic minimal c implementation http://www.pcg-random.org/download.html#minimal-c-implementation
        test "PCG demo 1" {
            let pcg = PCG.TryParse "36185706b82c2e03f8" |> Option.get
            Test.equal pcg.Stream 54 "stream"
            Test.equal pcg.State 0x185706b82c2e03f8UL "state"
            let expectedNext = [|0xa15c02b7u;0x7b47f409u;0xba1d3330u;0x83d2f293u;0xbfa4784bu;0xcbed606eu;0xbfc6a3adu;0x812fff6du;0xe61f305au|]
            let expectedState = [|0x2b47fed88766bb05UL;0x8b33296d19bf5b4eUL;0xf7079824c154bf23UL;0xebbf9e97aa16f694UL;0x8303569fbe80c471UL;0xbeb6d0b73fdb974aUL;0xed81149f2fb94e6fUL;0x730f84eec16daf0UL;0x91723b7b84518c9dUL|]
            for i = 0 to expectedNext.Length-1 do
                Test.equal (pcg.Next()) expectedNext.[i] ("n"+string i)
                Test.equal pcg.State expectedState.[i] ("s"+string i)
        }
        test "PCG demo 2" {
            let pcg = PCG.TryParse "1c04f77d504556f19" |> Option.get
            Test.equal pcg.Stream 1 "stream"
            Test.equal pcg.State 0xc04f77d504556f19UL "state"
            let expectedNext = [|0xe8f8db09u;0x0d01e424u;0xeb1929a2u;0x00428cebu;0x747f0a17u;0xe4a907efu;0x686c869fu;0xab4acaedu;0x0bfa48c7u|]
            let expectedState = [|0x2680fbb23aaeee68UL;0x6494c850bb8d804bUL;0x421477dd1a2bc232UL;0x1d1fc5c22e21f0cdUL;0xbfbfbd4bf5be070cUL;0x39b2141121e2311fUL;0xfead1480f62c0376UL;0xbb1c012e272225c1UL;0xe30a9b89171061f0UL|]
            for i = 0 to expectedNext.Length-1 do
                Test.equal (pcg.Next()) expectedNext.[i] ("n"+string i)
                Test.equal pcg.State expectedState.[i] ("s"+string i)
        }
        test "PCG demo 3" {
            let pcg = PCG.TryParse "05e64366ec2781f14" |> Option.get
            Test.equal pcg.Stream 0 "stream"
            Test.equal pcg.State 0x5e64366ec2781f14UL "state"
            let expectedNext = [|0xdf399087u;0x361c3e74u;0x532acb4fu;0x3bfccb00u;0x46d6c872u;0x454e4b43u;0xbf263a6au;0x7cae8e93u;0x5c2d9c24u|]
            let expectedState = [|0x40e1e399cd2c6285UL;0xdbd4fc47e9164c62UL;0x9160232795da0b3bUL;0x7590f9e9903d3e60UL;0xd8d165a68a9596e1UL;0x9e9a886f2f1a248eUL;0x88e915f0ae60def7UL;0x5b671fcecf66ba6cUL;0x9dc31b5cfc6658fdUL|]
            for i = 0 to expectedNext.Length-1 do
                Test.equal (pcg.Next()) expectedNext.[i] ("n"+string i)
                Test.equal pcg.State expectedState.[i] ("s"+string i)
        }
    }

let array =
    let shuffleInPlace (a:_ array) =
        let rand = PCG 1234
        for i = Array.length a - 1 downto 1 do
            let j = rand.Next(i+1)
            if i<>j then
                let temp = a.[j]
                a.[j] <- a.[i]
                a.[i] <- temp
    test "array" {
        test "shuffle" {
            let! actual = Gen.array.[..100] Gen.int.[..100]
            let expected = Array.sort actual
            shuffleInPlace actual
            Array.sortInPlace actual
            Test.equal actual expected "same items"
        }
    }

type KeyWithHash =
    val key : uint64
    val hashCode : int
    new(i:uint64, hashCode: int) = { key = i; hashCode = hashCode }
    member m.Key = m.key
    interface IEquatable<KeyWithHash> with
        member m.Equals (o:KeyWithHash) =
            m.key = o.key
    override m.Equals(o:obj) =
        o :? KeyWithHash && (o :?> KeyWithHash).key = m.key
    override m.GetHashCode() = m.hashCode

let mapslim =
    test "mapslim" {
        test "unit tests" {
            test "get none" {
                let! i = Gen.int
                let ms = MapSlim()
                Test.equal (ms.GetOption i) ValueNone "none"
            }
            test "get one none" {
                let! i = Gen.int
                let ms = MapSlim()
                let x = &ms.GetRef i
                x <- x + 1
                Test.equal (ms.GetOption (i+1)) ValueNone "none"
            }
            test "get one some" {
                let! k = Gen.int
                let! v = Gen.int
                let ms = MapSlim()
                ms.Set(k,v)
                Test.equal (ms.GetOption k) (ValueSome v) "some"
            }
            test "same hashcode" {
                let ms = MapSlim()
                let key1 = KeyWithHash(7UL, 3)
                ms.Set(key1,11)
                let key2 = KeyWithHash(19UL, 3)
                ms.Set(key2,53)
                let key3 = KeyWithHash(27UL, 3)
                ms.Set(key3,99)
                Test.equal (ms.GetOption key1) (ValueSome 11) "key1"
                Test.equal (ms.GetOption key2) (ValueSome 53) "key2"
                Test.equal (ms.GetOption key3) (ValueSome 99) "key3"
            }
            test "getref update" {
                let ms = MapSlim()
                let x = &ms.GetRef 7
                x <- x + 1
                let x = &ms.GetRef 7
                x <- x + 3
                Test.equal (ms.GetOption 7) (ValueSome 4) "update"
            }
            test "count" {
                let! c = Gen.int.[..1000]
                let ms = MapSlim()
                for i = c-1 downto 0 do
                    ms.Set(i,i)
                Test.equal ms.Count c "count"
            }
            test "item" {
                let ms = MapSlim()
                ms.Set(5,11)
                ms.Set(3,53)
                Test.equal (ms.Item 0) (5,11) "item 0"
                Test.equal (ms.Item 1) (3,53) "item 1"
            }
        }
        test "reference" {
            let getTest name (gen:Gen<'a>) = test name {
                let! items = Gen.tuple gen Gen.int
                             |> Gen.list.[..100]
                let! check = gen
                let actual =
                    let ms = MapSlim()
                    List.iter ms.Set items
                    ms.GetOption check
                    |> function | ValueSome i -> Some i | ValueNone -> None
                let expected =
                    List.fold (fun m (k,v) -> Map.add k v m) Map.empty items
                    |> Map.tryFind check
                Test.equal actual expected "check value equal"
            }
            getTest "get byte" Gen.byte
            getTest "get char" Gen.char
            getTest "get int" Gen.int.[..10]
            getTest "get uint" Gen.uint.[..10u]
            getTest "get string" Gen.string
            let groupTest name (gen:Gen<'a>) = test name {
                let! items = Gen.tuple gen Gen.int.[-100000..1000000]
                             |> Gen.list.[..100]
                let actual =
                    let ms = MapSlim()
                    List.iter (fun (k,v) ->
                        let t = &ms.GetRef k
                        t <- t+v
                    ) items
                    List.init ms.Count ms.Item
                    |> List.sort
                let expected =
                    List.groupBy fst items
                    |> List.map (fun (k,l) -> k, List.sumBy snd l)
                    |> List.sort
                Test.equal actual expected "group by"
            }
            groupTest "group byte" Gen.byte
            groupTest "group char" Gen.char
            groupTest "group int" Gen.int.[..10]
            groupTest "group uint" Gen.uint.[..10u]
            groupTest "group string" Gen.string
        }
        test "multithreading" {
            let n = 10
            let ms = MapSlim()
            test "update" {
                let! i = Gen.int.[..n-1]
                let v = &ms.GetRef i
                v <- 1-v
            }
            test "get" {
                let! i = Gen.int.[..n-1]
                let v = defaultValueArg (ms.GetOption i) 0
                Test.isTrue (v=0 || v=1) "get is 0 or 1"
            }
            test "item" {
                if ms.Count > 0 then
                    let! i = Gen.int.[..ms.Count-1]
                    let k,v = ms.Item i
                    Test.lessThan k n "key is ok"
                    Test.isTrue (v=0 || v=1) "value is 0 or 1"
            }
        }
        test "performance" {
            test "get" {
                let ms = MapSlim()
                let dict = Dictionary()
                let! n = Gen.int.[1..100]
                let! keys =
                    Gen.array.[n] Gen.int.[-1000..1000]
                    |> Gen.map (
                        Array.mapi (fun i h ->
                            let k = KeyWithHash(uint64 i, h)
                            ms.Set(k,k)
                            dict.Add(k,k)
                            k
                        )
                    )
                Test.faster
                    (fun () ->
                        for i = 0 to n-1 do
                            ms.GetOption keys.[i] |> ignore
                    )
                    (fun () ->
                        for i = 0 to n-1 do
                            dict.TryGetValue keys.[i] |> ignore
                    )
                    "get"
            }
            test "set" {
                let! keys =
                    let size, aggCount = 500, 25
                    Gen.uint64.[..uint64(size/aggCount-1)]
                    |> Gen.array.[size]
                let! hashes =
                    let k = Array.distinct keys
                    Gen.array.[k.Length] Gen.int.[-1000..1000]
                    |> Gen.map (Array.map2 (fun k v -> KeyWithHash(k,v)) k)
                let keys =
                    Array.map (fun k -> hashes |> Array.find (fun h -> h.Key = k)) keys
                Test.faster
                    (fun () ->
                        let ms = MapSlim()
                        for i = 0 to keys.Length-1 do
                            let k = keys.[i]
                            let v = &ms.GetRef k
                            v <- v + 1
                    )
                    (fun () ->
                        let dict = Dictionary()
                        for i = 0 to keys.Length-1 do
                            let k = keys.[i]
                            let mutable t = Unchecked.defaultof<_>
                            if dict.TryGetValue(k, &t) then
                                dict.[k] <- t + 1
                            else
                                dict.Add(k,1)
                    )
                    "set"
            }
            let memoizeOld (f:'a->'b) =
                let d = Dictionary HashIdentity.Structural
                fun a ->
                    let mutable b = Unchecked.defaultof<_>
                    if d.TryGetValue(a,&b) then b
                    else
                        b <- f a
                        d.Add(a,b)
                        b
            let memoize (f:'a->'b) =
                let d = MapSlim()
                fun a ->
                    let mutable isNew = false
                    let b = &d.GetRef(a, &isNew)
                    if isNew then b <- f a
                    b
            test "memoize" {
                let! n = Gen.int.[100..5000]
                Test.faster
                    (fun () ->
                        let times2 = memoize ((*)2)
                        for i = 0 to n-1 do
                            times2 i |> ignore
                    )
                    (fun () ->
                        let times2 = memoizeOld ((*)2)
                        for i = 0 to n-1 do
                            times2 i |> ignore
                    )
                    "memoize"
            }
        }
    }

let all =
    test "utils" {
        pcg
        array
        mapslim
    }