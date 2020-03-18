namespace global

open System
open System.Threading
open System.Diagnostics
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type PCG =
    val Inc : uint64
    val mutable State : uint64
    private new(inc:uint64,state:uint64) = { Inc = inc; State = state }
    new(stream:int,seed:uint64) =
        let inc = (uint64 stream <<< 1) ||| 1UL
        PCG(inc, (inc + seed) * 6364136223846793005UL + inc)
    new(stream:int) =
        PCG(stream, uint64(Stopwatch.GetTimestamp()))
    member i.Stream = int(i.Inc >>> 1)
    member i.ToString(state:uint64) =
        sprintf "%x%016x" (i.Inc >>> 1) state
    override i.ToString() =
        sprintf "%x%016x" (i.Inc >>> 1) i.State
    static member TryParse(s:string) =
        let l = s.Length
        let mutable stream = Unchecked.defaultof<int>
        let mutable state = Unchecked.defaultof<uint64>
        if  l>=17
         && Int32.TryParse(s.Substring(0,l-16), NumberStyles.HexNumber, null, &stream)
         && UInt64.TryParse(s.Substring(l-16,16), NumberStyles.HexNumber, null, &state) then
            Some(PCG((uint64 stream <<< 1) ||| 1UL,state))
        else None
    member p.Next() =
        let oldstate = p.State
        p.State <- oldstate * 6364136223846793005UL + p.Inc
        let xorshifted = uint32(((oldstate >>> 18) ^^^ oldstate) >>> 27)
        let rot = int(oldstate >>> 59)
        uint32((xorshifted >>> rot) ||| (xorshifted <<< (-rot &&& 31)))
    member p.Next64() =
        (uint64 (p.Next()) <<< 32) + uint64 (p.Next())
    member p.Next(maxExclusive:int) =
        let bound = uint32 maxExclusive
        let threshold = uint32(-maxExclusive) % bound
        let rec find() =
            let r = p.Next()
            if r >= threshold then int(r % bound)
            else find()
        find()
    member p.Next64(maxExclusive:int64) =
        let bound = uint64 maxExclusive
        let threshold = uint64(-maxExclusive) % bound
        let rec find() =
            let r = p.Next64()
            if r >= threshold then int64(r % bound)
            else find()
        find()

module private Result =
    let traverse f list =
        List.fold (fun s i ->
            match s,f i with
            | Ok l, Ok h -> Ok (h::l)
            | Error l, Ok _ -> Error l
            | Ok _, Error e -> Error [e]
            | Error l, Error h -> Error (h::l)
        ) (Ok []) list
    let sequence list = traverse id list

type Text =
    | For of string
    | Grey of string
    | Red of string
    | BrightRed of string
    | Green of string
    | BrightGreen of string
    | Yellow of string
    | BrightYellow of string
    | Blue of string
    | BrightBlue of string
    | Magenta of string
    | BrightMagenta of string
    | Cyan of string
    | BrightCyan of string
    | Text of struct (Text * Text)
    static member (+)(t1,t2) = Text(t1,t2)

module Text =
    [<Literal>]
    let private reset = "\u001b[0m"
    let rec private toANSIList t =
        match t with
        | For s -> [s]
        | Grey s -> ["\u001b[30;1m";s;reset]
        | Red s -> ["\u001b[31m";s;reset]
        | BrightRed s -> ["\u001b[31;1m";s;reset]
        | Green s -> ["\u001b[32m";s;reset]
        | BrightGreen s -> ["\u001b[32;1m";s;reset]
        | Yellow s -> ["\u001b[33m";s;reset]
        | BrightYellow s -> ["\u001b[33;1m";s;reset]
        | Blue s -> ["\u001b[34m";s;reset]
        | BrightBlue s -> ["\u001b[34;1m";s;reset]
        | Magenta s -> ["\u001b[35m";s;reset]
        | BrightMagenta s -> ["\u001b[35;1m";s;reset]
        | Cyan s -> ["\u001b[36m";s;reset]
        | BrightCyan s -> ["\u001b[36;1m";s;reset]
        | Text(t1,t2) -> toANSIList t1 @ toANSIList t2
    let toANSI t = toANSIList t |> String.Concat

type ListSlim<'k> =
    val mutable private count : int
    val mutable private entries : 'k[]
    new() = {count=0; entries=Array.empty}
    new(capacity:int) = {count = 0; entries = Array.zeroCreate capacity}

    member m.Count = m.count

    member m.Item
        with get i = m.entries.[i]
        and set i v = m.entries.[i] <- v

    member m.Add(key:'k) =
        let i = m.count
        if i = m.entries.Length then
            if i = 0 then
                m.entries <- Array.zeroCreate 4
            else
                let newEntries = i * 2 |> Array.zeroCreate
                Array.Copy(m.entries, 0, newEntries, 0, i)
                m.entries <- newEntries
        m.entries.[i] <- key
        m.count <- i+1
        i

    member m.ToArray() =
        Array.init m.count (Array.get m.entries)

    member m.ToList() =
        List.init m.count (Array.get m.entries)

[<Struct>]
type private Entry<'k,'v> =
    val mutable bucket : int
    val mutable key : 'k
    val mutable value : 'v
    val mutable next : int

type private InitialHolder<'k,'v>() =
    static let initial = Array.zeroCreate<Entry<'k,'v>> 1
    static member inline Initial = initial

type MapSlim<'k,'v when 'k : equality and 'k :> IEquatable<'k>> =
    val mutable private count : int
    val mutable private entries : Entry<'k,'v>[]
    new() = {count=0; entries=InitialHolder.Initial}
    new(capacity:int) = {
        count = 0
        entries =
            let inline powerOf2 v =
                if v &&& (v-1) = 0 then v
                else
                    let rec twos i =
                        if i>=v then i
                        else twos (i*2)
                    twos 2
            powerOf2 capacity |> Array.zeroCreate
    }

    member m.Count = m.count

    member private m.Resize() =
        let oldEntries = m.entries
        let entries = Array.zeroCreate<Entry<_,_>> (oldEntries.Length*2)
        for i = oldEntries.Length-1 downto 0 do
            entries.[i].value <- oldEntries.[i].value
            entries.[i].key <- oldEntries.[i].key
            let bi = entries.[i].key.GetHashCode() &&& (entries.Length-1)
            entries.[i].next <- entries.[bi].bucket-1
            entries.[bi].bucket <- i+1
        m.entries <- entries

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member private m.AddKey(key:'k, hashCode:int) =
        let i = m.count
        if i = 0 && m.entries.Length = 1 then
            m.entries <- Array.zeroCreate 2
        elif i = m.entries.Length then m.Resize()
        let entries = m.entries
        entries.[i].key <- key
        let bucketIndex = hashCode &&& (entries.Length-1)
        entries.[i].next <- entries.[bucketIndex].bucket-1
        entries.[bucketIndex].bucket <- i+1
        m.count <- i+1
        &entries.[i].value

    member m.Set(key:'k, value:'v) =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then entries.[i].value <- value
        else
            let v = &m.AddKey(key, hashCode)
            v <- value

    member m.GetRef(key:'k) : 'v byref =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do // check >= in IL
            i <- entries.[i].next
        if i >= 0 then &entries.[i].value
        else &m.AddKey(key, hashCode)

    member m.GetRef(key:'k, added: bool outref) : 'v byref =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then
            added <- false
            &entries.[i].value
        else
            added <- true
            &m.AddKey(key, hashCode)

    member m.GetOption (key:'k) : 'v voption =
        let entries = m.entries
        let mutable i = entries.[key.GetHashCode() &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then ValueSome entries.[i].value
        else ValueNone

    member m.Item i : 'k * 'v =
        let entries = m.entries.[i]
        entries.key, entries.value

    member m.Key i : 'k =
        m.entries.[i].key

[<AllowNullLiteral>]
type Size =
    val I : uint64
    val L : Size list
    new(i,l) = {I=i;L=l}
    override x.Equals y =
        x.I = (y :?> Size).I && x.L = (y :?> Size).L
    override x.GetHashCode() =
        int x.I
    interface IComparable<Size> with
        member x.CompareTo y =
            let rec compare (x:Size) (y:Size) =
                let i = int64 x.I - int64 y.I
                if i <> 0L then i
                else List.fold2 (fun s x y -> s + compare x y) 0L x.L y.L
            compare x y |> sign
    interface IComparable with
        member x.CompareTo y =
            (x :> IComparable<Size>).CompareTo (y :?> Size)
    static member zero = Size(0UL,[])

type Gen<'a> = abstract member Gen : PCG -> 'a * Size

type GenRange<'a,'b> =
    inherit Gen<'b>
    abstract member GetSlice : 'a option * 'a option -> Gen<'b>

type GenBuilder() =
    member inline _.Zero() =
        let g = (), Size.zero
        { new Gen<_> with member _.Gen _ = g }
    member inline _.Bind(ga:Gen<'a>,f:'a->Gen<'b>) =
        { new Gen<_> with
            member _.Gen r =
                let gb = ga.Gen r |> fst |> f
                gb.Gen r
        }

type TestText =
    | Normal of string
    | Minor of string
    | TestName of string
    | Message of string
    | Alert of string
    | Numeric of obj
    | TestText of struct (TestText * TestText)
    static member (+)(t1,t2) = TestText (t1,t2)
    static member (+)(s:string,t) = TestText(Normal s,t)
    static member (+)(t,s:string) = TestText(t,Normal s)

module TestText =
    let rec toText t =
        match t with
        | Normal s -> For s
        | Minor s -> Grey s
        | TestName s -> BrightGreen s
        | Message s -> BrightYellow s
        | Alert s -> BrightRed s
        | Numeric o ->
            match o with
            | :? int as i -> i.ToString("#,##0")
            | :? int64 as i -> i.ToString("#,##0")
            | o -> string o
            |> BrightCyan
        | TestText(t1,t2) -> Text(toText t1,toText t2)

type TestResult =
    | Success
    | Failure of TestText
    | Exception of exn
    | Info of string
    | Label of string
    | Faster of string * int * int * ListSlim<int>
    static member hasErrs (r:TestResult list) =
        List.exists (function | Failure _ | Exception _ -> true | _ -> false) r

[<Struct>]
type Test = private Test of string * (PCG->(TestResult list option->unit)->unit)

type TestBuilder(name:string) =
    let mutable s_mins = MapSlim<int,Size>()
    let mutable delayed = false
    let zero (_:PCG) (c:TestResult list option->unit) = c(Some[])
    member _.Zero() =
        Test(name, zero)
    member _.Yield (a:TestResult) =
        if a=Success then Test(name, zero)
        else Test(name, fun _ c -> c(Some [a]))
    member _.Yield(Test(n,f)) =
        [Test(name+"/"+n,f)]
    member _.Yield (l:Test list) =
        List.map (fun (Test(n,f)) -> Test(name+"/"+n,f)) l
    member _.Combine (Test(_,f1),Test(_,f2)) =
        Test(name, fun p c ->
            f1 p (function
                | Some r1 ->
                    f2 p (function
                        | Some r2 ->
                            c(Some(r1 @ r2))
                        | None -> c None
                    )
                | None -> c None
            )
        )
    member inline _.Combine (l1:Test list,l2:Test list) =
        l1 @ l2
    member inline _.Combine (l1:Test list,t2:Test) =
        l1 @ [t2]
    member _.Delay(f:unit->Test) =
        if delayed then f()
        else
            delayed <- true
            Test(name, fun p c ->
                try
                    let (Test(_,f)) = f()
                    f p c
                with e -> c(Some [Exception e])
            )
    member inline _.Delay(f:unit->Test list) =
        f()
    member inline m.Bind(t:Test,f:unit->Test) =
        m.Combine(t, m.Delay f)
    member _.Bind(g:#Gen<'a>,f:'a->Test,
      [<CallerLineNumberAttribute;Optional;DefaultParameterValue 0>]line:int) =
        Test(name, fun p c ->
            let a,s = g.Gen p
            match s_mins.GetOption line with
            | ValueSome v when s > v -> c None
            | _ ->
                let (Test(_,tf)) = f a
                tf p (function
                        | None -> c None
                        | Some r ->
                            if TestResult.hasErrs r then
                                lock s_mins (fun () ->
                                    let m = &s_mins.GetRef line
                                    if isNull m || s <= m then
                                        m <- s
                                        Some r
                                    else None
                                ) |> c
                            else c(Some r)
                )
        )
    member m.While(guard, body:unit->Test) =
        if guard() then m.Bind(body(), fun () -> m.While(guard, body))
        else m.Zero()
    member _.TryFinally(body, compensation) =
        try body()
        finally compensation()
    member m.Using(d:#IDisposable, body) =
        m.TryFinally((fun () -> body d), fun () -> d.Dispose())
    member m.For(s:seq<'a>,body:'a->Test) =
        let e = s.GetEnumerator()
        m.While(e.MoveNext, fun () -> body e.Current)

[<AutoOpen>]
module TestAutoOpen =
    let test name = TestBuilder name
    let gen = GenBuilder()

type Config =
    | Filt of string list
    | Para of int
    | Seed of PCG
    | Iter of int
    | Time of float
    | Memo of float
    | Wait of float
    | Info
    | Skip
    | Stop
    | NoSt
    | NoPr

module Config =

    type private Parser<'a> = (string[] * int * int) -> Result<'a,string> * int

    let inline private none case : Parser<_> =
        fun (_,_,l) -> Ok case, l

    let inline private string case : Parser<_> =
        fun (ss,i,l) ->
            if l>0 then Ok(case ss.[i]), l-1
            else Error "requires a parameter", 0

    let inline private list (parser:_->Parser<_>) case : Parser<_> =
        fun (ss,i,l) ->
            [i..i+l-1]
            |> Result.traverse (fun j -> parser id (ss,j,1) |> fst)
            |> Result.map (fun l -> case(List.rev l))
            |> Result.mapError (fun i -> String.Join(", ", i))
            , 0

    let inline private parseWith tryParseFn case: Parser<'a> =
        fun (args, i, l) ->
            if l = 0 then Error "requires a parameter", 0
            else
                match tryParseFn args.[i] with
                | Some i -> Ok(case i), l-1
                | None -> Error("Cannot parse parameter '" + args.[i] + "'"), l-1

    let inline tryParse (s: string) =
        let mutable r = Unchecked.defaultof<_>
        if (^a : (static member TryParse: string * ^a byref -> bool) (s, &r))
        then Some r else None

    let inline private tryParseNumber (s: string) =
        let mutable r = Unchecked.defaultof<_>
        if (^a : (static member TryParse: string * NumberStyles * IFormatProvider * ^a byref -> bool)
                                            (s, NumberStyles.Any, CultureInfo.InvariantCulture, &r))
        then Some r else None

    let inline private number case: Parser<'a> = parseWith tryParseNumber case

    let private options = [
        "--filt", "Test name filters.", list string Filt
        "--para", "Number of parallel threads.", number Para
        "--seed", "First thread starts with this seed.", parseWith PCG.TryParse Seed
        "--iter", "Run tests randomly for this number of iterations (defaults to 1).", number Iter
        "--time", "Run tests randomly for this time in minutes.", number Time
        "--memo", "Memory limit in GB (defaults to 100 MB).", number Memo
        "--wait", "Wait up to this number of minutes after last test started before reporting a timeout.", number Wait
        "--info", "Include info messages in the output.", none Info
        "--skip", "Skip failed random, passed none random and passed faster tests.", none Skip
        "--stop", "Stop on first failure.", none Stop
        "--nost", "No updating progress status.", none NoSt
        "--nopr", "No console output.", none NoPr
    ]

    let parse (strings:string[]) =
        let strings = if strings.Length > 0 && strings.[0].StartsWith("--") then strings
                      else Array.append [|"--filt"|] strings
        let rec updateUnknown unknown last length =
            if length = 0 then unknown
            else updateUnknown (strings.[last]::unknown) (last-1) (length-1)
        let rec collect isHelp unknown args paramCount i =
            if i>=0 then
                let currentArg = strings.[i]
                if currentArg = "--help" || currentArg = "-h" || currentArg = "-?" || currentArg = "/?" then
                    collect true (updateUnknown unknown (i+paramCount) paramCount) args 0 (i-1)
                else
                    match List.tryFind (fun (i,_,_) -> i = currentArg) options with
                    | Some (option, _, parser) ->
                        let arg, unknownCount = parser (strings, i+1, paramCount)
                        collect isHelp
                            (updateUnknown unknown (i+paramCount) unknownCount)
                            (Result.mapError (fun i -> option + " " + i) arg::args) 0 (i-1)
                    | None -> collect isHelp unknown args (paramCount+1) (i-1)
            else
                let unknown =
                    match updateUnknown unknown (paramCount-1) paramCount with
                    | [] -> None
                    | l -> String.Join(" ","unknown options:" :: l) |> Some
                match isHelp, Result.sequence args, unknown with
                | false, Ok os, None -> Ok(List.rev os)
                | true, Ok _, None -> Error []
                | _, Ok _, Some u -> Error [u]
                | _, r, None -> r
                | _, Error es, Some u -> List.rev (u::es) |> Error
        collect false [] [] 0 (strings.Length-1)

    let usage commandName =
        let sb = Text.StringBuilder("Usage: ")
        let add (text:string) = sb.Append(text) |> ignore
        add commandName
        add " [options]\n\nOptions:\n"
        let maxLength =
            options |> Seq.map (fun (s,_,_) -> s.Length) |> Seq.max
        ["--help","Show this help message."]
        |> Seq.append (Seq.map (fun (s,d,_) -> s,d) options)
        |> Seq.iter (fun (s,d) ->
            add "  "
            add (s.PadRight maxLength)
            add "  "
            add d
            add "\n"
        )
        sb.ToString()

module Tests =

    let private dulicateNames tests =
        Seq.countBy (fun (Test(n,_)) -> n) tests
        |> Seq.choose (fun (n,c) -> if c=1 then None else Some n)
        |> Seq.toList

    let private filterTests config tests =
        let includes,excludes =
            List.collect (function | Filt f -> f | _ -> []) config
            |> List.fold (fun (i,e) s -> if s.[0]='-' then i,s.Substring 1::e else s::i,e) ([],[])
        let ts =
            if List.isEmpty includes then tests
            else List.collect (fun f -> List.where (fun (Test(n,_)) -> n.Contains f) tests) includes
                 |> List.distinctBy (fun (Test(n,_)) -> n)
        List.fold (fun l e -> List.where (fun (Test(n,_)) -> n.Contains e |> not) l) ts excludes
        |> List.map (fun (Test(n,f)) -> n,f)
        |> List.toArray
        |> Array.unzip

    let private fasterVarianceCheck (actual:int) (expected:int) =
        float(actual-expected) * float(actual-expected) / float(actual+expected)

    let private testResultWriteLine config =
        let info = List.contains Info config
        fun (name:string) (results,seed) ->
            let rows =
                List.choose (function
                    | Success | Label _ -> None
                    | Failure t -> Alert "  FAIL: " + t |> Some
                    | Exception e ->
                        Alert "  EXCN: " + Message e.Message + " " +
                        Minor(string(e.GetType())) + "\n" + string e.StackTrace |> Some
                    | TestResult.Info s -> if info then Some(Normal "  INFO: " + s) else None
                    | Faster(s,c1,c2,l) ->
                        let variance = fasterVarianceCheck c1 c2
                        let failed = c1<c2 && variance >= 36.0
                        if failed || info then
                            let l = l.ToArray()
                            Array.sortInPlace l
                            let perf = float l.[l.Length/2] * 0.01
                            if failed then Alert "  FAIL: " + Message s + " ~" + Numeric((-perf).ToString("#.00")) + "% slower"
                            else Normal "  INFO: " + Message s + " ~" + Numeric(perf.ToString("#.00")) + "% faster"
                            + ", sigma=" + Numeric(sqrt(variance).ToString("#.0")) + " ("+Numeric c1+"/"+Numeric(c1+c2)+")" |> Some
                        else None
                ) results
            if List.isEmpty rows then None
            else
                let n = if name.Contains " " then "\"" + name + "\"" else name
                let n = match seed with None -> n | Some(p:PCG,s) -> n + " --seed " + p.ToString s
                TestName n :: rows
                |> List.reduce (fun a b -> a+"\n"+b) |> Some

    let run (config:Config list) (tests:Test list) =
        let mutable lastFinished = List.isEmpty tests
        let print,status =
            let inline toStr t = TestText.toText t |> Text.toANSI
            if List.contains NoPr config then ignore,ignore
            elif List.contains NoSt config then
                (toStr >> Console.WriteLine),ignore
            else let clear = "      \u001b[1000D\u001b[?25h"
                 Console.CancelKeyPress
                 |> Event.add (fun a ->
                    Console.Write clear
                    lastFinished <- true
                    a.Cancel <- true
                 )
                 fun (t:TestText) -> clear + t |> toStr |> Console.WriteLine
                 ,fun (t:TestText) -> t + "\u001b[1000D\u001b[?25l"
                                      |> toStr |> Console.Write

        match dulicateNames tests with
        | [] ->
            let allTestCount = List.length tests
            let testNames, tests = filterTests config tests
            lastFinished <- tests.Length=0
            let results = Array.zeroCreate tests.Length
            let skipTest = Array.zeroCreate<bool> tests.Length
            let successTests = ref 0
            let erroredTests = ref 0
            let skippedTests = ref 0
            let mutable timeout = Int64.MaxValue
            let skip = List.contains Skip config
            let runner (pcg:PCG) (nextTest:unit->int option) =
                let rec run stackReset =
                    match nextTest() with
                    | None -> ()
                    | Some i ->
                        if Array.get skipTest i then
                            Interlocked.Increment skippedTests |> ignore
                            if stackReset=0 then ThreadPool.UnsafeQueueUserWorkItem((fun _ -> run 10), null) |> ignore
                            else run (stackReset-1)
                        else
                            let f = tests.[i]
                            let stateBefore = pcg.State
                            f pcg (fun r ->
                                match r with
                                | None -> Interlocked.Increment skippedTests |> ignore
                                | Some r -> lock f (fun () ->
                                    let seed = if pcg.State=stateBefore then None else Some(pcg,stateBefore)
                                    if TestResult.hasErrs r then
                                        if skip || Option.isNone seed then Array.set skipTest i true
                                        Array.set results i (Some(r,seed))
                                        Interlocked.Increment erroredTests |> ignore
                                    else
                                        match Array.get results i with
                                        | None ->
                                            if skip && Option.isNone seed then Array.set skipTest i true
                                            Array.set results i (Some(r,seed))
                                            Interlocked.Increment successTests |> ignore
                                        | Some (pr,_) ->
                                            match List.tryPick (function Faster(m,t1,t2,rs) -> Some(m,t1,t2,rs) | _ -> None) r with
                                            | None -> Interlocked.Increment successTests |> ignore
                                            | Some (m,t1,t2,rs) ->
                                                let pt1,pt2,prs = List.pick (function Faster(pm,pt1,pt2,prs) when pm=m -> Some(pt1,pt2,prs) | _ -> None) pr
                                                prs.Add rs.[0] |> ignore
                                                let f = Faster(m,pt1+t1,pt2+t2,prs)
                                                let r = List.map (function Faster (n,_,_,_) when n=m -> f | i -> i) r
                                                if fasterVarianceCheck (pt1+t1) (pt2+t2) < 36.0 then
                                                    Interlocked.Increment successTests |> ignore
                                                elif pt1+t1<pt2+t2 then
                                                    Array.set skipTest i true
                                                    Interlocked.Increment erroredTests |> ignore
                                                else
                                                    if skip then Array.set skipTest i true
                                                    Interlocked.Increment successTests |> ignore
                                                Array.set results i (Some(r,None))
                                )
                                if stackReset=0 then ThreadPool.UnsafeQueueUserWorkItem((fun _ -> run 10), null) |> ignore
                                else run (stackReset-1)
                            )
                ThreadPool.UnsafeQueueUserWorkItem((fun _ -> run 10), null) |> ignore

            let wait = List.tryPick (function Wait t -> Some t | _ -> None) config
                       |> Option.defaultValue 1.0
                       |> (*) (60.0 * float Stopwatch.Frequency) |> int64
            let seed = List.tryPick (function Seed s -> Some s | _ -> None) config
            let startTime = Stopwatch.GetTimestamp()
            let running,progress =
                match List.tryPick (function Time t -> Some t | _ -> None) config with
                | Some t -> // Time
                    let endTime =
                        startTime + int64(t * 60.0 * float Stopwatch.Frequency)
                    timeout <- endTime + wait
                    let threads = List.tryPick (function Para s -> Some s | _ -> None) config
                                  |> Option.defaultWith Environment.get_ProcessorCount
                    let running = Array.zeroCreate threads
                    "Running " + Numeric tests.Length + " (out of " +
                    Numeric allTestCount + ") tests for " + Numeric(int(t * 60.0))
                    + " seconds on " + Numeric threads + " threads." |> print
                    let threadsRunning = ref threads
                    for i = 0 to threads-1 do
                        let pcg = PCG(i+threads)
                        runner (match i,seed with 0,Some pcg -> pcg | i,_ -> PCG i)
                            (fun () ->
                                if Stopwatch.GetTimestamp() > endTime then
                                    if Interlocked.Decrement threadsRunning = 0 then
                                        lastFinished <- true
                                    running.[i] <- None
                                    None
                                else
                                    let t = pcg.Next tests.Length
                                    running.[i] <- Some t
                                    Some t )
                    running, fun now -> 100L * (now - startTime) / (endTime - startTime) |> int
                | None -> // Iter
                    let iters =
                        List.tryPick (function Iter s -> Some s | _ -> None) config
                        |> Option.defaultValue 1
                    let testRunCount = Array.zeroCreate<int> tests.Length
                    let threads =
                        List.tryPick (function Para s -> Some s | _ -> None) config
                        |> Option.defaultWith Environment.get_ProcessorCount
                        |> min (tests.Length * iters)
                    let running = Array.zeroCreate threads
                    "Running " + Numeric tests.Length + " (out of " +
                    Numeric allTestCount + ") tests for " + Numeric iters +
                    " iterations on " + Numeric threads + " threads." |> print
                    let testsLeft = ref (tests.Length * iters)
                    let threadsRunning = ref threads
                    for i = 0 to threads-1 do
                        let pcg = PCG(i+threads)
                        runner (match i,seed with 0,Some pcg -> pcg | i,_ -> PCG i)
                            (fun () ->
                                let tl = Interlocked.Decrement testsLeft
                                if tl < 0 then
                                    if Interlocked.Decrement threadsRunning = 0 then
                                        lastFinished <- true
                                    running.[i] <- None
                                    None
                                else
                                    if tl = 0 then timeout <- Stopwatch.GetTimestamp()+wait
                                    let rec incRunCount t =
                                        if Interlocked.Increment(&testRunCount.[t]) <= iters then t
                                        else incRunCount (if t+1 = tests.Length then 0 else t+1)
                                    let t = pcg.Next tests.Length |> incRunCount
                                    running.[i] <- Some t
                                    Some t )
                    running, fun _ -> (1.0 - float !testsLeft / float(tests.Length*iters)) * 100.0 |> int

            let memoryLimit = List.tryPick (function Memo l -> Some l | _ -> None) config
                              |> Option.defaultValue 100.0 |> (*) (1024.0 * 1024.0) |> int64
            let doStatus = List.contains NoSt config |> not
            let stopOnError = List.contains Stop config
            let rec monitor maxMemory =
                let timeTaken = int((Stopwatch.GetTimestamp() - startTime) / Stopwatch.Frequency)
                if lastFinished || (stopOnError && !erroredTests <> 0) then
                    Array.iteri (fun i r -> match r with
                                            | None -> ()
                                            | Some r -> testResultWriteLine config testNames.[i] r
                                                        |> Option.iter print) results
                    "Maximum memory usage was " + Numeric(maxMemory/1024L) + " KB (limit set at " +
                    Numeric(memoryLimit/1024L) + " KB).\n" + Numeric(!successTests + !erroredTests + !skippedTests) +
                    " tests run in " + Numeric timeTaken + " seconds: " + Numeric !successTests +
                    " passed, " + Numeric !erroredTests + " failed, " + Numeric !skippedTests + " skipped. " +
                    (if !erroredTests = 0 then TestName "Success!" else Alert "Failure!") |> print
                    if !erroredTests = 0 then 0 else 1
                else
                    let now = Stopwatch.GetTimestamp()
                    if now > timeout then
                        Alert "Timeout!" + Message "\nTests running:" |> print
                        Array.iteri (fun i r -> match r with
                                                | None -> ()
                                                | Some r -> testResultWriteLine config testNames.[i] r
                                                            |> Option.iter print) results
                        Array.iteri (fun i ->
                            Option.iter (fun t ->
                                let th = i.ToString().PadLeft(3)
                                "Thread " + Numeric th + ": " + TestName testNames.[t]
                                |> print
                            )
                        ) running
                        2
                    else
                        let mem = GC.GetTotalMemory false
                        if mem > memoryLimit then
                            Array.iteri (fun i r -> match r with
                                                    | None -> ()
                                                    | Some r -> testResultWriteLine config testNames.[i] r
                                                                |> Option.iter print) results
                            Alert "Memory limited exceeded: " + Numeric(mem/1024L) +
                            " KB (limit set at " + Numeric(memoryLimit/1024L) + " KB)." +
                            Message "\nTests running:" |> print
                            Array.iteri (fun i ->
                                Option.iter (fun t ->
                                    let th = i.ToString().PadLeft(3)
                                    "Thread " + Numeric th + ": " + TestName testNames.[t]
                                    |> print
                                )
                            ) running
                            3
                        else
                            if doStatus then
                                let percent = string(progress now).PadLeft(3)
                                let a = match int(now / (Stopwatch.Frequency / 4L)) % 4 with
                                        | 0 -> "% |" | 1 -> "% /" | 2 -> "% -" | _ -> "% \\"
                                Numeric percent + Minor a |> status
                            Thread.Sleep 250
                            monitor (max maxMemory mem)
            monitor 0L
        | dups ->
            List.iter (fun n -> Alert "Duplicate test name: " + TestName n |> print) dups
            4