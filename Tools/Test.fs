[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Test

open System.Diagnostics

let info fmt =
    let sb = System.Text.StringBuilder()
    Printf.kbprintf (fun () -> sb.ToString() |> TestResult.Info) sb fmt

let label s = Label s

let isTrue (actual:bool) message =
    if actual then Success
    else Failure(Message message + "\n     Actual is false.")

let isFalse (actual:bool) message =
    if actual then Failure(Message message + "\n     Actual is true.")
    else Success

let lessThan (actual:'a) (expected:'a) message =
    if actual < expected then Success
    else
        let a = (sprintf "%A" actual).Replace("\n","")
        let e = (sprintf "%A" expected).Replace("\n","")
        Failure(Message message + "\n     actual is not less than expected\n     actual: " + Numeric a + "\n   expected: " + Numeric e)

let equal (actual:'a) (expected:'a) message =
    match box actual, box expected with
    | a,e ->
        if a=e then Success
        else
            let a = (sprintf "%A" actual).Replace("\n","")
            let e = (sprintf "%A" expected).Replace("\n","")
            Failure(Message message + "\n     actual: " + Numeric a + "\n   expected: " + Numeric e)

let between (actual:'a) (startInclusive:'a) (endInclusive:'a) message =
    if actual < startInclusive then
        Failure(Message message + "\n Actual (" + Numeric actual + ") is less than start (" + Numeric startInclusive + ").")
    elif actual > endInclusive then
        Failure(Message message + "\n Actual (" + Numeric actual + ") is greater than end (" + Numeric endInclusive + ").")
    else Success

let faster (actual:unit->'a) (expected:unit->'a) message =
    let t1 = Stopwatch.GetTimestamp()
    let aa,ta,ae,te =
        if t1 &&& 1L = 1L then
            let aa = actual()
            let t1 = Stopwatch.GetTimestamp() - t1
            let t2 = Stopwatch.GetTimestamp()
            let ae = expected()
            let t2 = Stopwatch.GetTimestamp() - t2
            aa,t1,ae,t2
        else
            let ae = expected()
            let t1 = Stopwatch.GetTimestamp() - t1
            let t2 = Stopwatch.GetTimestamp()
            let aa = actual()
            let t2 = Stopwatch.GetTimestamp() - t2
            aa,t2,ae,t1
    match equal aa ae message with
    | Success ->
        let l = ListSlim 1
        if te<>0L then l.Add (int((te-ta)*10000L/te)) |> ignore
        Faster(message,(if ta<te then 1 else 0),(if te<ta then 1 else 0),l)
    | fail -> fail

/// Chi-squared test to 6 standard deviations.
let chiSquared (actual:int[]) (expected:int[]) message =
    if actual.Length <> expected.Length then
        Failure(Message message + "\n    Actual and expected need to be the same length.")
    elif Array.exists (fun i -> i<=5) expected then
        Failure(Message message + "\n    Expected frequency for all buckets needs to be above 5.")
    else
        let chi = Array.fold2 (fun s a e ->
            let d = float(a-e)
            s+d*d/float e) 0.0 actual expected
        let mean = float(expected.Length - 1)
        let sdev = sqrt(2.0 * mean)
        let SDs = (chi - mean) / sdev
        if abs SDs > 6.0 then
            Failure(Message message + "\n    Chi-squared standard deviation = " + Numeric(SDs.ToString("0.0")))
        else Success