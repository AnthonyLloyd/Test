module GenTest

open System

let all =
    test "gen" {
        test "bound" {
            let! bound = Gen.uint
            let threshold = uint32(-int bound) % bound
            Test.equal (threshold-1ul) (UInt32.MaxValue % bound) "remainder"
        }
        test "int" {
            let! s = Gen.int
            let! l = Gen.int.[0..Int32.MaxValue-s]
            let! i = Gen.int.[s..s+l]
            Test.between i s (s+l) "between"
        }
        test "int distribution" {
            let freq = 10
            let buckets = 1000
            let! ints = Gen.seq.[freq * buckets] Gen.int.[0..buckets-1]
            let actual = Array.zeroCreate buckets
            Seq.iter (fun i -> actual.[i] <- actual.[i] + 1) ints
            let expected = Array.create buckets freq
            Test.chiSquared actual expected "chi-squared"
        }
        test "float" {
            let! f = Gen.float
            Test.between f 0.0 1.0 "unit range"
        }
        test "float distribution" {
            let freq = 10
            let buckets = 1000
            let! floats = Gen.seq.[freq * buckets] Gen.float.[*]
            let actual = Array.zeroCreate buckets
            Seq.iter (fun f ->
                let i = int(f * float buckets)
                actual.[i] <- actual.[i] + 1) floats
            let expected = Array.create buckets freq
            Test.chiSquared actual expected "chi-squared"
        }
        test "list rev does nothing not" {
            let! list =
                let f ma mi bu = Version (int ma,int mi,int bu)
                Gen.map3 f Gen.byte Gen.byte Gen.byte
                |> Gen.list.[0..100]
            Test.equal (List.rev list) list "rev equal"
        }
    }