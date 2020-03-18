module TestTest

open System.Threading

let all =
    test "test" {
        test "no test" {
            let actual = Tests.run [NoPr] [
                test "nothing" { () }
            ]
            Test.equal actual 0 "error code"
        }
        test "pass test" {
            let actual = Tests.run [NoPr] [
                test "equal" { Test.equal 1 1 "1=1" }
            ]
            Test.equal actual 0 "error code"
        }
        test "fail test" {
            let actual = Tests.run [NoPr] [
                test "equal" { Test.equal 1 0 "1=0" }
            ]
            Test.equal actual 1 "error code"
        }
        test "exception" {
            let actual = Tests.run [NoPr] [
                test "bang" { failwith "agh" }
            ]
            Test.equal actual 1 "error code"
        }
        test "timeout" {
            use mre = new ManualResetEventSlim()
            let actual = Tests.run [NoPr;Wait 0.0001] [
                test "bang" { mre.Wait() }
            ]
            mre.Set()
            Test.equal actual 2 "error code"
        }
        test "memory" {
            use mre = new ManualResetEventSlim()
            let actual = Tests.run [NoPr;Memo 0.001] [
                test "bang" { mre.Wait() }
            ]
            mre.Set()
            Test.equal actual 3 "error code"
        }
        test "duplicates" {
            let actual = Tests.run [NoPr] [
                test "bang" { failwith "agh" }
                test "bang" { failwith "agh2" }
            ]
            Test.equal actual 4 "error code"
        }
    }