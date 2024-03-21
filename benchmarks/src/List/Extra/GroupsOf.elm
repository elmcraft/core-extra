module List.Extra.GroupsOf exposing (coreTailGroupsOfWithStep, coreTailGreedyGroupsOfWithStep, tailRecGroupsOfWithStep, tailRecGreedyGroupsOfWithStep)

import Benchmark
import Benchmark.Runner.Alternative as BenchmarkRunner


coreTailGroupsOfWithStep : Int -> Int -> List a -> List (List a)
coreTailGroupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    let
                        thisGroup =
                            List.take size xs
                    in
                    if size == List.length thisGroup then
                        let
                            rest =
                                List.drop step xs
                        in
                        go rest (thisGroup :: acc)

                    else
                        List.reverse acc
        in
        go list []


coreTailGreedyGroupsOfWithStep : Int -> Int -> List a -> List (List a)
coreTailGreedyGroupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    go
                        (List.drop step xs)
                        (List.take size xs :: acc)
        in
        go list []


tailRecGroupsOfWithStep : Int -> Int -> List a -> List (List a)
tailRecGroupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    let
                        thisGroup =
                            takeTailRec size xs
                    in
                    if size == List.length thisGroup then
                        let
                            rest =
                                List.drop step xs
                        in
                        go rest (thisGroup :: acc)

                    else
                        List.reverse acc
        in
        go list []


tailRecGreedyGroupsOfWithStep : Int -> Int -> List a -> List (List a)
tailRecGreedyGroupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    go
                        (List.drop step xs)
                        (takeTailRec size xs :: acc)
        in
        go list []


takeTailRec : Int -> List a -> List a
takeTailRec n list =
    List.reverse (takeReverse n list [])


takeReverse : Int -> List a -> List a -> List a
takeReverse n list kept =
    if n <= 0 then
        kept

    else
        case list of
            [] ->
                kept

            x :: xs ->
                takeReverse (n - 1) xs (x :: kept)
