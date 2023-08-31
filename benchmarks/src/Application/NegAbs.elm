module Application.NegAbs exposing (composeR, declarationArgumentPipeline, lambdaPipeComposeR, lambdaPipeline)


declarationArgumentPipeline : number -> number
declarationArgumentPipeline n =
    n |> abs |> negate


lambdaPipeComposeR : number -> number
lambdaPipeComposeR =
    \n -> n |> abs >> negate


lambdaPipeline : number -> number
lambdaPipeline =
    \n -> n |> abs |> negate


composeR : number -> number
composeR =
    abs >> negate
