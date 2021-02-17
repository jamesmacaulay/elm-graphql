module GraphQL.Request.Builder.Variable.Util exposing (mergeVariables)

import GraphQL.Request.Builder.Variable as Variable exposing (Variable)


mergeVariables : List (Variable source) -> List (Variable source) -> List (Variable source)
mergeVariables varsA varsB =
    varsA ++ (varsB |> List.filter (variableIsNotInList varsA))


variableIsNotInList : List (Variable source) -> Variable source -> Bool
variableIsNotInList existingVars thisVar =
    let
        thisVarAST =
            Variable.toDefinitionAST thisVar

        sameASTAsThisVar var =
            Variable.toDefinitionAST var == thisVarAST
    in
    not (List.any sameASTAsThisVar existingVars)
