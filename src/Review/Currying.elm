module Review.Currying exposing (forbid)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Type
import Review.ModuleNameLookupTable
import Review.Project.Dependency
import Review.Rule exposing (withDeclarationListVisitor)
import Set exposing (Set)


{-| Reports any curried function call:


### reported

    justs =
        List.filterMap identity


### allowed

    justs list =
        list |> List.filterMap identity

-}
forbid : Review.Rule.Rule
forbid =
    Review.Rule.newProjectRuleSchema "Review.Currying" initialProjectState
        |> Review.Rule.withDirectDependenciesProjectVisitor
            (\directDependencies state ->
                ( []
                , { introducedFunctions =
                        directDependencies
                            |> Dict.foldl
                                (\_ dependency soFarAcrossDependencies ->
                                    dependency
                                        |> Review.Project.Dependency.modules
                                        |> List.foldl
                                            (\moduleInterface withModulesSoFar ->
                                                Dict.insert moduleInterface.name
                                                    (moduleInterface |> moduleInterfaceIntroducedFunctions)
                                                    withModulesSoFar
                                            )
                                            soFarAcrossDependencies
                                )
                                state.introducedFunctions
                  }
                )
            )
        |> Review.Rule.withModuleVisitor
            (\schema ->
                schema
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarations state ->
                            ( [], state |> moduleDeclarationsVisitor declarations )
                        )
                    |> Review.Rule.withDeclarationExitVisitor
                        (\_ state ->
                            ( [], { state | rangesToIgnore = [] } )
                        )
                    |> Review.Rule.withExpressionEnterVisitor expressionEnterVisitor
                    |> Review.Rule.withExpressionExitVisitor
                        (\expressionNode state ->
                            ( [], state |> expressionExitVisitor expressionNode )
                        )
            )
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = initializeModuleStateWithProject
            , fromModuleToProject = moduleStateAsProject
            , foldProjectContexts = projectStateMerge
            }
        |> Review.Rule.fromProjectRuleSchema


type alias ProjectState =
    { introducedFunctions :
        Dict
            -- module name
            String
            (Dict String (List String))
    }


type alias ModuleState =
    { moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , importIntroducedFunctions :
        Dict
            -- module name
            String
            (Dict String (List String))
    , introducedFunctions : Dict String (List String)
    , letIntroducedFunctions : Dict String (List String)
    , rangesToIgnore : List Elm.Syntax.Range.Range
    }


initialProjectState : ProjectState
initialProjectState =
    { introducedFunctions = Dict.empty
    }


initializeModuleStateWithProject : Review.Rule.ContextCreator ProjectState ModuleState
initializeModuleStateWithProject =
    Review.Rule.initContextCreator
        (\moduleOriginLookup projectState ->
            { importIntroducedFunctions = projectState.introducedFunctions
            , moduleOriginLookup = moduleOriginLookup
            , introducedFunctions = Dict.empty
            , letIntroducedFunctions = Dict.empty
            , rangesToIgnore = []
            }
        )
        |> Review.Rule.withModuleNameLookupTable


moduleStateAsProject : Review.Rule.ContextCreator ModuleState ProjectState
moduleStateAsProject =
    Review.Rule.initContextCreator
        (\syntaxModuleName moduleState ->
            let
                moduleName : String
                moduleName =
                    String.join "." syntaxModuleName
            in
            { introducedFunctions =
                Dict.singleton moduleName
                    moduleState.introducedFunctions
            }
        )
        |> Review.Rule.withModuleName


projectStateMerge : ProjectState -> ProjectState -> ProjectState
projectStateMerge new soFar =
    { introducedFunctions =
        Dict.union
            new.introducedFunctions
            soFar.introducedFunctions
    }


moduleInterfaceIntroducedFunctions : Elm.Docs.Module -> Dict String (List String)
moduleInterfaceIntroducedFunctions moduleInterface =
    let
        choiceTypeIntroducedFunctions : Dict String (List String)
        choiceTypeIntroducedFunctions =
            moduleInterface.unions
                |> List.foldl
                    (\choiceTypeInterface soFar ->
                        choiceTypeInterface.tags
                            |> List.foldl
                                (\( _, variantValues ) withVariantsSoFar ->
                                    case variantValues of
                                        (_ :: _ :: _) as parameterTypes ->
                                            withVariantsSoFar
                                                |> Dict.insert choiceTypeInterface.name
                                                    (parameterTypes
                                                        |> listToDisambiguatedVariables typeInterfaceToVariable
                                                    )

                                        -- only 0-1 parameters,
                                        -- can never be partially applied
                                        _ ->
                                            withVariantsSoFar
                                )
                                soFar
                    )
                    Dict.empty

        choiceTypeAndTypeAliasIntroducedFunctions : Dict String (List String)
        choiceTypeAndTypeAliasIntroducedFunctions =
            moduleInterface.aliases
                |> List.foldl
                    (\typeAliasInterface soFar ->
                        case typeAliasInterface.tipe |> typeInterfaceToUnparenthesized of
                            Elm.Type.Record ((_ :: _ :: _) as parameterTypes) Nothing ->
                                soFar
                                    |> Dict.insert typeAliasInterface.name
                                        (parameterTypes
                                            |> List.map (\( fieldName, _ ) -> fieldName)
                                        )

                            -- not record or record with only 0-1 fields,
                            -- can never be partially applied
                            _ ->
                                soFar
                    )
                    choiceTypeIntroducedFunctions

        choiceTypeAndTypeAliasAndOperatorIntroducedFunctions : Dict String (List String)
        choiceTypeAndTypeAliasAndOperatorIntroducedFunctions =
            moduleInterface.binops
                |> List.foldl
                    (\operatorInterface soFar ->
                        case operatorInterface.tipe |> typeInterfaceExpandToFunction |> .inputs of
                            (_ :: _ :: _) as parameterTypes ->
                                soFar
                                    |> Dict.insert operatorInterface.name
                                        (parameterTypes
                                            |> List.map typeInterfaceToVariable
                                        )

                            -- only 0-1 parameters,
                            -- can never be partially applied
                            _ ->
                                soFar
                    )
                    choiceTypeAndTypeAliasIntroducedFunctions
    in
    moduleInterface.values
        |> List.foldl
            (\valueInterface soFar ->
                case valueInterface.tipe |> typeInterfaceExpandToFunction |> .inputs of
                    (_ :: _ :: _) as parameterTypes ->
                        soFar
                            |> Dict.insert valueInterface.name
                                (parameterTypes
                                    |> List.map typeInterfaceToVariable
                                )

                    -- only 0-1 parameters,
                    -- can never be partially applied
                    _ ->
                        soFar
            )
            choiceTypeAndTypeAliasAndOperatorIntroducedFunctions


typeInterfaceExpandToFunction :
    Elm.Type.Type
    -> { inputs : List Elm.Type.Type, output : Elm.Type.Type }
typeInterfaceExpandToFunction typeInterface =
    typeInterfaceExpandToFunctionAfterInputsReverse [] typeInterface


typeInterfaceExpandToFunctionAfterInputsReverse :
    List Elm.Type.Type
    -> Elm.Type.Type
    -> { inputs : List Elm.Type.Type, output : Elm.Type.Type }
typeInterfaceExpandToFunctionAfterInputsReverse inputsSoFarReverse typeInterface =
    case typeInterface of
        Elm.Type.Lambda input output ->
            typeInterfaceExpandToFunctionAfterInputsReverse (input :: inputsSoFarReverse) output

        Elm.Type.Tuple parts ->
            case parts of
                [ inParens ] ->
                    typeInterfaceExpandToFunctionAfterInputsReverse inputsSoFarReverse inParens

                _ ->
                    { inputs = inputsSoFarReverse |> List.reverse, output = typeInterface }

        Elm.Type.Var _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = typeInterface }

        Elm.Type.Type name _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = typeInterface }

        Elm.Type.Record _ _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = typeInterface }


syntaxTypeExpandToFunction :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        { inputs : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
        , output : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
syntaxTypeExpandToFunction syntaxTypeNode =
    syntaxTypeExpandToFunctionAfterInputsReverse [] syntaxTypeNode


syntaxTypeExpandToFunctionAfterInputsReverse :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        { inputs : List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
        , output : Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
        }
syntaxTypeExpandToFunctionAfterInputsReverse inputsSoFarReverse syntaxTypeNode =
    case syntaxTypeNode |> Elm.Syntax.Node.value of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputTypeNode outputTypeNode ->
            syntaxTypeExpandToFunctionAfterInputsReverse
                (inputTypeNode :: inputsSoFarReverse)
                outputTypeNode

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [ inParens ] ->
                    syntaxTypeExpandToFunctionAfterInputsReverse inputsSoFarReverse inParens

                _ ->
                    { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }

        Elm.Syntax.TypeAnnotation.Typed _ _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }

        Elm.Syntax.TypeAnnotation.Unit ->
            { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }

        Elm.Syntax.TypeAnnotation.Record _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }

        Elm.Syntax.TypeAnnotation.GenericType _ ->
            { inputs = inputsSoFarReverse |> List.reverse, output = syntaxTypeNode }


typeInterfaceToUnparenthesized : Elm.Type.Type -> Elm.Type.Type
typeInterfaceToUnparenthesized typeInterface =
    case typeInterface of
        Elm.Type.Tuple [ inParens ] ->
            typeInterfaceToUnparenthesized inParens

        _ ->
            typeInterface


syntaxTypeToUnparenthesized :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
syntaxTypeToUnparenthesized syntaxTypeNode =
    case syntaxTypeNode of
        Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Tupled [ inParens ]) ->
            syntaxTypeToUnparenthesized inParens

        _ ->
            syntaxTypeNode


variableDisambiguateFrom : List String -> String -> String
variableDisambiguateFrom alreadyUsed new =
    if new == "()" then
        "()"

    else if alreadyUsed |> List.member new then
        variableDisambiguateFrom alreadyUsed (new ++ "_")

    else
        new


listToDisambiguatedVariables : (a -> String) -> List a -> List String
listToDisambiguatedVariables parameterTypeToVariableName parameterTypes =
    parameterTypes
        |> List.foldl
            (\parameterType parameterNamesSoFar ->
                (parameterType
                    |> parameterTypeToVariableName
                    |> variableDisambiguateFrom parameterNamesSoFar
                )
                    :: parameterNamesSoFar
            )
            []


typeInterfaceToVariable : Elm.Type.Type -> String
typeInterfaceToVariable typeInterface =
    case typeInterface of
        Elm.Type.Lambda input output ->
            (input |> typeInterfaceToVariable)
                ++ "To"
                ++ stringFirstCharToUpper (output |> typeInterfaceToVariable)

        Elm.Type.Var variableName ->
            variableName

        Elm.Type.Tuple parts ->
            case parts of
                [] ->
                    "()"

                [ inParens ] ->
                    typeInterfaceToVariable inParens

                part0 :: part1 :: part2Up ->
                    typeInterfaceToVariable part0
                        ++ stringFirstCharToUpper
                            (typeInterfaceToVariable part1)
                        ++ (part2Up
                                |> List.map
                                    (\part ->
                                        stringFirstCharToUpper
                                            (typeInterfaceToVariable part)
                                    )
                                |> String.concat
                           )

        Elm.Type.Type reference _ ->
            reference |> String.replace "." "" |> stringFirstCharToLower

        Elm.Type.Record fields Nothing ->
            case fields of
                [] ->
                    "emptyRecord"

                ( field0Name, _ ) :: field1Up ->
                    field0Name
                        ++ (field1Up
                                |> List.map
                                    (\( fieldName, _ ) ->
                                        stringFirstCharToUpper fieldName
                                    )
                                |> String.concat
                           )

        Elm.Type.Record _ (Just recordVariable) ->
            recordVariable


syntaxTypeToVariable :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> String
syntaxTypeToVariable (Elm.Syntax.Node.Node _ syntaxType) =
    case syntaxType of
        Elm.Syntax.TypeAnnotation.GenericType variable ->
            variable

        Elm.Syntax.TypeAnnotation.Unit ->
            "()"

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    "()"

                [ inParens ] ->
                    syntaxTypeToVariable inParens

                part0 :: part1 :: part2Up ->
                    syntaxTypeToVariable part0
                        ++ stringFirstCharToUpper
                            (syntaxTypeToVariable part1)
                        ++ (part2Up
                                |> List.map
                                    (\part ->
                                        stringFirstCharToUpper
                                            (syntaxTypeToVariable part)
                                    )
                                |> String.concat
                           )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputTypeNode outputTypeNode ->
            (inputTypeNode |> syntaxTypeToVariable)
                ++ "To"
                ++ stringFirstCharToUpper
                    (outputTypeNode |> syntaxTypeToVariable)

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( moduleName, name )) _ ->
            (moduleName |> String.concat) ++ stringFirstCharToLower name

        Elm.Syntax.TypeAnnotation.Record fields ->
            case fields of
                [] ->
                    "emptyRecord"

                (Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ field0Name, _ )) :: field1Up ->
                    field0Name
                        ++ (field1Up
                                |> List.map
                                    (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, _ )) ->
                                        stringFirstCharToUpper fieldName
                                    )
                                |> String.concat
                           )

        Elm.Syntax.TypeAnnotation.GenericRecord (Elm.Syntax.Node.Node _ recordVariableName) _ ->
            recordVariableName


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case String.uncons string of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (headChar |> Char.toUpper) tailString


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case String.uncons string of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (headChar |> Char.toLower) tailString


moduleDeclarationsVisitor :
    List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> ModuleState
    -> ModuleState
moduleDeclarationsVisitor moduleDeclarations state =
    { state
        | introducedFunctions =
            moduleDeclarations
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                case valueOrFunctionDeclaration.signature of
                                    Nothing ->
                                        soFar

                                    Just (Elm.Syntax.Node.Node _ signature) ->
                                        case signature.typeAnnotation |> syntaxTypeExpandToFunction |> .inputs of
                                            (_ :: _ :: _) as parameterTypes ->
                                                soFar
                                                    |> Dict.insert
                                                        (signature.name |> Elm.Syntax.Node.value)
                                                        (parameterTypes
                                                            |> listToDisambiguatedVariables syntaxTypeToVariable
                                                        )

                                            -- only 0-1 parameters, can never be partially applied
                                            _ ->
                                                soFar

                            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                                case typeAliasDeclaration.typeAnnotation |> syntaxTypeToUnparenthesized of
                                    Elm.Syntax.Node.Node _ (Elm.Syntax.TypeAnnotation.Record ((_ :: _ :: _) as parameterTypes)) ->
                                        soFar
                                            |> Dict.insert
                                                (typeAliasDeclaration.name |> Elm.Syntax.Node.value)
                                                (parameterTypes
                                                    |> List.map
                                                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, _ )) ->
                                                            fieldName
                                                        )
                                                )

                                    -- not record or record with only 0-1 fields,
                                    -- meaning its constructor can never be partially applied
                                    _ ->
                                        soFar

                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                choiceTypeDeclaration.constructors
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ variant) withVariantsSoFar ->
                                            case variant.arguments of
                                                (_ :: _ :: _) as parameterTypes ->
                                                    soFar
                                                        |> Dict.insert
                                                            (variant.name |> Elm.Syntax.Node.value)
                                                            (parameterTypes
                                                                |> listToDisambiguatedVariables syntaxTypeToVariable
                                                            )

                                                -- only 0-1 parameters, cannot be partially applied
                                                _ ->
                                                    soFar
                                        )
                                        soFar

                            Elm.Syntax.Declaration.PortDeclaration portDeclaration ->
                                -- ports always have 0-1 parameters,
                                -- therefore they cannot be partially applied
                                soFar

                            Elm.Syntax.Declaration.InfixDeclaration _ ->
                                -- not possible in user land
                                soFar

                            -- invalid syntax
                            Elm.Syntax.Declaration.Destructuring _ _ ->
                                soFar
                    )
                    state.introducedFunctions
    }


expressionEnterVisitor :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> ModuleState
    -> ( List (Review.Rule.Error {}), ModuleState )
expressionEnterVisitor ((Elm.Syntax.Node.Node expressionRange _) as expressionNode) state =
    if state.rangesToIgnore |> List.head |> isJustAnd (\rangeToIgnore -> rangeToIgnore |> rangeIncludesLocation expressionRange.start) then
        ( [], state )

    else
        case expressionNode |> syntaxExpressionToApplication of
            Nothing ->
                nonApplicationExpressionEnterVisitor expressionNode
                    state

            Just application ->
                ( case application.arguments of
                    [] ->
                        []

                    (_ :: _) as actualArguments ->
                        case
                            referenceGetExpectedParameters
                                { range = application.referenceRange
                                , name = application.referenceName
                                }
                                state
                        of
                            Nothing ->
                                []

                            Just expectedParameters ->
                                case expectedParameters |> List.drop (actualArguments |> List.length) of
                                    -- fully applied
                                    [] ->
                                        []

                                    (_ :: _) as partiallyAppliedParameters ->
                                        [ Review.Rule.error
                                            { message = "Curried function call"
                                            , details = [ "Not all expected arguments have been provided. Elm will interpret this as as 'partial application', which can be confusing and is also slow. To fix this error, add the missing arguments or use lambda if necessary." ]
                                            }
                                            application.referenceRange
                                        ]
                , { state | rangesToIgnore = expressionRange :: state.rangesToIgnore }
                )


rangeIncludesLocation : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Range -> Bool
rangeIncludesLocation locationToCheckForInclusionInRange range =
    (Elm.Syntax.Range.compareLocations range.start locationToCheckForInclusionInRange /= GT)
        && (Elm.Syntax.Range.compareLocations range.end locationToCheckForInclusionInRange /= LT)


nonApplicationExpressionEnterVisitor :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> ModuleState
    -> ( List (Review.Rule.Error {}), ModuleState )
nonApplicationExpressionEnterVisitor expressionNode state =
    case Elm.Syntax.Node.value expressionNode of
        Elm.Syntax.Expression.LetExpression letIn ->
            ( []
            , { state
                | letIntroducedFunctions =
                    letIn.declarations
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node _ letDeclaration) soFar ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetDestructuring _ _ ->
                                        soFar

                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        case letValueOrFunctionDeclaration.signature of
                                            Nothing ->
                                                soFar

                                            Just (Elm.Syntax.Node.Node _ signature) ->
                                                case signature.typeAnnotation |> syntaxTypeExpandToFunction |> .inputs of
                                                    (_ :: _ :: _) as parameterTypes ->
                                                        soFar
                                                            |> Dict.insert
                                                                (signature.name |> Elm.Syntax.Node.value)
                                                                (parameterTypes
                                                                    |> listToDisambiguatedVariables syntaxTypeToVariable
                                                                )

                                                    -- only 0-1 parameters, can never be partially applied
                                                    _ ->
                                                        soFar
                            )
                            state.letIntroducedFunctions
              }
            )

        _ ->
            ( [], state )


syntaxExpressionToApplication :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        Maybe
            { referenceRange : Elm.Syntax.Range.Range
            , referenceName : String
            , arguments : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
            }
syntaxExpressionToApplication expressionNode =
    syntaxExpressionToApplicationBefore [] expressionNode


syntaxExpressionToApplicationBefore :
    List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        Maybe
            { referenceRange : Elm.Syntax.Range.Range
            , referenceName : String
            , arguments : List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
            }
syntaxExpressionToApplicationBefore argumentsAfter (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            syntaxExpressionToApplicationBefore argumentsAfter inParens

        Elm.Syntax.Expression.Application (called :: arguments) ->
            syntaxExpressionToApplicationBefore
                (arguments ++ argumentsAfter)
                called

        Elm.Syntax.Expression.OperatorApplication "|>" _ argument called ->
            syntaxExpressionToApplicationBefore
                (argument :: argumentsAfter)
                called

        Elm.Syntax.Expression.OperatorApplication "<|" _ called argument ->
            syntaxExpressionToApplicationBefore
                (argument :: argumentsAfter)
                called

        Elm.Syntax.Expression.FunctionOrValue _ name ->
            Just { referenceRange = expressionRange, referenceName = name, arguments = argumentsAfter }

        Elm.Syntax.Expression.PrefixOperator operator ->
            Just { referenceRange = expressionRange, referenceName = operator, arguments = argumentsAfter }

        _ ->
            Nothing


expressionExitVisitor : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> ModuleState -> ModuleState
expressionExitVisitor (Elm.Syntax.Node.Node expressionRange expression) state =
    let
        stateWithUpdatedRangesToIgnore : ModuleState
        stateWithUpdatedRangesToIgnore =
            case state.rangesToIgnore of
                [] ->
                    state

                rangeToIgnore :: remainingRangesToIgnore ->
                    if rangeToIgnore == expressionRange then
                        { state | rangesToIgnore = remainingRangesToIgnore }

                    else
                        state
    in
    case expression of
        Elm.Syntax.Expression.LetExpression letIn ->
            { stateWithUpdatedRangesToIgnore
                | letIntroducedFunctions =
                    letIn.declarations
                        |> List.foldl
                            (\(Elm.Syntax.Node.Node _ letDeclaration) soFar ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetDestructuring _ _ ->
                                        soFar

                                    Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                                        case letValueOrFunctionDeclaration.signature of
                                            Nothing ->
                                                soFar

                                            Just (Elm.Syntax.Node.Node _ signature) ->
                                                case signature.typeAnnotation |> syntaxTypeExpandToFunction |> .inputs of
                                                    (_ :: _ :: _) as parameterTypes ->
                                                        soFar
                                                            |> Dict.remove
                                                                (signature.name |> Elm.Syntax.Node.value)

                                                    -- only 0-1 parameters, can never be partially applied
                                                    _ ->
                                                        soFar
                            )
                            stateWithUpdatedRangesToIgnore.letIntroducedFunctions
            }

        _ ->
            stateWithUpdatedRangesToIgnore


referenceGetExpectedParameters :
    { range : Elm.Syntax.Range.Range
    , name : String
    }
    -> ModuleState
    -> Maybe (List String)
referenceGetExpectedParameters reference state =
    case Review.ModuleNameLookupTable.moduleNameAt state.moduleOriginLookup reference.range of
        Nothing ->
            Nothing

        Just [] ->
            state.letIntroducedFunctions
                |> Dict.get reference.name
                |> onNothing
                    (\() ->
                        state.introducedFunctions
                            |> Dict.get reference.name
                    )

        Just ((_ :: _) as moduleOrigin) ->
            state.importIntroducedFunctions
                |> Dict.get (moduleOrigin |> String.join ".")
                |> Maybe.andThen (\inModule -> inModule |> Dict.get reference.name)


onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing nextTry maybe =
    case maybe of
        (Just _) as just ->
            just

        Nothing ->
            nextTry ()


isJustAnd : (a -> Bool) -> Maybe a -> Bool
isJustAnd valueToBool maybe =
    case maybe of
        Nothing ->
            False

        Just value ->
            value |> valueToBool



--
