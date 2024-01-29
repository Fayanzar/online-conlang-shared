module SharedModels

let routeBuilder (typeName: string) (methodName: string) =
    let friendlyTypeName = typeName.TrimStart('I').ToLower()
    sprintf "/api/%s/%s" friendlyTypeName methodName

type CustomError = { errorMsg: string }

type Class = string
type PartOfSpeech = string

[<StructuredFormatDisplay("{input} → {output}")>]
[<CLIMutable>]
type Transformation =
    {
        input         : string
        output        : string
        applyMultiple : bool
    }

type Language =
    {
        id   : int
        name : string
    }

type Inflection =
    {
        inflectionName       : string Option
        inflectionClasses    : Class list
        inflectionSpeechPart : PartOfSpeech
        inflectionAxes       : int list
    }

type Affix =
    | Prefix of string * Transformation
    | Suffix of string * Transformation
    | Infix of string * Transformation * Transformation * int

type Rule =
    | AffixRule of Affix
    | TRule of Transformation

type RuleSet = Rule list

type AxisValue = int

type Axis =
    {
        name         : string
        inflections  : Map<AxisValue, RuleSet>
    }
    with
    member this.InflectionNames = this.inflections.Keys |> Seq.toList
    member this.InflectionRules = this.inflections.Values |> Seq.toList

type Axes =
    {
        axes      : Axis list
        overrides : (AxisValue list * RuleSet) list
    }
    with
    member this.axesNumber = List.length this.axes
    member this.ruleSetByNames (names : AxisValue list) =
        match this.overrides |> List.filter (fun (k, _) -> Set k = Set names) with
        | [] ->
            let inflections = List.map (fun a -> a.inflections |> Map.toList) this.axes |> List.concat |> Map.ofList
            let ruleSetList = List.map (fun name -> Map.find name inflections) names
            List.concat ruleSetList
        | ruleSet -> ruleSet |> List.map snd |> List.concat

type OverrideRule =
    {
        overrideRule : Rule
        overrideAxes : int list
    }

type AxisForAPI =
    {
        id     : int
        name   : string
        values : (int * string) list
    }

type InflectTForAPI =
    {
        id         : int
        name       : string Option
        language   : int
        speechPart : PartOfSpeech
        classes    : Class Set
        axes       : Axes
    }

type TermForAPI =
    {
        id                : int
        word              : string
        speechPart        : PartOfSpeech
        wordClasses       : Class Set
        inflection        : (string Option * ((string list) * ((int list) * string) list)) list Option
        transcription     : string Option
    }

type IServer = {
    getLanguages : Async<list<Language>>
    postLanguage : string -> Async<unit>
    putLanguage : int -> string -> Async<unit>
    deleteLanguage : int -> Async<unit>

    getClasses : int -> Async<Map<string, seq<string>>>
    postClass : int -> string -> Async<unit>
    putClass : int -> string -> string -> Async<unit>
    deleteClass : int -> string -> Async<unit>

    postClassValue : int -> string -> string -> Async<unit>
    putClassValue : int -> string -> string -> string -> Async<unit>
    deleteClassValue : int -> string -> string -> Async<unit>

    getSpeechParts : int -> Async<seq<string>>
    postSpeechPart : int -> string -> Async<unit>
    putSpeechPart : int -> string -> string -> Async<unit>
    deleteSpeechPart : int -> string -> Async<unit>

    getTranscriptions : int -> Async<list<Transformation>>
    postTranscription : int -> Transformation -> Async<unit>
    putTranscription : int -> int -> Transformation -> Async<unit>
    deleteTranscription : int -> int -> Async<unit>

    getTerms : int -> Async<seq<TermForAPI>>
    postTerm : int -> TermForAPI -> Async<unit>
    putTerm : int -> int -> TermForAPI -> Async<unit>
    deleteTerm : int -> int -> Async<unit>

    rebuildInflections : int -> Async<unit>

    getAxes : int -> Async<seq<AxisForAPI>>
    postAxisName : int -> string -> Async<unit>
    putAxisName : int -> string -> Async<unit>
    deleteAxisName : int -> Async<unit>

    postAxisValue : int -> string -> Async<unit>
    putAxisValue : int -> string -> Async<unit>
    deleteAxisValue : int -> Async<unit>

    getAxisRules : int -> int -> Async<Map<int, Rule>>
    postAxisRule : int -> int -> Rule -> Async<unit>
    postAxisRules : int -> int -> Rule list -> Async<unit>
    putAxisRule : int -> Rule -> Async<unit>
    deleteAxisRule : int -> Async<unit>

    getOverrideRules : int -> int -> Async<Map<int, OverrideRule>>
    postOverrideRule : int -> OverrideRule -> Async<unit>
    postOverrideRules : int -> OverrideRule list -> Async<unit>
    putOverrideRule : int -> OverrideRule -> Async<unit>
    deleteOverrideRule : int -> Async<unit>

    getInflections : int -> Async<seq<InflectTForAPI>>
    getInflectionsStructure : int -> Async<Map<int, Inflection>>
    postInflection : Inflection -> Async<unit>
    putInflection : int -> Inflection -> Async<unit>
    deleteInflection : int -> Async<unit>

    postPhonemeClass : int -> char -> char option -> Async<unit>
    putPhonemeClass : int -> char -> char -> Async<unit>
    deletePhonemeClass : int -> char -> Async<unit>
}
