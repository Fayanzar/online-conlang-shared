module SharedModels

let routeBuilder (typeName: string) (methodName: string) =
    let friendlyTypeName = typeName.TrimStart('I').ToLower()
    sprintf "/api/%s/%s" friendlyTypeName methodName

type CustomError = { errorMsg: string }

type LoginInfo = { username: string; password: string }

type SecurityToken = SecurityToken of string

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
    postLogin : LoginInfo -> Async<SecurityToken>
    postRegister : LoginInfo -> Async<unit>

    getLanguages : Async<list<Language>>
    postLanguage : SecurityToken -> string -> Async<SecurityToken>
    putLanguage : SecurityToken -> int -> string -> Async<unit>
    deleteLanguage : SecurityToken -> int -> Async<unit>

    getClasses : int -> Async<Map<string, seq<string>>>
    postClass : SecurityToken -> int -> string -> Async<unit>
    putClass : SecurityToken -> int -> string -> string -> Async<unit>
    deleteClass : SecurityToken -> int -> string -> Async<unit>

    postClassValue : SecurityToken -> int -> string -> string -> Async<unit>
    putClassValue : SecurityToken -> int -> string -> string -> string -> Async<unit>
    deleteClassValue : SecurityToken -> int -> string -> string -> Async<unit>

    getSpeechParts : int -> Async<seq<string>>
    postSpeechPart : SecurityToken -> int -> string -> Async<unit>
    putSpeechPart : SecurityToken -> int -> string -> string -> Async<unit>
    deleteSpeechPart : SecurityToken -> int -> string -> Async<unit>

    getTranscriptions : int -> Async<list<Transformation>>
    postTranscription : SecurityToken -> int -> Transformation -> Async<unit>
    putTranscription : SecurityToken -> int -> Transformation -> Async<unit>
    deleteTranscription : SecurityToken -> int -> Async<unit>

    getTerms : int -> Async<seq<TermForAPI>>
    postTerm : SecurityToken -> int -> TermForAPI -> Async<unit>
    putTerm : SecurityToken -> int -> TermForAPI -> Async<unit>
    deleteTerm : SecurityToken -> int -> Async<unit>

    rebuildInflections : SecurityToken -> int -> Async<unit>

    getAxes : int -> Async<seq<AxisForAPI>>
    postAxisName : SecurityToken -> int -> string -> Async<unit>
    putAxisName : SecurityToken -> int -> string -> Async<unit>
    deleteAxisName : SecurityToken -> int -> Async<unit>

    postAxisValue : SecurityToken -> int -> string -> Async<unit>
    putAxisValue : SecurityToken -> int -> string -> Async<unit>
    deleteAxisValue : SecurityToken -> int -> Async<unit>

    getAxisRules : int -> int -> Async<Map<int, Rule>>
    postAxisRules : SecurityToken -> int -> int -> Rule list -> Async<unit>
    putAxisRule : SecurityToken -> int -> Rule -> Async<unit>
    deleteAxisRule : SecurityToken -> int -> Async<unit>

    getOverrideRules : int -> int -> Async<Map<int, OverrideRule>>
    postOverrideRules : SecurityToken -> int -> OverrideRule list -> Async<unit>
    putOverrideRule : SecurityToken -> int -> OverrideRule -> Async<unit>
    deleteOverrideRule : SecurityToken -> int -> Async<unit>

    getInflections : int -> Async<seq<InflectTForAPI>>
    getInflectionsStructure : int -> Async<Map<int, Inflection>>
    postInflection : SecurityToken -> Inflection -> Async<unit>
    putInflection : SecurityToken -> int -> Inflection -> Async<unit>
    deleteInflection : SecurityToken -> int -> Async<unit>

    postPhonemeClass : SecurityToken -> int -> char -> char option -> Async<unit>
    putPhonemeClass : SecurityToken -> int -> char -> char -> Async<unit>
    deletePhonemeClass : SecurityToken -> int -> char -> Async<unit>
}
