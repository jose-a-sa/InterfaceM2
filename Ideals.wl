
BeginPackage["InterfaceM2`Ideals`", {
    "InterfaceM2`Utils`",
    "InterfaceM2`Core`"
}]

Unprotect["InterfaceM2`Ideals`*"];
ClearAll["InterfaceM2`Ideals`*"];


PrimaryDecompositionM2::usage = "";
AssociatedPrimesM2::usage = "";
RadicalM2::usage = "";
SingularLocusM2::usage = "";
MapKernelM2::usage = "";
MinimalPresentationM2::usage = "";
KrullDimensionM2::usage = "";
SpecDimM2::usage = "";
ProjDimM2::usage = "";
VersalDeformationsM2::usage = "";


$BaseRingM2 = "QQ";


Begin["`Private`"]


variableRules[s_String, vars_List] := 
  MapIndexed[
    ToString[#1, InputForm] -> s <> ToString[First@#2] &, 
    vars
  ];


ringCommand[ambR_, vars : KeyValuePattern[_]|{}, 
    params : KeyValuePattern[_]|{} : {},  q_ : {}, deg_ : {}] := 
  Module[{idealQ, varOpts, varStr, quoStr}, 
    idealQ = StringReplace[(ToString[#, InputForm] &) /@ q, Union[vars,params] ];
    varOpts = If[
      (VectorQ[deg]||MatrixQ[deg]) && First@Dimensions@deg == Length@vars > 0, 
      {"Degrees=>" <> ToString[deg, InputForm]}, {}];
    varStr = StringRiffle[Join[Values@vars, varOpts], {"[", ",", "]"}];
    quoStr = If[Length@idealQ > 0, 
      StringRiffle[idealQ, {"/(", ", ", ")"}], ""];
    StringJoin[ambR, varStr, quoStr] // StringDelete["[]"]
  ];


idealCommand[cmd_, rules_, v_List, p_List, ext_List : {}] := 
  Module[{vars, params, nums, rls, ideals, 
      baseRingCmd, ringCmd, idealCmd},
    vars = variableRules["X", v];
    params = variableRules["z", p];
    nums = variableRules["a", ext];
    rls = Union[vars, params, nums];
    baseRingCmd = "KK = " <> ringCommand[
      $BaseRingM2, 
      nums,  {},
      MinimalPolynomial[ext, ToExpression@Values@nums] 
    ];
    ringCmd = "R = " <> ringCommand[
      ringCommand["KK", params],
      vars, Union[nums, params]
    ];
    ideals = (StringReplace[
      ToString[#, InputForm], 
      Join[ReverseSortBy[First]@rls, {"}" -> ")","{" -> "ideal("}]
    ] &) /@ KeySelect[Association@rules, 
      StringMatchQ["I" ~~ (DigitCharacter ..)]
    ];
    idealCmd = StringReplace[cmd, KeyValueMap[Rule]@ideals];
    {
      {baseRingCmd, ringCmd, idealCmd},
      ReverseSortBy[First]@Map[Reverse]@rls
    }
  ];


parseIdealOutput[res_, rules : KeyValuePattern[_] ] :=
  Module[{out},
    out = res["Output"];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "ideal" ~~ "("|" (" ~~ s:Longest[___] ~~ ")" /;StringFreeQ[s, "ideal"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression
  ];


parseMapOutput[res_, rules : KeyValuePattern[_] ] :=
  Module[{out},
    out = res["Output"];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "map" ~~ "("|" (" ~~ s:Longest[___] ~~ ")" /;StringFreeQ[s, "map"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression // Last
  ];


SyntaxInformation[AssociatedPrimesM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[AssociatedPrimesM2] = {Extension -> {}, "Degrees" -> {{}}};
AssociatedPrimesM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[AssociatedPrimesM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "associatedPrimes I1",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[AssociatedPrimesM2, {Protected, ReadProtected}];


SyntaxInformation[PrimaryDecompositionM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[PrimaryDecompositionM2] = {Extension -> {}, "Degrees" -> {{}}, "ParameterIdeal" -> {}};
PrimaryDecompositionM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[PrimaryDecompositionM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "primaryDecomposition I1",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[PrimaryDecompositionM2, {Protected, ReadProtected}];


SyntaxInformation[RadicalM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[RadicalM2] = {Extension -> {}, "Degrees" -> {{}}};
RadicalM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[RadicalM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "radical I1",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[RadicalM2, {Protected, ReadProtected}];


SyntaxInformation[SingularLocusM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[SingularLocusM2] = {Extension -> {}, "Degrees" -> {{}}};
SingularLocusM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[SingularLocusM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "ideal presentation singularLocus I1",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[SingularLocusM2, {Protected, ReadProtected}];


SyntaxInformation[SpecDimM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[SpecDimM2] = {Extension -> {}, "Degrees" -> {{}}};
SpecDimM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[SpecDimM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "dim Spec(R/I1)",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    ToExpression@res["Output"]
  ];
SetAttributes[SpecDimM2, {Protected, ReadProtected}];


SyntaxInformation[ProjDimM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[ProjDimM2] = {Extension -> {}, "Degrees" -> {{}}};
ProjDimM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[ProjDimM2] ] :=
  Module[{ideal, cmds, rules, vars, res},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "dim Proj(R/I1)",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    ToExpression@res["Output"]
  ];
SetAttributes[ProjDimM2, {Protected, ReadProtected}];


SyntaxInformation[KrullDimensionM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
};
Options[KrullDimensionM2] = {Extension -> {}, "Degrees" -> {{}}};
KrullDimensionM2[is : {(List|And)[Except[_List]..]..}, v : _, 
    OptionsPattern[KrullDimensionM2] ] :=
  Module[{l1, cmds, rules, vars, res},
    l1 = MapIndexed["I"<>ToString[First@#2] -> ToSubtractList[#1] &, is];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      StringRiffle[Keys@l1, {"{", ",", "} / dim"}],
      l1, vars,
      Complement[Variables@Values@l1, vars],
      OptionValue[Extension]
    ];
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    ToExpression@res["Output"]
  ];
KrullDimensionM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[KrullDimensionM2] ] :=
  First@KrullDimensionM2[{i1}, v];
SetAttributes[KrullDimensionM2, {Protected, ReadProtected}];


Options[MapKernelM2] = {Extension -> {}, "Degrees" -> {{}}, "ParameterIdeal" -> {}};
MapKernelM2[map : KeyValuePattern[{}], i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[MapKernelM2] ] :=
  Module[{idealA, vX, varsX, params, nums, ambRCmd, pRingCmd, aRingCmd, vM, varsM, 
      bRingCmd, cmds, res, rules},
    idealA = ToSubtractList[i1];
    vX = Flatten[{v}];
    varsX = variableRules["X", vX];
    vM = Keys[map];
    varsM = variableRules["M", vM];
    params = variableRules["z", Union[Variables@OptionValue["ParameterIdeal"], Complement[Variables@idealA, vX]] ];
    nums = variableRules["a", OptionValue[Extension] ];
    rules = ReverseSortBy[
      Reverse /@ Union[params,varsX,varsM,nums],
      First
    ];
    ambRCmd = "KK = " <> ringCommand[$BaseRingM2, nums, {},
      MinimalPolynomial[OptionValue[Extension], ToExpression@Values@nums]
    ];
    pRingCmd = "PP = " <> ringCommand["KK", params, OptionValue["ParameterIdeal"]];
    aRingCmd = "A = " <> ringCommand[
      "PP",
      varsX, Union[params, nums], idealA,
      OptionValue["Degrees"]
    ];
    mesListCmd = "mes = " <> StringReplace[
      ToString[Values@map, InputForm],
      varsX
    ];
    bRingCmd = "B = " <> ringCommand[
      "PP",
      varsM
    ];
    cmds = {
      ambRCmd, pRingCmd, aRingCmd, mesListCmd, bRingCmd,
      "kernel map(A,B,mes)"
    };
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[MapKernelM2, {Protected, ReadProtected}];


Options[MinimalPresentationM2] = {Extension -> {}, "Degrees" -> {{}}, "ParameterIdeal" -> {}};
MinimalPresentationM2[
    i1 : (List | And)[Except[_List] ..], v : _, 
    OptionsPattern[MinimalPresentationM2] ] := 
  Module[{ideal, vars, params, nums, rules, ambRCmd, pRingCmd, ringCmd, idealCmd, preCmd, 
      preOutput, minIdealOut, mapOutput}, 
    ideal = ToSubtractList[i1];
    vars = variableRules["X", Flatten@{v}];
    params = variableRules["z", 
      Union[Variables@OptionValue["ParameterIdeal"], Complement[Variables@ideal, Flatten@{v}]]
    ];
    nums = variableRules["a", OptionValue[Extension]];
    rules = ReverseSortBy[Reverse /@ Union[params, vars, nums], First];
    ambRCmd = "KK = " <> ringCommand[$BaseRingM2, nums, {}, 
      MinimalPolynomial[OptionValue[Extension], 
      ToExpression@Values@nums]
    ];
    pRingCmd = "PP = " <> ringCommand["KK", params, 
      OptionValue["ParameterIdeal"]
    ];
    ringCmd = "A = " <> ringCommand["PP", vars, 
      Union[params, nums], {}, OptionValue["Degrees"]];
    idealCmd = "I = ideal " <> StringReplace[ToString[ideal, InputForm], Union[params, vars]];
    preCmd = StringRiffle[{ambRCmd, pRingCmd, ringCmd, idealCmd}, "; "];
    preOutput = EvaluateM2[preCmd];
    Assert[!FailureQ@preOutput, "preCmdFailed"];
    minIdealOut = EvaluateM2[
      "first entries gens trim minimalPresentation I"];
    Assert[!FailureQ@minIdealOut, "minimalPresentationFailed"];
    mapOutput = EvaluateM2[
      "{gens(A)|gens(baseRing A), first entries I.cache.minimalPresentationMap.matrix}"];
    Assert[!FailureQ@minIdealOut, "minimalPresentationFailed"];
    {minIdealOut, mapOutput} = Map[
      ToExpression@*StringReplace[rules]@*Lookup["Output"],
      {minIdealOut, mapOutput}
    ];
    {
      minIdealOut, 
      SortBy[First]@Thread[Rule @@ mapOutput] // DeleteCases[HoldPattern[Rule][x_,x_]]
    }
];
SetAttributes[MinimalPresentationM2, {Protected, ReadProtected}];


Options[VersalDeformationsM2] = {Extension -> {}, "Degrees" -> {{}}};
VersalDeformationsM2[
    i1 : (List|And)[Except[_List]..], v : _,
    deg_Integer?NonNegative, 
    opts : OptionsPattern[VersalDeformationsM2] ] := 
  VersalDeformationsM2[i1, v, {deg, deg}, opts];
VersalDeformationsM2[
    i1 : (List|And)[Except[_List]..], v : _,
    {dm_Integer, dM_Integer} /; (dm <= dM), 
    OptionsPattern[VersalDeformationsM2] ] :=
  Module[{ideal, vars, params, nums, rules, ambRCmd, ringCmd, idealCmd, preCmd, output,
      fEntriesCell, rEntriesCell, gEntriesCell, cEntriesCell},
    ideal = ToSubtractList[i1];
    vars = variableRules["X", Flatten@{v}];
    params = variableRules["z", Complement[Variables@ideal,Flatten@{v}] ];
    nums = variableRules["a", OptionValue[Extension] ];
    tvars = {"t_"~~(num : Longest[DigitCharacter..]) :>  "\[FormalT]["<>num<>"]"};
    rules = ReverseSortBy[
      Reverse /@ Union[params,vars,nums],
      First
    ];
    ambRCmd = "K = " <> ringCommand[$BaseRingM2, nums, {},
      MinimalPolynomial[OptionValue[Extension], ToExpression@Values@nums]
    ];
    ringCmd = "A = " <> ringCommand[
      ringCommand["K", params],
      vars, Union[params, nums], {},
      OptionValue["Degrees"]
    ];
    idealCmd = "I = gens ideal " <> StringReplace[
      ToString[ideal, InputForm],
      Union[params,vars]
    ];
    preCmd = StringRiffle[{"needsPackage \"VersalDeformations\"", ambRCmd, ringCmd,
      idealCmd}, "; "];
    output = EvaluateM2[preCmd];
    Assert[!FailureQ@output, "SetupCommandFailed"];
    output = EvaluateM2[StringJoin[
      "(F,R,G,C) = versalDeformation(I,",
      StringRiffle[Map[# <> StringRiffle[{dm, dM}, {"(", ",", ",I)"}] &, {"CT^1", "CT^2"}], ","],
      ");"]
    ];
    Assert[!FailureQ@output, "versalDeformationFailed"];
    fEntriesCell = EvaluateM2["F / entries"];
    Assert[!FailureQ[fEntriesCell], "FentriesFailed"];
    rEntriesCell = EvaluateM2["R / entries"];
    Assert[!FailureQ[rEntriesCell], "RentriesFailed"];
    gEntriesCell = EvaluateM2["G / entries"];
    Assert[!FailureQ[gEntriesCell], "GentriesFailed"];
    cEntriesCell = EvaluateM2["C / entries"];
    Assert[!FailureQ[cEntriesCell], "CentriesFailed"];
    Map[
      ToExpression@*StringReplace[tvars]@*StringReplace[rules]@*Lookup["Output"],
      {fEntriesCell, rEntriesCell, gEntriesCell, cEntriesCell}
    ]
  ];
SetAttributes[VersalDeformationsM2, {Protected, ReadProtected}];


End[]


EndPackage[]