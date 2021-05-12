
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


$BaseRingM2 = "QQ";


Begin["`Private`"]


variableRules[s_String, vars_List] := 
  MapIndexed[
    ToString[#1, InputForm] -> s <> ToString[First@#2] &, 
    vars
  ];


ringCommand[ambR_, vars : KeyValuePattern[{}], 
    params : KeyValuePattern[{}] : {},  q_ : {}, deg_ : {}] := 
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


parseIdealOutput[res_, rules : KeyValuePattern[{}] ] :=
  Module[{out},
    out = res["Output"];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "ideal" ~~ "("|" (" ~~ s:Longest[___] ~~ ")" /;StringFreeQ[s, "ideal"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression
  ];


parseMapOutput[res_, rules : KeyValuePattern[{}] ] :=
  Module[{out},
    out = res["Output"];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "map" ~~ "("|" (" ~~ s:Longest[___] ~~ ")" /;StringFreeQ[s, "map"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression // Last
  ];


SyntaxInformation[MinimalPresentationM2] = {
  "ArgumentsPattern" -> {_, _, OptionsPattern[]},
  "OptionNames" -> {"Extension"}
}
Options[MinimalPresentationM2] = {Extension -> {}, "Degrees" -> {{}}};
MinimalPresentationM2[i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[MinimalPresentationM2] ] :=
  Module[{ideal, cmds, rules, vars, res1, resI, res2, resMap},
    ideal = ToSubtractList[i1];
    vars = Flatten[{v}];
    {cmds, rules} = idealCommand[
      "J = I1; trim minimalPresentation J",
      {"I1" -> ideal},
      vars,
      Complement[Variables@ideal, vars],
      OptionValue[Extension]
    ];
    If[ FailureQ[ res1 = EvaluateM2@StringRiffle[cmds, "; "] ], 
      Return[res1]
    ];
    resI = parseIdealOutput[res1, rules];
    If[FailureQ[ res2 = EvaluateM2["J.cache.minimalPresentationMap"] ],
      Return[res2]
    ];
    resMap = parseMapOutput[res2, rules];
    { 
      resI, 
      DeleteCases[Thread[vars->resMap], 
        HoldPattern[Rule][x_,x_]
      ]
    }
  ];
SetAttributes[MinimalPresentationM2, {Protected, ReadProtected}];


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
Options[PrimaryDecompositionM2] = {Extension -> {}, "Degrees" -> {{}}};
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


SyntaxInformation[MapKernelM2] = {
  "ArgumentsPattern" -> {_, _, _, OptionsPattern[]},
  "OptionNames" -> {"Extension", "Degrees"}
};
Options[MapKernelM2] = {Extension -> {}, "Degrees" -> {{}}};
MapKernelM2[map : KeyValuePattern[{}], i1 : (List|And)[Except[_List]..], v : _, 
    OptionsPattern[MapKernelM2] ] :=
  Module[{idealA, vX, varsX, params, nums, ambRCmd, aRingCmd, vM, varsM, 
      bRingCmd, cmds, res, rules},
    idealA = ToSubtractList[i1];
    vX = Flatten[{v}];
    varsX = variableRules["X", vX];
    vM = Keys[map];
    varsM = variableRules["M", vM];
    params = variableRules["z", Complement[Variables@idealA,vX] ];
    nums = variableRules["a", OptionValue[Extension] ];
    rules = ReverseSortBy[
      Reverse /@ Union[params,varsX,varsM,nums],
      First
    ];
    ambRCmd = "KK = " <> ringCommand[$BaseRingM2, nums, {},
      MinimalPolynomial[OptionValue[Extension], ToExpression@Values@nums]
    ];
    aRingCmd = "A = " <> ringCommand[
      ringCommand["KK", params],
      varsX, Union[params, nums], idealA,
      OptionValue["Degrees"]
    ];
    mesListCmd = "mes = " <> StringReplace[
      ToString[Values@map, InputForm],
      varsX
    ];
    bRingCmd = "B = " <> ringCommand[
      ringCommand["KK", params],
      varsM
    ];
    cmds = {
      ambRCmd, aRingCmd, mesListCmd, bRingCmd,
      "kernel map(A,B,mes)"
    };
    If[FailureQ[ res = EvaluateM2@StringRiffle[cmds, "; "] ],
      Return[res]
    ];
    parseIdealOutput[res, rules]
  ];
SetAttributes[MapKernelM2, {Protected, ReadProtected}];


End[]


EndPackage[]