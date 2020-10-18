
BeginPackage["InterfaceM2`Ideals`", {
    "InterfaceM2`Core`"
}]

Unprotect["InterfaceM2`Ideals`*"];
ClearAll["InterfaceM2`Ideals`*"];


PrimaryDecompositionM2::usage = "";
AssociatedPrimesM2::usage = "";
RadicalM2::usage = "";
SingularLocusM2::usage = "";


$BaseRingM2 = "ZZ/101";


Begin["`Private`"]


toSubtractList[ expr : (List|And)[Except[_List]..] ] := 
  Map[ Through@*If[MatchQ[_Equal], Apply[Subtract], Identity],
    List @@ expr
  ];


toCommandIdealsM2[cmd_, rules_, v_List, p_List] := 
  Module[{rp, vars, params, ideals, ringStr},
    rp = Association[rules];
    vars = MapIndexed[
      ToString[#1, InputForm] -> "A"<>ToString[First@#2] &, v
    ];
    params = MapIndexed[
      ToString[#1, InputForm] -> "m"<>ToString[First@#2] &, p
    ];
    ringStr = StringJoin[
      "R = ", $BaseRingM2, 
      If[0 == Length@params, "", 
        StringRiffle[Values@params, {"[", ",", "]"}] 
      ],
      StringRiffle[Values@vars, {"[", ",", "]"}],
      "; "
    ];
    ideals = (StringReplace[ToString[#, InputForm],
      Join[ ReverseSortBy[First]@Union[vars, params],
        { "}" -> ")", "{" -> "ideal(" }] 
    ] &) /@ KeySelect[rp, StringMatchQ["I" ~~ (DigitCharacter ..)] ];
    {
      ringStr <> StringReplace[cmd, KeyValueMap[Rule]@ideals], 
      ReverseSortBy[First]@Map[Reverse]@Union[vars, params]
    }
  ]


SyntaxInformation[AssociatedPrimesM2] = {"ArgumentsPattern" -> {_, _}};
AssociatedPrimesM2[i1 : (List|And)[Except[_List]..], v : _] :=
  Module[{cmd, ideal1, rules, vars, res, out},
    ideal1 = toSubtractList@i1;
    vars = Flatten[{v}];
    {cmd, rules} = toCommandIdealsM2[
      "associatedPrimes I1",
      {"I1" -> ideal1},
      vars,
      Complement[Variables@ideal1, vars]
    ];
    If[FailureQ[res = EvaluateM2@cmd],
      Return[$Failed]
    ];
    out = First@Flatten@StringCases[res, 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~ x__ :> x
    ];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "ideal" ~~ "("|" (" ~~ s:Longest[__] ~~ ")" /;StringFreeQ[s, "ideal"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression
  ];
SetAttributes[AssociatedPrimesM2, {Protected, ReadProtected}];


SyntaxInformation[PrimaryDecompositionM2] = {"ArgumentsPattern" -> {_, _}};
PrimaryDecompositionM2[i1 : (List|And)[Except[_List]..], v : _] :=
  Module[{cmd, ideal1, rules, vars, res, out},
    ideal1 = toSubtractList@i1;
    vars = Flatten[{v}];
    {cmd, rules} = toCommandIdealsM2[
      "primaryDecomposition I1",
      {"I1" -> ideal1},
      vars,
      Complement[Variables@ideal1, vars]
    ];
    If[FailureQ[res = EvaluateM2@cmd],
      Return[$Failed]
    ];
    out = First@Flatten@StringCases[res, 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~ x__ :> x
    ];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "ideal" ~~ "("|" (" ~~ s:Longest[__] ~~ ")" /;StringFreeQ[s, "ideal"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression
  ];
SetAttributes[PrimaryDecompositionM2, {Protected, ReadProtected}];


SyntaxInformation[RadicalM2] = {"ArgumentsPattern" -> {_, _}};
RadicalM2[i1 : (List|And)[Except[_List]..], v : _] :=
  Module[{cmd, ideal1, rules, vars, res, out},
    ideal1 = toSubtractList@i1;
    vars = Flatten[{v}];
    {cmd, rules} = toCommandIdealsM2[
      "radical I1",
      {"I1" -> ideal1},
      vars,
      Complement[Variables@ideal1, vars]
    ];
    If[FailureQ[res = EvaluateM2@cmd],
      Return[$Failed]
    ];
    out = First@Flatten@StringCases[res, 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~ x__ :> x
    ];
    StringReplace[rules]@StringJoin@StringSplit[out, 
      "ideal" ~~ "("|" (" ~~ s:Longest[__] ~~ ")" /;StringFreeQ[s, "ideal"] 
          :> StringJoin["List[", s, "]"] 
    ] // ToExpression
  ];
SetAttributes[RadicalM2, {Protected, ReadProtected}];


SyntaxInformation[SingularLocusM2] = {"ArgumentsPattern" -> {_, _}};
SingularLocusM2[i1 : (List|And)[Except[_List]..], v : _] :=
  Module[{cmd, ideal1, rules, vars, res, out},
    ideal1 = toSubtractList@i1;
    vars = Flatten[{v}];
    {cmd, rules} = toCommandIdealsM2[
      "singularLocus( I1 )",
      {"I1" -> ideal1},
      vars,
      Complement[Variables@ideal1, vars]
    ];
    If[FailureQ[res = EvaluateM2@cmd],
      Return[$Failed]
    ];
    out = First@Flatten@StringCases[res, 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~
         Shortest[__] ~~ " / " ~~ x__ :> x
    ];
    StringReplace[ StringTrim@out, 
      Join[rules, { ")" ~~ EndOfString -> "]", "ideal(" -> "List["}]
    ] // ToExpression
  ];
SetAttributes[SingularLocusM2, {Protected, ReadProtected}];


End[]


EndPackage[]