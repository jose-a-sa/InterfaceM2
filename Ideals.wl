
BeginPackage["InterfaceM2`Ideals`", {
    "InterfaceM2`Core`"
}]

Unprotect["InterfaceM2`Ideals`*"];
ClearAll["InterfaceM2`Ideals`*"];


IdealPrimaryDecomposition::usage = "";
IdealRadical::usage = "";


$BaseRingM2 = "ZZ/104729";


Begin["`Private`"]


polyVarsStr[n_, v_String : "a"] := 
	StringReplace["a"->v]@Switch[n, 
		0, "", 
		1, "[a_1]", 
		_, "[a_1 .. a_" <> ToString[n] <> "]"
	];

SyntaxInformation[IdealPrimaryDecomposition] = {"ArgumentsPattern" -> {_, _}};
IdealPrimaryDecomposition[
		i : (And|List)[__],
  	v : ({__} | _)] :=
	Module[{ideal, vars, nV, params, nP, idealStr, ringStr, outStr},
		ideal = List @@ Map[If[MatchQ[#, _Equal], Subtract@@#, #]&]@i;
  	vars = MapIndexed[
			ToString[#1, InputForm] -> "A_"<>ToString[First@#2] &,
			Flatten@{v}
		];
		params = MapIndexed[
			ToString[#1, InputForm] -> "m_"<>ToString[First@#2] &,
			Complement[Variables[ideal], Flatten@{v}]
		];
  	nV = Length[vars];
		nP = Length[params];
		ringStr = StringJoin[
			"R = ", $BaseRingM2, 
			polyVarsStr[ nP, "m"],
			polyVarsStr[ nV, "A"],
			"; "
		];
		idealStr = StringReplace[ToString[ideal, InputForm], 
			Join[ ReverseSortBy[First]@Union[vars, params], 
				{ "}" -> ")", "{" -> "ideal(" } 
			]
		];
		outStr = First@Flatten@StringCases[
			EvaluateM2[ringStr <> "primaryDecomposition " <> idealStr], 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~ x__ :> x
		];
		ToExpression@StringReplace[outStr, Join[ 
				ReverseSortBy[First]@Map[Reverse]@Union[vars, params], 
				{ "(" -> "[", ")" -> "]", "ideal" -> "List"} 
			]
		]
  ];
SetAttributes[IdealPrimaryDecomposition, {Protected, ReadProtected}];


SyntaxInformation[IdealRadical] = {"ArgumentsPattern" -> {_, _}};
IdealRadical[
		i : (And|List)[__],
  	v : ({__} | _)] :=
	Module[{ideal, vars, rp, nV, params, nP, idealStr, ringStr, outStr},
		ideal = List @@ Map[If[MatchQ[#, _Equal], Subtract@@#, #]&]@i;
  	vars = MapIndexed[
			ToString[#1, InputForm] -> "A_"<>ToString[First@#2] &,
			Flatten@{v}
		];
		params = MapIndexed[
			ToString[#1, InputForm] -> "m_"<>ToString[First@#2] &,
			Complement[Variables[ideal], Flatten@{v}]
		];
  	nV = Length[vars];
		nP = Length[params];
		ringStr = StringJoin[
			"R = ", $BaseRingM2, 
			polyVarsStr[ nP, "m"],
			polyVarsStr[ nV, "A"],
			"; "
		];
		idealStr = StringReplace[ToString[ideal, InputForm], 
			Join[ ReverseSortBy[First]@Union[vars, params], 
				{ "}" -> ")", "{" -> "ideal(" } 
			]
		];
		outStr = First@Flatten@StringCases[
			EvaluateM2[ringStr <> "radical " <> idealStr], 
      "o" ~~ (DigitCharacter ..) ~~ " = " ~~ x__ :> x
		];
		ToExpression@StringReplace[outStr, Join[ 
				ReverseSortBy[First]@Map[Reverse]@Union[vars, params], 
				{ "(" -> "[", ")" -> "]", "ideal" -> "List"} 
			]
		]
  ]
SetAttributes[IdealRadical, {Protected, ReadProtected}];


End[]


EndPackage[]