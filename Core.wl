
BeginPackage["InterfaceM2`Core`"]

Unprotect["InterfaceM2`Core`*"];
ClearAll["InterfaceM2`Core`*"];


InitializeM2::usage = "";
EvaluateM2::usage = "";
SessionCellsM2::usage = "";
SessionHistoryM2::usage = "";
RawSessionHistoryM2::usage = "";
RestartM2::usage = "";
KillM2::usage = "";

$PrintRawM2 = False;
$PrintDebugM2 = False;
$EvaluationTimeOverflowM2 = 600;
$InitializeTimeOverflowM2 = 10;


Begin["`Private`"]

$M2String = "";

$processM2 = Automatic;


$varPatt = LetterCharacter ~~ ((LetterCharacter | DigitCharacter) ...);
$subPatt = $varPatt | (DigitCharacter ..);
$subLinePatt = ($subPatt | " ") ..;
$superPatt = (DigitCharacter ..);
$inPatt = "i" ~~ (DigitCharacter ..) ~~ (" : " );
$outPatt = "o" ~~ (DigitCharacter ..) ~~ (" = " );
$typePatt = "o" ~~ (DigitCharacter ..) ~~ (" : ");
$superLinePatt = ($superPatt | " ") ..;
$multilinePatt = Repeated["-", {500, Infinity}] ~~ "\n";


InitializeM2::nspec = "M2 process location was never specified. \
Please introduce it as the argument.";
InitializeM2::tmovrflw = 
"Initialization time surpassed. Please verify if Macaulay2 is working properly. \
Modify $InitializeTimeOverflowM2 to override the default value.";


SyntaxInformation[InitializeM2] = {"ArgumentsPattern" -> {_.}};
InitializeM2[proc : (_String | {__String} | Automatic) : Automatic] := 
  Module[{buf = "", m2, timeStep = 0.01},
    If[proc === Automatic,
      If[$processM2 === Automatic, 
        Message[InitializeM2::nspec]; Return[Null]
      ],
      $processM2 = proc
    ];
    m2 = StartProcess[$processM2];
    TimeConstrained[
      While[!StringMatchQ[buf, ___ ~~ "\ni" ~~ (DigitCharacter ..) ~~ " : "],
        Pause[timeStep];
        buf = ReadString[m2, EndOfBuffer];
        $M2String = StringJoin[$M2String, buf];
      ],
      $InitializeTimeOverflowM2,
      Message[InitializeM2::tmovrflw, $InitializeTimeOverflowM2];
      KillM2[]; Return[$Failed]
    ];
    $M2String = StringDelete[$M2String, StartOfString ~~ "\n"];
    M2 = m2
  ];
SetAttributes[InitializeM2, {Protected, ReadProtected}];


SyntaxInformation[RestartM2] = {"ArgumentsPattern" -> {}};
RestartM2[] := 
  Module[{},
    If[ FailureQ@EvaluateM2["restart;"],
      Return[Null],
      $M2String = ""; Return[$Failed]
    ];
  ];
SetAttributes[RestartM2, {Protected, ReadProtected}];


SyntaxInformation[KillM2] = {"ArgumentsPattern" -> {}}
KillM2[] := 
  Module[{},
    KillProcess[M2];
    $M2String = "";
    Return[Null];
  ];
SetAttributes[KillM2, {Protected, ReadProtected}];


multiLineStringDrop[str_, d_] := 
  StringRiffle[StringDrop[StringSplit[str, "\n"], d], "\n"];


multiLineStringTrim[s : {__String}] := 
  StringDrop[s, 
   UpTo@Min@StringCases[s, 
      StartOfString ~~ (x : (" " ...)) :> StringLength@x]
  ];


createCell[res_String] := 
  Module[{lines, in, out0, type0, out, type, dropStrCases}, 
    dropStrCases = multiLineStringDrop[#1, 
      UpTo@StringLength@First@StringCases[#1, #2]
    ] &;
    lines = StringSplit[res, "\n\n"];
    in = StringDelete[First@lines, StartOfString ~~ $inPatt];
    type0 = dropStrCases[Last@Cases[lines, 
      _String?(StringContainsQ[$typePatt])], $typePatt];
    out0 = dropStrCases[parseMultiline@Cases[lines, 
      _String?(StringContainsQ[$outPatt])], $outPatt];
    type = parseSupersubscript@StringSplit[type0, "\n"];
    out = Switch[type0, 
      _, parseSupersubscript@StringSplit[out0, "\n"]
    ];
    Association[
      "Input" -> in, 
      "Output" -> out, 
      "Type" -> type
    ]
  ];


parseMultiline[{}] := "";
parseMultiline[{str_}] := 
  str /; StringFreeQ[str, $multilinePatt];
parseMultiline[{str_}] :=
  Module[{outPattLen, lineSepPatt, split},
    outPattLen = StringLength@First@StringCases[str, $outPatt];
    lineSepPatt = StringRepeat[" ", outPattLen] ~~ $multilinePatt;
    split = StringSplit[DeleteCases[StringSplit[str, lineSepPatt],
      _String?(StringMatchQ[lineSepPatt])], "\n"];
    StringRiffle[StringJoin @@@ Transpose[split], "\n"]
  ];


parseSupersubscript[{}] := "";
parseSupersubscript[{str1_String}] := 
  str1;  
parseSupersubscript[{str1_String, str2_String}] :=
  If[StringMatchQ[str1, $superLinePatt],
    parseSupersubscript[{str1,str2,""}],
    parseSupersubscript[{"",str1,str2}]
  ];
parseSupersubscript[{superStr_String, ioStr_String, subStr_String}] :=
  Module[{ioLen, chars, posPattF, aF, posQQ, posSuper, posSub, posSS, 
      finalA, removeTimesPatt},
    ioLen = Max@Map[StringLength, {superStr, ioStr, subStr}];
    chars = Map[
      Join[Characters@#1, Table[" ", ioLen - StringLength@#1] ] &, 
      {superStr, ioStr, subStr}
    ];
    posPattF = {
      Position[{" ", "-", Except[" "]} | {Except[" "], "-", _}],
      Position[{_?(StringMatchQ[DigitCharacter]), " ", _}],
      Position[{_, " ", _?(StringMatchQ[DigitCharacter])}]
    };
    aF = AssociationMap[
      StringTrim[ StringJoin @@@ chars[[{1, 3}, Range @@ #1]] ] &
    ];
    {posQQ, posSuper, posSub} = Table[
      MinMax /@ Split[First/@(FF@Transpose@chars), Abs[#1-#2]<=1 &],
      {FF, posPattF}
    ];
    posSS = Values@Map[Apply[{Min@#1, Max@#2} &]@*Transpose]@Select[
      GroupBy[Join[posSuper, posSub], First],
      Length[#] > 1 &
    ];
    posSuper = DeleteCases[posSuper, {Alternatives @@ posSS[[All, 1]], _}];
    posSub = DeleteCases[posSub, {Alternatives @@ posSS[[All, 1]], _}];
    finalA = Join[
      ("_" <> #2 <> "^" <> #1 <> "*" &) @@@ aF@posSS,
      ("^" <> #1 <> "*" &) @@@ aF@posSuper,
      ("_" <> #2 <> "*" &) @@@ aF@posSub,
      ("(" <> #1 <> "/" <> #2 <> ")*" &) @@@ aF@posQQ
    ];
    removeTimesPatt = ("," | ")" | "}" | " +" | " -" | "[" | EndOfString);
    StringReplacePart[StringJoin@chars[[2]], Values@finalA, Keys@finalA] // 
      StringReplace[{"*" ~~ x : removeTimesPatt :> x}]
  ];


EvaluateM2::tmovrflw = 
"Default maximum evaluation time surpassed. \
Modify $EvaluationTimeOverflowM2 to override the default value.";
SyntaxInformation[EvaluateM2] = {"ArgumentsPattern" -> {_}};
EvaluateM2[cmd_String] :=
  Module[{res = "", buf = "", timeStep=0.02, trimmedRes},
    If[Head[M2] =!= ProcessObject || ProcessStatus@M2 == "Finished", 
      M2 = InitializeM2[$processM2] 
    ];
    WriteLine[M2, cmd];
    CheckAbort[
      TimeConstrained[
        While[!StringMatchQ[buf, ___ ~~ "\n" ~~ "i" ~~ (DigitCharacter ..) ~~ " : "],
          Pause[timeStep];
          buf = ReadString[M2, EndOfBuffer];
          res = StringJoin[res, buf];
          $M2String = StringJoin[$M2String, buf];
        ],
        $EvaluationTimeOverflowM2,
        Message[EvaluateM2::tmovrflw, $EvaluationTimeOverflowM2];
        KillM2[]; Return[$Failed]
      ],
      KillM2[]
    ];
    trimmedRes = StringDelete[res, "\n\n" ~~ $inPatt] // 
      If[$PrintRawM2, Echo, Identity];
    createCell[trimmedRes] // 
      If[$PrintDebugM2, Echo, Identity]
  ];
SetAttributes[EvaluateM2, {Protected, ReadProtected}];


SyntaxInformation[SessionCellsM2] = {"ArgumentsPattern" -> {_.}};
SessionCellsM2[Infinity | All] := 
  SessionCellsM2[];
SessionCellsM2[n_Integer?NonNegative] := 
  Take[SessionCellsM2[], UpTo@n];
SessionCellsM2[n_Integer?Negative] := 
  Reverse@Take[Reverse@SessionCellsM2[], UpTo@Abs@n];
SessionCellsM2[] := 
  Module[{split},
    split = StringSplit[$M2String, "\n\n"];
    groups = Join @@@ Partition[
      SplitBy[split, MatchQ[_String?(StringContainsQ[$inPatt])] ],
      UpTo[2]
    ];
    (createCell@StringRiffle[#,"\n\n"] &) /@ Drop[groups,-1]
  ];
SetAttributes[SessionCellsM2, {Protected, ReadProtected}];


SyntaxInformation[SessionHistoryM2] = {"ArgumentsPattern" -> {_.}};
SessionHistoryM2[Infinity | All] := 
  SessionHistoryM2[];
SessionHistoryM2[n_Integer?NonNegative] := 
  Take[SessionHistoryM2[], UpTo@n];
SessionHistoryM2[n_Integer?Negative] := 
  Reverse@Take[Reverse@SessionHistoryM2[], UpTo@Abs@n];
SessionHistoryM2[] := 
  Module[{cells, cellF},
    cells = SessionCellsM2[];
    cellF = Join[
      {"I"<>ToString[First@#2]<>" : "<>Lookup[#1, "Input"]},
      MapThread[
        (If[MissingQ[#1] || #1 == "", Nothing, #2<>#1] &),
        {
          Lookup[#1, {"Output","Type"}], 
          { "O"<>ToString[First@#2]<>" = ",
            "O"<>ToString[First@#2]<>" : " }
        }] 
    ] &;
    Join @@ MapIndexed[cellF, cells]
  ];
SetAttributes[SessionHistoryM2, {Protected, ReadProtected}];


SyntaxInformation[RawSessionHistoryM2] = {"ArgumentsPattern" -> {}};
RawSessionHistoryM2[] := 
  Module[{},
    $M2String
  ];
SetAttributes[RawSessionHistoryM2, {Protected, ReadProtected}];


End[];


EndPackage[];