
BeginPackage["InterfaceM2`Core`"]

Unprotect["InterfaceM2`Core`*"];
ClearAll["InterfaceM2`Core`*"];


InitializeM2::usage = "";
EvaluateM2::usage = "";
RestartM2::usage = "";
KillM2::usage = "";


$PrintRawM2 = False;
$PrintDebugM2 = False;
$EvaluationTimeOverflowM2 = 20;
$InitializeTimeOverflowM2 = 5;


Begin["`Private`"]


$processM2 = Automatic;

$varPatt = LetterCharacter ~~ ((LetterCharacter | DigitCharacter) ...);
$subPatt = $varPatt | (DigitCharacter ..);
$subLinePatt = ($subPatt | " ") ..;
$superPatt = (DigitCharacter ..);
$superLinePatt = ($superPatt | " ") ..;
$outLinePatt = 
  "o" ~~ (DigitCharacter ..) ~~ (" = " | " : ") ~~ Shortest[__];
$superSubOutPatt = $superLinePatt ~~ "\n" ~~ $outLinePatt ~~ 
   "\n" ~~ $subLinePatt;
$subOutPatt = $outLinePatt ~~ "\n" ~~ $subLinePatt;
$superOutPatt =  $superLinePatt ~~ "\n" ~~ $outLinePatt;



InitializeM2::nspec = "M2 process location was never specified. \
Please introduce it as the argument.";
InitializeM2::tmovrflw = 
"Initialization time surpassed. Please verify if Macaulay2 is working properly. \
Modify $InitializeTimeOverflowM2 to override the default value.";

SyntaxInformation[InitializeM2] = {"ArgumentsPattern" -> {_.}};
InitializeM2[proc : (_String | {__String} | Automatic) : Automatic] := 
  Module[{str = "", buf = "", m2, timeStep = 0.01, i = 1},
    If[proc === Automatic,
      If[$processM2 === Automatic, 
        Message[InitializeM2::nspec]; Return[Null]
      ],
      $processM2 = proc
    ];
    m2 = StartProcess[$processM2];
    While[!StringMatchQ[buf = ReadString[m2, EndOfBuffer], 
        ___ ~~ "\ni" ~~ (DigitCharacter ..) ~~ " : "],
      str = StringJoin[str, buf];
      Pause[timeStep];
      If[timeStep * (i++) > $InitializeTimeOverflowM2,
        Message[InitializeM2::tmovrflw, $InitializeTimeOverflowM2];
        KillM2[]; Return[$Failed];    
      ];
    ];
    M2 = m2
  ];
SetAttributes[InitializeM2, {Protected, ReadProtected}];


supersubscriptM2[{superStr_String, ioStr_String, subStr_String}] :=
  Module[{posSuper, posSub, assoc, both, newStr, monomialPatt, afterStarPatt},
    newStr = ioStr<>StringRepeat[" ", 50];
    posSuper = StringPosition[superStr, $superPatt, Overlaps -> False];
    posSub = StringPosition[subStr, $subPatt, Overlaps -> False];
    both = (If[# === {}, {{}, {}}, Transpose@#] &)@Position[
      Outer[Equal[First@#1, First@#2] &, posSub, posSuper, 1, 1], True];
    assoc = Join[
      MapThread[First@MaximalBy[{#1, #2}, Last] -> StringJoin["_", 
          StringTake[subStr, #1], "^", StringTake[superStr, #2], "*"] &, 
        {posSub[[First@both]], posSuper[[Last@both]]} ],
      Table[ p -> StringJoin["_", StringTake[subStr, p], "*"],
        {p, Delete[posSub, Map[List]@First@both]} ],
      Table[ p -> StringJoin["^", StringTake[superStr, p], "*"],
        {p, Delete[posSuper, Map[List]@Last@both]} ]
    ];
    afterStarPatt = (" " | "," | ")" | "}" | EndOfString);
    monomialPatt = (DigitCharacter | LetterCharacter | "_" | "^" | "+" | "*") ..;
    StringReplace[
      StringTrim@StringReplacePart[newStr, Values@assoc, Keys@assoc],
      (n:RepeatedNull[DigitCharacter]) ~~ (s:monomialPatt) ~~ 
        "*" ~~ (f:afterStarPatt) :> If[n == "", "", n<>"*"] <> s <> f
    ]
  ];


parseLineM2[out_String] :=
  Module[{RP},
    RP = ReplaceAll[{
      s_String?(StringMatchQ[$superSubOutPatt]) :> 
        supersubscriptM2[ StringSplit[s, "\n"] ],
      s_String?(StringMatchQ[$superOutPatt]) :> 
        supersubscriptM2[ StringSplit[s, "\n"]~Join~{""} ],
      s_String?(StringMatchQ[$subOutPatt]) :> 
        supersubscriptM2[ {""}~Join~StringSplit[s, "\n"] ]
    }];
    RP@StringSplit[out, "\n\n"]
  ];


EvaluateM2::tmovrflw = 
"Evaluation time surpassed. \
Modify $EvaluationTimeOverflowM2 to override the default value.";

SyntaxInformation[EvaluateM2] = {"ArgumentsPattern" -> {_}};
EvaluateM2[cmd_String] :=
  Module[{str = "", i=1, timeStep=0.02},
    If[Head[M2] =!= ProcessObject || ProcessStatus@M2 == "Finished", 
      M2 = InitializeM2[$ProcessM2] 
    ];
    WriteLine[M2, cmd];
    While[!StringMatchQ[str, ___ ~~ "\ni" ~~ (DigitCharacter ..) ~~ " : "],
      Pause[timeStep];
      str = StringJoin[ str, ReadString[M2, EndOfBuffer] ];
      If[timeStep * (i++) > $EvaluationTimeOverflowM2,
        Message[EvaluateM2::tmovrflw, $EvaluationTimeOverflowM2];
        KillM2[]; Return[$Failed];    
      ];
    ];
    StringDelete[str, "\n\ni" ~~ (DigitCharacter ..) ~~ " : "] // 
      If[$PrintRawM2, Echo, Identity] // 
      parseLineM2 // 
      If[$PrintDebugM2, Echo, Identity]
  ];
SetAttributes[EvaluateM2, {Protected, ReadProtected}];


SyntaxInformation[RestartM2] = {"ArgumentsPattern" -> {}};
RestartM2[] := 
  Module[{},
    If[ FailureQ@EvaluateM2["restart;"],
      Return[Null],
      Return[$Failed]
    ];
  ];
SetAttributes[RestartM2, {Protected, ReadProtected}];


SyntaxInformation[KillM2] = {"ArgumentsPattern" -> {}}
KillM2[] := 
  Module[{},
    KillProcess[M2];
    Return[Null];
  ];
SetAttributes[KillM2, {Protected, ReadProtected}];


End[];


EndPackage[];