
BeginPackage["InterfaceM2`Utils`", {
    "InterfaceM2`Core`"
}]


Unprotect["InterfaceM2`Utils`*"];
ClearAll["InterfaceM2`Utils`*"];


ToSubtractList::usage = "";


Begin["`Private`"]


SyntaxInformation[ToSubtractList] = {"ArgumentsPattern" -> {_}};
ToSubtractList[{}] = {};
ToSubtractList[ expr : _Equal ] := ToSubtractList[{expr}];
ToSubtractList[ expr : (List|And)[Except[_List]...] ] := 
  Map[ Through@*If[MatchQ[_Equal], Apply[Subtract], Identity],
    List @@ expr
  ];
SetAttributes[ToSubtractList, {Protected, ReadProtected}];


End[]


EndPackage[]