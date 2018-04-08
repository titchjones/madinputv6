(* ::Package:: *)

BeginPackage["unsortedUnion`"];


unsortedUnion::usage="unsortedUnion[list] does a Union[list] but retains the original ordering.";


Begin["`Private`"]


unsortedUnion[x_]:=Reap[Sow[1,x],_,#1&][[2]]


End[]


EndPackage[]
