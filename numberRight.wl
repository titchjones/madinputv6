(* ::Package:: *)

BeginPackage["numberRight`"];


numberRight::usage="numberRight[n,acc] returns number a to <acc> accuracy with no exponent function. Useful for writing to text files.";


Begin["`Private`"]


numberRight[a_,acc_:15]:=NumberForm[N[a],acc,ExponentFunction->(Null&)]


End[]


EndPackage[]
