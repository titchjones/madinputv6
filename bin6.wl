(* ::Package:: *)

BeginPackage["bin6`"];


bin6::usage="bin6[b,c]\nbin6 is used to put data lists back in the right order. \"c\" provides the position data and \"b\" is the actual data";


Begin["`Private`"]


bin6[b_,c_]:=b[[#]]&/@Split[Ordering[c],c[[#1]]==c[[#2]]&];


End[]


EndPackage[]
