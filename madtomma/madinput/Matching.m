(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["Madtomma`MADInput`Matching`",{"Optimise`SimplexOptimise`","Optimise`EPOptimisev3`"}];


Coupled::usage="Coupled[parameter,pos,weight]\nCouples the points at pos using parameter."


EqualTo::usage="Equals[parameter1,parameter2,weight]\nWeights the difference in parameter1 and parameter2."


LessThan::usage="LessThan[parameter,upperlimit,weight]\nWeights according to whether parameter is greater than upperlimit."


SmallerThan::usage="SmallerThan[parameter,upperlimit,weightbad,weightgood]\nWeights according to whether parameter is greater than upperlimit. If less, returns a negative value weighted with weightgood."


GreaterThan::usage="GreaterThan[parameter,lowerlimit,weight]\nWeights according to whether parameter is less than lowerlimit."


BiggerThan::usage="BiggerThan[parameter,lowerlimit,weightbad,weightgood]\nWeights according to whether parameter is less than lowerlimit. If greater, returns a negative value weighted with weightgood."


Between::usage="Between[parameter,lowerlimit,upperlimit,weight]\nWeights according to whether parameter is between lowerlimt and upperlimit."


EqualToClosest::usage="EqualsClosest[parameter,equalsList,weight]\nWeights according to how close parameter is to the nearest value of equalsList."


Constraints::usage="Constraints[constraintsList]\nReturns a root sum square of the constraints listed in constraintsList.\nConstraints should be in the form {<constraintType>,parameter1,...}."


ConstraintsIndividual::usage="Constraints[constraintsList]\nReturns a list of the constraint values listed in constraintsList.\nConstraints should be in the form {<constraintType>,parameter1,...}."


Begin["`Private`"]


Coupled[parameters_,pos_Integer,weight_]:=If[(ans=Chop[Total[Abs[#[[1]]-Rest[#]]]&[Map[#[[pos]]&,parameters]],10^-4])===0,0,Abs[weight ans]^2]


Coupled[parameter_,pos_List,weight_]:=If[(ans=Chop[Total[Abs[#[[1]]-Rest[#]]]&[parameter[[pos]]],10^-4])===0,0,Abs[weight ans]^2]


If[$VersionNumber<10.3,EqualTo[parameter1_,parameter2_,weight_]:=If[(ans=Chop[parameter1-parameter2])===0,0,Abs[weight ans]^2]]


If[$VersionNumber<10.3,EqualTo[parameter1_List,parameter2_List,weight_]:=If[(ans=Total[Abs[Chop[parameter1-parameter2]]])===0,0,Abs[weight ans]^2]]


If[$VersionNumber<10.3,LessThan[parameter1_,parameter2_,weight_]:=If[parameter1<parameter2,0,(weight Abs[(parameter1-parameter2)])^2]]


If[$VersionNumber<10.3,LessThan[parameter1_List,parameter2_,weight_]:=If[And@@(#<=parameter2&/@parameter1),0,(weight Total[If[#<parameter2,0,Abs[#-parameter2]]&/@parameter1])^2]]


If[$VersionNumber<10.3,SmallerThan[parameter_,limit_,weightbad_,weightgood_:1]:=If[parameter<limit,-(weightgood  (limit-parameter))^2,(weightbad  (parameter-limit))^2]]


If[$VersionNumber<10.3,SmallerThan[parameter_List,limit_,weightbad_,weightgood_:1]:=-Total[If[#<limit,(weightgood  (limit-#)),(weightbad  (#-limit))]&/@parameter]^2]


If[$VersionNumber<10.3,GreaterThan[parameter1_,parameter2_,weight_]:=If[parameter1>parameter2,0,Abs[weight(parameter2-parameter1)]^2]]


If[$VersionNumber<10.3,GreaterThan[parameter1_List,parameter2_,weight_]:=If[And@@(#>=parameter2&/@parameter1),0,(weight Total[If[#>parameter2,0,Abs[parameter2-#]]&/@parameter1])^2]]


If[$VersionNumber<10.3,BiggerThan[parameter_,limit_,weightbad_,weightgood_:1]:=If[parameter>limit,-(weightgood  (parameter-limit))^2,(weightbad  (limit-parameter))^2]]


If[$VersionNumber<10.3,Between[parameter_,lowerlimit_,upperlimit_,weight_]:=(If[parameter>upperlimit,parameter-upperlimit,If[parameter<lowerlimit,lowerlimit-parameter,0]]weight)^2]


If[$VersionNumber<10.3,EqualToClosest[parameter_,equals_List,weight_]:=Min[If[(ans=Chop[parameter-#])===0,0,(weight ans)^2]&/@equals]]


If[$VersionNumber>=10.3,
analyseConstraints[constraintsList_List]:=Switch[#[[1]],
LessThan,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[LessThan[#][input[[3]]],(input[[4]]Abs[(#-input[[3]])]),0]&/@input[[2]]]^2],
If[LessThan[#[[2]]][#[[3]]],(#[[4]]Abs[#[[2]]-#[[3]]])^2,0]
],
SmallerThan,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[LessThan[#][input[[3]]],(input[[4]]Abs[(#-input[[3]])])^2,(input[[5]]Abs[(input[[3]]-#)])]&/@input[[2]]]^2],
If[LessThan[#[[2]]][#[[3]]],(#[[4]]Abs[#[[2]]-#[[3]]])^2,-(#[[5]]Abs[#[[3]]-#[[2]]])^2]
],
GreaterThan,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[GreaterThan[#][input[[3]]],(input[[4]]Abs[(#-input[[3]])]),0]&/@input[[2]]]^2],
If[GreaterThan[#[[2]]][#[[3]]],(#[[4]]Abs[#[[2]]-#[[3]]])^2,0]
],
BiggerThan,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[GreaterThan[#][input[[3]]],(input[[4]]Abs[(#-input[[3]])])^2,(input[[5]]Abs[(input[[3]]-#)])]&/@input[[2]]]^2],
If[GreaterThan[#[[2]]][#[[3]]],(#[[4]]Abs[#[[2]]-#[[3]]])^2,-(#[[5]]Abs[#[[3]]-#[[2]]])^2]
],
EqualTo,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[!EqualTo[#][input[[3]]],(input[[4]]Abs[(#-input[[3]])]),0]&/@input[[2]]]^2],
If[!EqualTo[#[[2]]][#[[3]]],(#[[4]]Abs[#[[2]]-#[[3]]])^2,0]
],
Between,
If[ListQ[#[[2]]],
Block[{input=#},
Total[If[!EqualTo[#][input[[3]]],input[[4]]Abs[(#-input[[3]])],0]&/@input[[2]]]^2],
If[!Between[#[[2]],{#[[3]],#[[4]]}],If[GreaterThan[#[[4]]][#[[2]]],#[[2]]-#[[4]],#[[3]]-#[[2]]]^2,0]
]
]&/@constraintsList]


If[$VersionNumber<10.3,
Constraints[constraints_List]:=If[#>0,Sqrt[#],-Sqrt[Abs[#]]]&[Plus@@(First[#][Sequence@@Rest[#]]&/@constraints)],
Constraints[constraints_List]:=If[#>0,Sqrt[#],-Sqrt[Abs[#]]]&[Plus@@analyseConstraints[constraints]]
]


If[$VersionNumber<10.3,
ConstraintsIndividual[constraints_List]:=(If[#>0,Sqrt[#],-Sqrt[Abs[#]]]&[#[[1]][Sequence@@Rest[#]]]&/@constraints),
ConstraintsIndividual[constraints_List]:=N[(If[#>0,Sqrt[#],-Sqrt[Abs[#]]]&/@analyseConstraints[constraints])]
]


End[]


EndPackage[]



