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



(* ::Input::Initialization:: *)
Needs["madtomma`madinput`madinput`"]


(* ::Input::Initialization:: *)
Needs["gdfBinaryInterpret`"]


(* ::Input::Initialization:: *)
Needs["sddsBinaryInterpret`"]


(* ::Input::Initialization:: *)
Clear[GDF2SDDS];
GDF2SDDS[gdffile_,sddsfile_,postfix_:"",range_:All]:=Block[{c,me,MeV,MeVc,gamma,v,p,E0},
headertext="SDDS1
!This file was created by gdf2sdds
&column name=x, units=m, type=double,  &end
&column name=y, units=m, type=double,  &end
&column name=t, units=s, type=double,  &end
&column name=xp, symbol=x', type=double,  &end
&column name=yp, symbol=y', type=double,  &end
&column name=p, units=\"m$be$nc\", type=double,  &end
&data mode=ascii &end
";
Clear[Evaluate["G"<>postfix]];
gdfBinaryInterpret[gdffile,gdfVerbose->True];
c=QuantityMagnitude[UnitConvert[Quantity["SpeedOfLight"]]];
me=QuantityMagnitude[UnitConvert[Quantity["ElectronMass"]]];
MeV=QuantityMagnitude[UnitConvert[Quantity["MeV"]]];
MeVc=QuantityMagnitude[UnitConvert[Quantity["ElectronMass"]Quantity["SpeedOfLight"],"MeV/c"]];
E0=me c^2;
If[ListQ[ToExpression["G"<>postfix]],
gamma=1/Sqrt[1-ToExpression["Bz"<>postfix]^2];,
gamma=1/Sqrt[1-ToExpression["Bz"<>postfix]^2];
];
v=ToExpression["Bz"<>postfix] c;
p=(E0 Sqrt[-1+gamma^2])/(MeV MeVc);
file=OpenWrite[sddsfile<>".ascii"];
WriteString[file,headertext];
Block[{data=#},If[Length[#[[1]]]>0,
sddsP=#[[-1]];
WriteString[file,ToString[Length[#[[1]]]]<>"\n"];
Map[WriteString[file,StringJoin@@MapIndexed[If[#2[[1]]>1," ",""]<>ToString[NumberForm[#,32,NumberFormat->(Row[{#1,"E",#3}]&),ExponentFunction->(3 Quotient[#,3]&)]]&,#]<>"\n"]&,Transpose[data]];
]
]&/@Which[postfix=="t",Sort[Transpose[{ToExpression["x"<>postfix],ToExpression["y"<>postfix],-((#-Mean[#]&/@ToExpression["z"<>postfix])/v),ArcTan[ToExpression["Bx"<>postfix]/ToExpression["Bz"<>postfix]],ArcTan[ToExpression["By"<>postfix]/ToExpression["Bz"<>postfix]],p,ToExpression["z"<>postfix]}],Mean[#1[[-1]]]<Mean[#2[[-1]]]&][[range,1;;6]],
postfix=="p",
Print["p"];
Sort[Select[Transpose[{ToExpression["x"<>postfix],ToExpression["y"<>postfix],(#-Mean[#]&/@ToExpression["t"<>postfix]),ArcTan[ToExpression["Bx"<>postfix]/ToExpression["Bz"<>postfix]],ArcTan[ToExpression["By"<>postfix]/ToExpression["Bz"<>postfix]],p,ToExpression["t"<>postfix]}],Length[#[[-1]]]>0&],Mean[#1[[-1]]]<Mean[#2[[-1]]]&][[range,1;;6]],
postfix=="time",
Sort[Transpose[{ToExpression["x"<>postfix],ToExpression["y"<>postfix],-((#-Mean[#]&/@ToExpression["z"<>postfix])/v),ArcTan[ToExpression["Bx"<>postfix]/ToExpression["Bz"<>postfix]],ArcTan[ToExpression["By"<>postfix]/ToExpression["Bz"<>postfix]],p,ToExpression["z"<>postfix]}],Mean[#1[[-1]]]<Mean[#2[[-1]]]&][[range,1;;6]],
postfix=="position",
If[ListQ[ToExpression["G"]],
gamma=ToExpression["G"];,
gamma=1/Sqrt[1-ToExpression["Bz"]^2];
];
v=ToExpression["Bz"] c;
p=(E0 Sqrt[-1+gamma^2])/(MeV MeVc);
Sort[Transpose[{ToExpression["x"],ToExpression["y"],-(#-Mean[#]&/@ToExpression["t"]),ToExpression["Bx"]/ToExpression["Bz"],ToExpression["By"]/ToExpression["Bz"],p,ToExpression["z"]}],Mean[#1[[-1]]]<Mean[#2[[-1]]]&][[range,1;;6]],
postfix=="",
Sort[Transpose[{ToExpression["x"<>postfix],ToExpression["y"<>postfix],-((#-Mean[#]&/@ToExpression["z"<>postfix])/v),ArcTan[ToExpression["Bx"<>postfix]/ToExpression["Bz"<>postfix]],ArcTan[ToExpression["By"<>postfix]/ToExpression["Bz"<>postfix]],p,ToExpression["z"<>postfix]}],Mean[#1[[-1]]]<Mean[#2[[-1]]]&][[range,1;;6]]];
Close[file];
ReadList["!sddsconvert "<>sddsfile<>".ascii "<>sddsfile<>" -binary"];
(*DeleteFile[sddsfile<>".ascii"]*);
]


(* ::Input::Initialization:: *)
Clear[SDDS2GDF];
SDDS2GDF[sddsfile_,gdffile_,postfix_:"",range_:All,asci2gdfPath_:""]:=Block[{c,me,MeV,gamma,v,p,MeVc,betaz,betay,betax,z,sdds2gdfz,beta},
headertext="x y z Bx By Bz G
";
sddsBinaryInterpret[sddsfile,gdfVerbose->False,sddsPrefix->"sdds2gdf"];
c=QuantityMagnitude[UnitConvert[Quantity["SpeedOfLight"]]];
me=QuantityMagnitude[UnitConvert[Quantity["ElectronMass"]]];
MeV=QuantityMagnitude[UnitConvert[Quantity["MeV"]]];
MeVc=QuantityMagnitude[UnitConvert[Quantity["ElectronMass"]Quantity["SpeedOfLight"],"MeV/c"]];
E0=me c^2;
gamma=Sqrt[1+((sdds2gdfp MeVc)/(E0/MeV))^2];
beta=Sqrt[1-gamma^-2];
betax=Tan[sdds2gdfxp] beta;
betay=Tan[sdds2gdfyp] beta;
betaz=Sqrt[beta^2-betax^2-betay^2];
sdds2gdfz=-betaz  c (sdds2gdft-Mean[sdds2gdft]);
file=OpenWrite[gdffile<>".ascii"];
WriteString[file,headertext];
If[Length[Dimensions[sdds2gdfx]]>1,
Block[{data=#},If[Length[#[[1]]]>0,
Map[WriteString[file,StringJoin@@MapIndexed[If[#2[[1]]>1," ",""]<>ToString[NumberForm[#,32,NumberFormat->(Row[{#1,"E",#3}]&),ExponentFunction->(3 Quotient[#,3]&)]]&,#]<>"\n"]&,Transpose[data]];
]
]&/@Transpose[{sdds2gdfx,sdds2gdfy,sdds2gdfz,betax,betay,betaz,gamma}],
Map[WriteString[file,StringJoin@@MapIndexed[If[#2[[1]]>1," ",""]<>ToString[NumberRight[#]]&,#]<>"\n"]&,Transpose[{sdds2gdfx,sdds2gdfy,sdds2gdfz,betax,betay,betaz,gamma}]]
];
Close[file];
ReadList["!"<>asci2gdfPath<>"asci2gdf -o "<>gdffile<>" "<>gdffile<>".ascii"];
(*DeleteFile[sddsfile<>".ascii"]*);
]


