(* ::Package:: *)

BeginPackage["genesisLatticeWriter`",{"numberRight`","Madtomma`MADInput`MADInput`"}];


genesisLatticeWriter::usage="genesisLatticeWriter[lattice, unitlength,B\[Rho]] writes a simple genesis lattice using input lattice <lattice> and the parameters <unitlength> and the rigidity <B\[Rho]>."


Begin["`Private`"]


genesisHeader[UNITLENGTH_]:="# header is included
? VERSION= 1.00  including new format
? UNITLENGTH= "<>ToString[numberRight[UNITLENGTH]]<>" :unit length in header
";


genesisDistanceFunction[lattice_]:=Block[{list},
list=Partition[#&[lattice],2,1,1,{}];
Transpose[{lattice,Prepend[Chop[If[Length[#]>1,#[[2,-1]]-#[[1,-1]]-#[[2,3]],Nothing],0.001]&/@list,list[[1,1,-1]]-list[[1,1,3]]]}]
]


genesisAWWriter[lattice_]:=Block[{distancelattice},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@Map["AW  "<>ToString[numberRight[#[[1,4]]]]<>"  "<>ToString[numberRight[#[[1,3]]]]<>"  "<>ToString[numberRight[#[[-1]]]]<>"\n"&,distancelattice]
]


genesisQFWriter[lattice_,B\[Rho]_]:=Block[{distancelattice},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@Map["QF  "<>ToString[numberRight[B\[Rho] #[[1,4]]]]<>"  "<>ToString[numberRight[#[[1,3]]]]<>"  "<>ToString[numberRight[#[[-1]]]]<>"\n"&,distancelattice]
]


genesisADWriter[lattice_]:=Block[{distancelattice},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@MapIndexed["AD  0  "<>ToString[numberRight[#[[-1]]]]<>"  "<>ToString[
	If[#2[[1]]===1,
		numberRight[#[[1,3]]+#[[-1]]],
		numberRight[#[[-1]]]
	]
]<>"\n"&,distancelattice]
]


(* ::Text:: *)
(*genesisADWriter[lattice_] := Block[{distancelattice, splitlattice},*)
(*  distancelattice = genesisDistanceFunction[lattice];*)
(*  (*splitlattice=Map[{{#[[1,1,1]],"Drift",Total[#[[All,1,3]]],"","","","",#[[-1,1,-1]]},Total[#[[All,-1]]]}&,Split[distancelattice,#2[[-1]]==0.&]];*)*)
(*  Print[distancelattice];*)
(*  StringJoin @@ Map["AD  " <> ToString[numberRight[#[[1, 4]]]] <> "  " <> ToString[numberRight[#[[1, 3]]]] <> "  " <> ToString[numberRight[#[[-1]]]] <> "\n" &, distancelattice]*)
(*  ]*)


genesisQXWriter[lattice_,xvals_]:=Block[{},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@Map["QX  "<>ToString[numberRight[#[[2]]]]<>"  "<>ToString[numberRight[#[[1]][[1,3]]]]<>"  "<>ToString[numberRight[#[[1]][[-1]]]]<>"\n"&,Transpose[{distancelattice,xvals}]]
]


genesisQYWriter[lattice_,yvals_]:=Block[{},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@Map["QY  "<>ToString[numberRight[#[[2]]]]<>"  "<>ToString[numberRight[#[[1]][[1,3]]]]<>"  "<>ToString[numberRight[#[[1]][[-1]]]]<>"\n"&,Transpose[{distancelattice,yvals}]]
]


genesisQXYWriter[lattice_,yvals_]:=Block[{},
distancelattice=genesisDistanceFunction[lattice];
StringJoin@@Map["QX  "<>ToString[numberRight[#[[2,1]]]]<>"  "<>ToString[numberRight[#[[1]][[1,3]]]]<>"  "<>ToString[numberRight[#[[1]][[-1]]]]<>"
QY  "<>ToString[numberRight[#[[2,2]]]]<>"  "<>ToString[numberRight[#[[1]][[1,3]]]]<>"  "<>ToString[numberRight[#[[1]][[-1]]]]<>"\n"&,Transpose[{distancelattice,yvals}]]
]


genesisReplaceType[element_List]:=Block[{},
ReplacePart[element,2->Switch[element[[2]],
"GWiggler","GWiggler",
"Quadrupole","Quadrupole",
"Drift","Drift",
_,"Drift"]]
]


genesisLatticeString[lattice_,UNITLENGTH_,B\[Rho]_,opts___]:=Block[{groupings,xerrors,yerrors,xyerrors,genesisstring},
xerrors=Global`XErrors/.{opts}/.{Global`XErrors->{}};
yerrors=Global`YErrors/.{opts}/.{Global`YErrors->{}};
xyerrors=Global`XYErrors/.{opts}/.{Global`XYErrors->{}};
groupings=GroupBy[MADInfo[Map[ReplacePart[genesisReplaceType[#],3->#[[3]]/UNITLENGTH]&,MADFlatten[lattice]],NoTable->True],#[[2]]&];
genesisstring=genesisHeader[UNITLENGTH]<>genesisAWWriter[groupings[["GWiggler"]]]<>genesisQFWriter[groupings[["Quadrupole"]],B\[Rho]]<>genesisADWriter[groupings[["GWiggler"]]];
If[Dimensions[xyerrors]=={Length[groupings[["Quadrupole"]]],2},
	If[Length[xerrors]==Length[groupings[["Quadrupole"]]],xyerrors+=xerrors];
	If[Length[yerrors]==Length[groupings[["Quadrupole"]]],xyerrors+=yerrors];
	genesisstring=genesisstring<>genesisQXYWriter[groupings[["Quadrupole"]],xyerrors];,
	If[Length[xerrors]==Length[groupings[["Quadrupole"]]],
		genesisstring=genesisstring<>genesisQXWriter[groupings[["Quadrupole"]],xerrors]
	];
	If[Length[yerrors]==Length[groupings[["Quadrupole"]]],
		genesisstring=genesisstring<>genesisQYWriter[groupings[["Quadrupole"]],yerrors]
	];
];
genesisstring
]


genesisLatticeWriter[filename_,lattice_,UNITLENGTH_,B\[Rho]_,opts___]:=Block[{file,string},
file=OpenWrite[filename];
string=genesisLatticeString[lattice,UNITLENGTH,B\[Rho],opts];
WriteString[file,string];
Close[file];
]


End[]


EndPackage[]
