(* ::Package:: *)

(* ::Subsubsection:: *)
(*Testig*)


(* ::Input:: *)
(*Close[#] & /@ Drop[Streams[], 2]*)


(* ::Input:: *)
(*$headers={};*)


(* ::Input::Initialization:: *)
fmtbinrules={"double"->"Real64","long"->"Integer32"}


(* ::Input::Initialization:: *)
$namereplacerules={"/"->""}


(* ::Input:: *)
(*file=OpenRead["nominal.ana.W1",BinaryFormat->True];*)


(* ::Input:: *)
(*AppendTo[$headers,ans=Read[file,Record]];*)


(* ::Input:: *)
(*While[!StringMatchQ[ans,"&data mode=binary, &end"],*)
(*AppendTo[$headers,ans=Read[file,Record]]*)
(*];*)


(* ::Input:: *)
(*parseparametertype[col_]:=If[Length[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&]]>0,assignparameterdata[StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]],StringCases[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&],"fixed_value="~~type:((WordCharacter|"-"|WhitespaceCharacter|"\"")..):>type][[1,1]]];{},StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]]]*)


(* ::Input:: *)
(*parseparametername[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter..)~~","...:>name][[1,1]]*)


(* ::Input:: *)
(*parametertypes=Flatten[parseparametertype[#]&/@Select[$headers,StringMatchQ[#,"&parameter*"]&]]*)


(* ::Input:: *)
(*parameternames=parseparametername[#]&/@Select[$headers,StringMatchQ[#,"&parameter*"]&&!StringMatchQ[#,"*fixed_value*"]&]*)


(* ::Input:: *)
(*parsecolumnname[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter..)~~","...:>name][[1,1]]*)


(* ::Input:: *)
(*parsecolumntype[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]]*)


(* ::Input:: *)
(*columnnames=parsecolumnname[#]&/@Select[$headers,StringMatchQ[#,"&column*"]&]*)


(* ::Input:: *)
(*columntypes=parsecolumntype[#]&/@Select[$headers,StringMatchQ[#,"&column*"]&]*)


(* ::Input:: *)
(*BinaryRead[file,"Byte"]*)


(* ::Input:: *)
(*columns=parameters={};*)
(*norows=BinaryRead[file,"Integer32"];*)
(*While[norows=!=EndOfFile,*)
(*parameters={Sequence@@parameters,Switch[#,*)
(*"string",strlen=BinaryRead[file,"Integer32"];StringJoin@@BinaryReadList[file,"Character8",strlen],*)
(*"double",BinaryRead[file,"Real64"],*)
(*"long",BinaryRead[file,"Integer32"]*)
(*]&/@parametertypes};*)
(*columns={Sequence@@columns,If[MemberQ[columntypes,"string"],*)
(*Table[*)
(*Switch[#,*)
(*"string",strlen=BinaryRead[file,"Integer32"];StringJoin@@BinaryReadList[file,"Character8",strlen],*)
(*"double",BinaryRead[file,"Real64"],*)
(*"long",BinaryRead[file,"Integer32"]*)
(*]&/@columntypes,{norows}],*)
(*columntypesbin=columntypes/.fmtbinrules;*)
(*BinaryReadList[file,columntypesbin,norows]*)
(*]};*)
(*norows=BinaryRead[file,"Integer32"];*)
(*];*)


(* ::Input:: *)
(*parameternames*)


(* ::Input:: *)
(*columnnames*)


(* ::Input:: *)
(*Clear[Evaluate[#]]&/@parameternames;*)
(*If[Length[Dimensions[parameters]]>1,*)
(*Evaluate[Map[Symbol,parameternames]]=Transpose[parameters];,*)
(*Evaluate[Map[Symbol,parameternames]]=parameters];*)


(* ::Input:: *)
(*Dimensions[columns][[1]]===1,*)


(* ::Input:: *)
(*Transpose[Map[Transpose,columns]][[All,1]]*)


(* ::Input:: *)
(*Clear[Evaluate[#]]&/@columnnames;*)
(*If[Dimensions[columns][[1]]===1,*)
(*Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns]][[All,1]];,*)
(*Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns]];]*)


(* ::Text:: *)
(*Close[file];*)


(* ::Section:: *)
(*Package*)


(* ::Input::Initialization:: *)
BeginPackage["sddsBinaryInterpret`"];


(* ::Input::Initialization:: *)
sddsBinaryInterpret::usage="sddsBinaryInterpret[\"filename\"] interprets an SDDS Binary-Formatted File";


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Input::Initialization:: *)
fmtbinrules={"double"->"Real64","long"->"Integer32"}


(* ::Input::Initialization:: *)
$namereplacerules={"/"->""}


(* ::Input::Initialization:: *)
parseparametertype[col_]:=If[Length[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&]]>0,assignparameterdata[StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]],StringCases[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&],"fixed_value="~~type:((WordCharacter|"-"|WhitespaceCharacter|"\"")..):>type][[1,1]]];{},StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]]];


(* ::Input::Initialization:: *)
parseparametername[col_]:=(StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter|"/")..~~","...:>StringReplace[name,$namereplacerules]][[1,1]])


(* ::Input::Initialization:: *)
parsecolumntype[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]]


(* ::Input::Initialization:: *)
parsecolumnname[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter..|"/")~~","...:>StringReplace[name,$namereplacerules]][[1,1]]


(* ::Input::Initialization:: *)
Clear[sddsBinaryInterpret]


(* ::Input::Initialization:: *)
sddsBinaryInterpret[filein_String,opts___]:=Block[{file},
file=OpenRead[filein,BinaryFormat->True];
sddsBinaryInterpretFunction[file,opts];
Close[file];
]


(* ::Input::Initialization:: *)
sddsBinaryInterpret[file_InputStream,opts___]:=Block[{},
sddsBinaryInterpretFunction[file,opts];
Close[file];
]


(* ::Input::Initialization:: *)
sddsBinaryInterpret[string_String,opts___]/;StringMatchQ[string,"!*"]:=Block[{file},
sddsBinaryInterpretFunction[file=StringToStream[Import[string,"String"]],opts];
Close[file];
]


(* ::Input::Initialization:: *)
sddsBinaryInterpretFunction[file_InputStream,opts___]:=Block[{verbose,$headers={},pages=0,parametertypes,parameternames,columnnames,columntypes,prefix,append},
verbose=Global`sddsVerbose/.{opts}/.{Global`sddsVerbose->False};
prefix=Global`sddsPrefix/.{opts}/.{Global`sddsPrefix->""};
append = Global`sddsPostfix /. {opts} /. {Global`sddsPostfix -> ""};
(*file=OpenRead[filein,BinaryFormat\[Rule]True]*);

AppendTo[$headers,ans=Read[file,Record]];

While[!StringMatchQ[ans,"&data mode=* &end"],
AppendTo[$headers,ans=Read[file,Record]]
];

parametertypes=Flatten[parseparametertype[#]&/@Select[$headers,StringMatchQ[#,"&parameter*"]&]];
parameternames=prefix<>parseparametername[#]<>append&/@Select[$headers,StringMatchQ[#,"&parameter*"]&&!StringMatchQ[#,"*fixed_value*"]&];
columnnames=prefix<>parsecolumnname[#]<>append&/@Select[$headers,StringMatchQ[#,"&column*"]&];
columntypes=parsecolumntype[#]&/@Select[$headers,StringMatchQ[#,"&column*"]&];

BinaryRead[file,"Byte"];
columns=parameters={};
norows=BinaryRead[file,"Integer32"];
While[norows=!=EndOfFile,
pages++;
parameters={Sequence@@parameters,Switch[#,
"string",strlen=BinaryRead[file,"Integer32"];StringJoin@@BinaryReadList[file,"Character8",strlen],
"double",BinaryRead[file,"Real64"],
"long",BinaryRead[file,"Integer32"]
]&/@parametertypes};
columns={Sequence@@columns,If[MemberQ[columntypes,"string"],
Table[
Switch[#,
"string",strlen=BinaryRead[file,"Integer32"];StringJoin@@BinaryReadList[file,"Character8",strlen],
"double",BinaryRead[file,"Real64"],
"long",BinaryRead[file,"Integer32"]
]&/@columntypes,{norows}],
columntypesbin=columntypes/.fmtbinrules;
BinaryReadList[file,columntypesbin,norows]
]};
norows=BinaryRead[file,"Integer32"];
];
Clear[Evaluate[#]]&/@parameternames;
If[Length[Dimensions[parameters]]>1,
Evaluate[Map[Symbol,parameternames]]=Transpose[parameters];,
Evaluate[Map[Symbol,parameternames]]=parameters];
Clear[Evaluate[#]]&/@columnnames;
If[Dimensions[columns][[1]]===1,
Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns]][[All,1]];,
Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns]];]
(*Close[file];*)
If[verbose,
Print["Number of Pages: "<>ToString[pages]];
Print["Variables (Re)Assigned: "<>ToString[columnnames]];
Print["Parameters (Re)Assigned: "<>ToString[parameternames]]
]
]


(* ::Input::Initialization:: *)
End[]


(* ::Input::Initialization:: *)
EndPackage[]


(* ::Input:: *)
(*sddsBinaryInterpret["020.0348.001.sdds",Global`sddsVerbose->True,Global`sddsPostfix->"dd"]*)



