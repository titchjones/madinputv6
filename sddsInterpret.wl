(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["sddsInterpret`"];


(* ::Input::Initialization:: *)
sddsInterpret::usage="sddsInterpret[\"filename\"] interprets an SDDS Formatted File";


(* ::Text::Initialization:: *)
(*Print["SDDS 22nd January 2009 12:21"]*)


(* ::Input::Initialization:: *)
Begin["`Private`"]


(* ::Input::Initialization:: *)
replacementrules={};


(* ::Input::Initialization:: *)
fmtrules={"long"->Number,"double"->Real,"float"->Real,"string"->Record,"short"->Number};


(* ::Input::Initialization:: *)
removeunwantedlines[file_]:=Block[{orig,final},
orig=ReadList[file,{Word},WordSeparators->{"\n"}];
(orig=Drop[orig,{#}])&/@Reverse[Flatten[Position[orig,#]&/@FindList[file,"!",AnchoredSearch->True],1][[All,1]]];
Flatten[orig]]


(* ::Input::Initialization:: *)
parsecolumnline[line_,opts___]:=Block[{name={},description={},type={},units={},prepend,append},
prepend = Global`sddsPrefix /. {opts} /. {Global`sddsPrefix -> ""};
append = Global`sddsPostfix /. {opts} /. {Global`sddsPostfix -> ""};
If[StringQ[#],name=#]&/@Extract[ll,Flatten[Position[Flatten[ll],"&column name"]+1]];
If[StringQ[#],description=#]&/@Extract[ll,Flatten[Position[Flatten[ll]," description"]+1]];
If[StringQ[#],type=#]&/@Extract[ll,Flatten[Position[Flatten[ll]," type"]+1]];
If[StringQ[#],units=#]&/@Extract[ll,Flatten[Position[Flatten[ll]," units"]+1]];
{prepend<>name<>append,description,type,units}
];


(* ::Input::Initialization:: *)
parsecolumns[columns_,opts___]:=Block[{list,tt,ll},
(tt=StringToStream[StringReplace[#,{" = "->"=","  "->" ","_"->"$"}]];
ll=ReadList[tt,{Word},WordSeparators->{"\n",",","="}];Close[tt];
parsecolumnline[ll,opts])&/@columns/.fmtrules
]


(* ::Input::Initialization:: *)
columndata[rawdata_,columns_]:=Block[{tt,cols,ll},
cols=parsecolumns[columns];
Transpose[Flatten[(ll={ReadList[tt=StringToStream[StringReplace[#,{"1.#QNAN0000000000e+000"->"1.00e+000","-nan"->"0.00e+000"}]],cols[[All,3]],RecordSeparators->" "][[1]]};Close[tt];ll)&/@rawdata,1]]
]


(* ::Input::Initialization:: *)
assigndata[column_,columndata_,opts___]:=Block[{capitals},
capitals=Global`MakeCapitals/.{opts}/.{Global`MakeCapitals->False};
Clear[Evaluate["Global`"<>If[capitals,ToUpperCase[ToString[column]],ToString[column]]]];
Evaluate[Symbol@("Global`"<>If[capitals,ToUpperCase[ToString[column]],ToString[column]])]=columndata;
];


(* ::Input::Initialization:: *)
parseparameterline[line_,opts___]:=Block[{name={},description={},type={},units={},symbol={},prepend,append},
prepend = Global`sddsPrefix /. {opts} /. {Global`sddsPrefix -> ""};
append = Global`sddsPostfix /. {opts} /. {Global`sddsPostfix -> ""};
If[StringQ[#],name=StringReplace[#,{"/"->""}]]&/@Extract[ll,Flatten[Position[Flatten[ll],"&parameter name"]+1]];
If[StringQ[#],description=StringReplace[#,{"/"->""}]]&/@Extract[ll,Flatten[Position[Flatten[ll]," description"]+1]];
If[StringQ[#],type=StringReplace[#,{"/"->""}]]&/@Extract[ll,Flatten[Position[Flatten[ll]," type"]+1]];
If[StringQ[#],units=StringReplace[#,{"/"->""}]]&/@Extract[ll,Flatten[Position[Flatten[ll]," units"]+1]];
If[StringQ[#],symbol=StringReplace[#,{"/"->""}]]&/@Extract[ll,Flatten[Position[Flatten[ll]," symbol"]+1]];
{prepend<>name<>append,description,type,units,symbol}
];


(* ::Input::Initialization:: *)
parseparameters[parameters_,opts___]:=Block[{list,tt,ll},
(tt=StringToStream[StringReplace[#,{" = "->"=","  "->" "}]];
ll=ReadList[tt,{Word},WordSeparators->{"\n",",","="}];Close[tt];
parseparameterline[ll,opts])&/@parameters/.fmtrules
]


(* ::Input::Initialization:: *)
parameterdata[file_,page_]:=Block[{tt,pars,final={},ll},
AppendTo[final,Take[orig,{Flatten[Position[orig,#]&/@pages[[{#}]],1][[1,1]]+1,Flatten[Position[orig,#]&/@pages[[{#}]],1][[1,1]]+Length[parameters]}]]&/@page;
Flatten[final,2]
]


(* ::Input::Initialization:: *)
parameterdata2[rawdata_]:=Block[{tt,cols,ll,ans},
cols=parseparameters[parameters];
Transpose[Partition[Flatten[(ans=ReadList[tt=StringToStream[StringReplace[#[[1]],"1.#QNAN0000000000e+000"->"1.00e+000"]],#[[2]]];Close[tt];ans)&/@MapThread[List,{Flatten[rawdata],Flatten[Table[cols[[All,3]],{Length[rawdata]/Length[cols[[All,3]]]}]]}]],Length[cols[[All,3]]]]]
]


(* ::Input::Initialization:: *)
assignparameterdata[parameter_,parameterdata_,opts___]:=Block[{capitals},
capitals=Global`MakeCapitals/.{opts}/.{Global`MakeCapitals->False};
Clear[Evaluate["Global`"<>If[capitals,ToUpperCase[ToString[parameter]],ToString[parameter]]]];
Evaluate[Symbol@("Global`"<>If[capitals,ToUpperCase[ToString[parameter]],ToString[parameter]])]=parameterdata;
];


(* ::Input::Initialization:: *)
numberofpages[file_]:=Block[{},Length[FindList[ToString[file],"! page number",AnchoredSearch->True]]]


(* ::Input::Initialization:: *)
getfullcolumn[file_,pos_]:=Block[{a,b,c},a=ReadList[file,Word,WordSeparators->{"\n"}];b=a[[pos]];c=pos;While[StringTake[StringReplace[a[[c]],{" "->""}],-4]=!="&end",b=StringInsert[b,a[[c+1]],-1];c+=1;];b]


(* ::Input::Initialization:: *)
getfullcolumn[file_,pos_List]:=Block[{a},a=ReadList[file,Word,WordSeparators->{"\n"}];
Block[{b,c},
b=a[[#]];
c=pos;
While[!StringMatchQ[b,"*&end"],b=StringInsert[b,a[[c+1]],-1];c+=1;];b]&/@pos]


(* ::Input::Initialization:: *)
pagelengths[file_]:=Block[{pages,parameters},
pages=FindList[ToString[file],"! page number",AnchoredSearch->True];
parameters=FindList[ToString[file],"&parameter",AnchoredSearch->True];
{Flatten[Position[orig,#]&/@pages,1][[All,1]],Flatten[Position[orig,#]&/@pages,1][[All,1]]+Length[parameters]+2}]


(* ::Input::Initialization:: *)
selectpage[file_,page_]:=Block[{final={}},
If[pages=={-1},
(AppendTo[final,Take[orig,{Position[orig,{"&data mode=ascii &end"}][[1,1]]+Length[parameters]+1,-1}]]),
(AppendTo[final,Take[orig,{Flatten[Position[orig,#]&/@pages[[{#}]],1][[1,1]]+Length[parameters]+2,If[#>=Length[pages],-1,Flatten[Position[orig,#]&/@pages[[{#+1}]],1][[1,1]]-1]}]])&/@page];
(*Print[final];*)
Flatten[final,2]]


(* ::Input::Initialization:: *)
sddsBinaryRead[filein_String,opts___Rule]:=Block[{file},
file=OpenRead[filein,BinaryFormat->True];
sddsBinaryInterpretFunction[file,opts];
Close[file];
]


(* ::Input::Initialization:: *)
sddsInterpret::badpage="Page `1` is not less than or equal to number of pages. Returning All pages.";


(* ::Input::Initialization:: *)
sddsInterpret::badpages="Pages `1` are not less than or equal to number of pages. Returning valid pages.";


(* ::Input::Initialization:: *)
sddsBinaryInterpretFunction[file_InputStream,opts___]:=Block[{verbose,$headers={},pages=0,parametertypes,parameternames,columnnames,columntypes,prefix,append,parseparametertype,assignparameterdata,parseparametername,parsecolumntype,parsecolumnname,columns,parameters,norows,strlen,columntypesbin,ans,choosepages,capitals,choosepageorig,choosepage},
capitals=Global`MakeCapitals/.{opts}/.{Global`MakeCapitals->False};
verbose=Global`sddsVerbose/.{opts}/.{Global`sddsVerbose->False};
prefix=Global`sddsPrefix/.{opts}/.{Global`sddsPrefix->""};
append = Global`sddsPostfix /. {opts} /. {Global`sddsPostfix ->""};
fmtbinrules={"double"->"Real64","long"->"Integer32"};
$namereplacerules={"/"->""};
parseparametertype[col_]:=If[Length[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&]]>0,assignparameterdata[StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]],StringCases[Select[StringSplit[col,{","," "}],StringMatchQ[#1,"*fixed_value=*"]&],"fixed_value="~~type:((WordCharacter|"-"|WhitespaceCharacter|"\"")..):>type][[1,1]]];{},StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]]];
parseparametername[col_]:=(StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter|"/")..~~","...:>StringReplace[name,$namereplacerules]][[1,1]]);
parsecolumntype[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"type=*"]&],"type="~~type:(WordCharacter..)~~","...:>type][[1,1]];
parsecolumnname[col_]:=StringCases[Select[StringSplit[col],StringMatchQ[#1,"name=*"]&],"name="~~name:(WordCharacter..|"/")~~","...:>StringReplace[name,$namereplacerules]][[1,1]];

AppendTo[$headers,ans=Read[file,Record]];

While[!StringMatchQ[ans,"&data mode=* &end"],
AppendTo[$headers,ans=Read[file,Record]]
];
parametertypes=Flatten[parseparametertype[#]&/@Select[$headers,StringMatchQ[#,"&parameter*"]&]];
parameternames=prefix<>parseparametername[#]<>append&/@Select[$headers,StringMatchQ[#,"&parameter*"]&&!StringMatchQ[#,"*fixed_value*"]&];
parameternames=If[capitals,ToUpperCase[ToString[#]],ToString[#]]&/@parameternames;
columnnames=prefix<>parsecolumnname[#]<>append&/@Select[$headers,StringMatchQ[#,"&column*"]&];
columnnames=If[capitals,ToUpperCase[ToString[#]],ToString[#]]&/@columnnames;
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
choosepage=Global`ChoosePage/.{opts}/.{Global`ChoosePage->Range[Length[pages]]};
choosepageorig=choosepage;
choosepage=If[ListQ[choosepage],choosepage,{choosepage}];
validpages=Select[choosepage,#<=pages&];
If[validpages=!=choosepage,Message[sddsInterpret::badpages,Complement[choosepage,validpages]]];
choosepage=validpages;
If[choosepage=={},choosepage=Range[pages]];
Clear[Evaluate[#]]&/@parameternames;
If[Length[Dimensions[parameters]]>1,
Evaluate[Map[Symbol,parameternames]]=Transpose[parameters[[choosepage]]];,
Evaluate[Map[Symbol,parameternames]]=parameters];
Clear[Evaluate[#]]&/@columnnames;
If[Dimensions[columns][[1]]===1,
Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns]][[All,1]];,
Evaluate[Map[Symbol,columnnames]]=Transpose[Map[Transpose,columns[[choosepage]]]];
];
If[verbose,
Print["Number of Pages: "<>ToString[pages]<>";\tCurrent Page(s): "<>ToString[choosepage]];
Print["Variables (Re)Assigned: "<>ToString[columnnames]];
Print["Parameters (Re)Assigned: "<>ToString[parameternames]]
]
]


(* ::Input::Initialization:: *)
Clear[sddsInterpret];
sddsInterpret[file_,opts___]:=Block[{filedata,choosepage,verbose,capitals,ans,prepend,append,binary=False,pages,orig,choosepageorig},
prepend = Global`sddsPrefix /. {opts} /. {Global`sddsPrefix -> ""};
append = Global`sddsPostfix /. {opts} /. {Global`sddsPostfix -> ""};
verbose=Global`sddsVerbose/.{opts}/.{Global`sddsVerbose->False};
capitals=Global`MakeCapitals/.{opts}/.{Global`MakeCapitals->False};
pages=If[(ans=FindList[ToString[file],"! page number",AnchoredSearch->True])=={},{-1},ans];
orig=ReadList[file,{Word},WordSeparators->{"\n"}];
parameters=getfullcolumn[file,Union[Flatten[(Position[orig,#]&/@FindList[ToString[file],"&parameter",AnchoredSearch->True]),1][[All,1]]]];
columns=getfullcolumn[file,Union[Flatten[(Position[orig,#]&/@FindList[ToString[file],"&column",AnchoredSearch->True]),1][[All,1]]]];
data=getfullcolumn[file,Union[Flatten[(Position[orig,#]&/@FindList[ToString[file],"&data",AnchoredSearch->True]),1][[All,1]]]];
If[StringMatchQ[data[[1]],"*mode=binary*"],
binary=True;
sddsBinaryRead[file,opts];,
choosepage=Global`ChoosePage/.{opts}/.{Global`ChoosePage->Range[Length[pages]]};
choosepageorig=choosepage;
choosepage=If[ListQ[choosepage],choosepage,{choosepage}];
validpages=Select[choosepage,#<=Length[pages]&];
If[validpages=!=choosepage,Message[sddsInterpret::badpages,Complement[choosepage,validpages]]];
choosepage=validpages;
If[choosepage=={},choosepage=Range[Length[pages]]];
If[Length[pages]>1,
assigndata[#[[1]],#[[2]],opts]&/@MapThread[List,{parsecolumns[columns,opts][[All,1]],Transpose[columndata[selectpage[file,{#}],columns]&/@choosepage]}],
assigndata[#[[1]],#[[2]],opts]&/@MapThread[List,{parsecolumns[columns,opts][[All,1]],columndata[selectpage[file,choosepage],columns]}]
]
];
(*Print[selectpage[file,choosepage]];*)
If[Length[parameters]>0,
If[!StringMatchQ[data[[1]],"*mode=binary*"],
assignparameterdata[#[[1]],#[[2]],opts]&/@MapThread[List,{parseparameters[parameters,opts][[All,1]],parameterdata2[parameterdata[file,choosepage]]}];
]
];
If[verbose&&Not[binary],
Print["Number of Pages: "<>ToString[numberofpages[file]]<>";\tCurrent Page(s): "<>ToString[choosepage]];
Print["Variables (Re)Assigned: "<>ToString[If[capitals,ToUpperCase[#],#]&[parsecolumns[columns,opts][[All,1]]]]];
Print["Parameters (Re)Assigned: "<>ToString[If[capitals,ToUpperCase[#],#]&[parseparameters[parameters,opts][[All,1]]]]]
];
]


(* ::Input::Initialization:: *)
End[]


(* ::Input::Initialization:: *)
EndPackage[]
