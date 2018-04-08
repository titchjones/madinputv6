(* ::Package:: *)

BeginPackage["CLARAErrors`",{"DatabaseLink`","numberRight`"}]


insertErrorSet::usage="";


readErrorSet::usage="";


applyErrorSet::usage="";


listErrorSet::usage="";


updateErrorSet::usage="";


insertResult::usage="";


readResult::usage="";


getErrorSetID::usage="";


Begin["`Private`"];


Clear[mySQLConvert];
mySQLConvert[a_List]:=ToString[numberRight[a]]
mySQLConvert[a_Rational]:=N[a]
mySQLConvert[a_String]:=a
mySQLConvert[a_]:=a


Clear[mySQLRevert];
mySQLRevert[a_String]:=a
mySQLRevert[a_String]/;StringMatchQ[a,"{*"]:=ToExpression[a]
mySQLRevert[a_]:=a


errorSetValues={"$Errorid","$ErrorSetName","A","$NoBPMReadings","$BPMOffsetErrorAmplitude","$BPMReadErrorAmplitude","$BPMSystematicCalibrationErrorAmplitude","$BPMRandomCalibrationErrorAmplitude","$BPMSystematicRotationAmplitude","$QuadrupoleOffsetErrorAmplitude","$QuadrupoleMoverSystematicCalibrationErrorAmplitude","$QuadrupoleMoverRandomCalibrationErrorAmplitude","$QuadrupoleMoverErrorAmplitude","$SystematicLaunchPositionErrorAmplitude","$SystematicLaunchAngleErrorAmplitude","$RandomLaunchPositionErrorAmplitude","$RandomLaunchAngleErrorAmplitude","$DispersionRangeJitter","$TikhonovValueListX","$TikhonovValueListY","$DispersionRange"};


CloseSQLConnection[conn];
conn=OpenSQLConnection["Docker MySQL"];


unsortedUnion[x_]:=Reap[Sow[1,x],_,#1&][[2]]


errorColumns=unsortedUnion[Select[SQLColumnNames[conn,"Errors"][[All,2]],#=!="id"&]];


resultsColumns=unsortedUnion[Select[SQLColumnNames[conn,"Results"][[All,2]],#=!="id"&]];


insertErrorSet[data_List]:=SQLInsert[conn,"Errors",errorColumns,mySQLConvert/@data]


readErrorSet[name_String]:=mySQLRevert/@SQLSelect[conn,"Errors",SQLColumn["Name"]==name][[1]]


readErrorSet[id_Integer]:=mySQLRevert/@SQLSelect[conn,"Errors",SQLColumn["id"]==id][[1]]


getErrorSetID[errorset_String]:=SQLSelect[conn,"Errors",SQLColumn["id"],SQLColumn["Name"]==errorset][[1,1]]


getErrorSetID[]:=SQLSelect[conn,"Errors",{"id","Name","BPMReadErrorAmplitude"},"SortingColumns"->{
SQLColumn["BPMReadErrorAmplitude"]->"Ascending"
}]


applyErrorSet[id_Integer]:=Block[{},
Block[{},
Clear[Evaluate[#[[1]]]];Evaluate[Symbol@Evaluate[Evaluate[#[[1]]]]]=#[[2]]
]&/@Transpose[{errorSetValues,readErrorSet[id]}]
]


applyErrorSet[name_String]:=Block[{},
Block[{},
Clear[Evaluate[#[[1]]]];Evaluate[Symbol@Evaluate[Evaluate[#[[1]]]]]=#[[2]]
]&/@Transpose[{errorSetValues,readErrorSet[name]}]
]


listErrorSet[]:=SQLSelect[conn,"Errors",SQLColumn["Name"]]


updateErrorSet[data_List]:=SQLUpdate[conn,"Errors",errorColumns,mySQLConvert/@data]


updateErrorSet[data_List,name_]:=SQLUpdate[conn,"Errors",errorColumns,mySQLConvert/@data,SQLColumn["Name"]==name]


insertResult[errorset_String,seed_Integer,iteration_Integer,results_List]:=Block[{errorid},
errorid=getErrorSetID[errorset];
SQLInsert[conn,"Results",resultsColumns,mySQLConvert/@{errorid,seed,iteration,results}]
]


insertResult[errorset_Integer,seed_Integer,iteration_Integer,results_List]:=SQLInsert[conn,"Results",resultsColumns,mySQLConvert/@{errorset,seed,iteration,results}]


readResult[errorset_Integer,seed_Integer,iteration_Integer]:=SQLSelect[conn,"Results",SQLColumn["Data"],SQLColumn["ErrorSet"]==errorset&&SQLColumn["Seed"]==seed&&SQLColumn["Iteration"]==iteration]


readResult[errorset_Integer,seed_Integer]:=SQLSelect[conn,"Results",SQLColumn["Data"],SQLColumn["ErrorSet"]==errorset&&SQLColumn["Seed"]==seed]


formatResult[sqldata_]:=Sort[#,#1[[2]]<#2[[2]]&]&/@Split[Sort[mySQLRevert/@#&/@sqldata,#1[[1]]<#2[[1]]&],#1[[1]]==#2[[1]]&]


readResult[errorset_Integer,seed_Integer,iteration_Integer]:=formatResult[SQLSelect[conn,"Results",{"Seed","Iteration","Data"},SQLColumn["ErrorSet"]==errorset&&SQLColumn["Seed"]==seed&&SQLColumn["Iteration"]==iteration]][[1,1,-1]]


readResult[errorset_Integer,seed_Integer]:=formatResult[SQLSelect[conn,"Results",{"Seed","Iteration","Data"},SQLColumn["ErrorSet"]==errorset&&SQLColumn["Seed"]==seed]][[1,All,-1]]


readResult[errorset_Integer]:=formatResult[SQLSelect[conn,"Results",{"Seed","Iteration","Data"},SQLColumn["ErrorSet"]==errorset]][[All,All,-1]]


End[]


EndPackage[]
