(* ::Package:: *)

BeginPackage["MinimumVolumeEllipse`",{"addEpilog`","CCompilerDriver`","CCompilerDriver`GenericCCompiler`"}]


MinimumVolumeEllipse::usage="MinimumVolumeEllipse[P,tolerance] provides the matrix A for determining the Minimum volume ellipse covering the points P, along with the centre point array c."


MinimumVolumeEllipsePlot::usage="MinimumVolumeEllipsePlot[P,tol,plotopts] provides a plot of the Minimum volume ellipse covering the points P.\nMinimumVolumeEllipsePlot[A,c,plotopts] provides a plot of the Minimum volume ellipse covering the points P."


MinimumVolumeEllipsePlotList::usage="MinimumVolumeEllipsePlotList[P,tol,plotopts] provides a plot of the Minimum volume ellipse covering the points p in list P."


MinimumVolumeEllipsePlotDisk::usage="MinimumVolumeEllipse[A,c,plotopts] provides a plot of the Minimum volume ellipse covering the points P."


MinimumVolumeEllipsePlotDiskList::usage="MinimumVolumeEllipse[A,c,plotopts] provides a plot of the Minimum volume ellipse covering the points P."


MinimumVolumeEllipseLogPlotDiskList::usage="MinimumVolumeEllipse[A,c,plotopts] provides a plot of the Minimum volume ellipse covering the points P."


Begin["`Private`"]


Clear[khachiyan];


bsxfun[f_,a_,b_]:=Transpose[MapThread[f,{Transpose[a],b}]]


(*khachiyan[Q_,uin_,d_,tol_]:=Block[{X,M,maximum,j,stepsize,newu,count,err},*)
(*err=2*tol;*)
(*u=uin;*)
(*While[err>tol,*)
(*X=Q.DiagonalMatrix[u].Transpose[Q];*)
(*M=Total[PseudoInverse[X].Q*Q,1];*)
(*maximum=Max[M];*)
(*j=Position[M,maximum][[1,1]];*)
(*stepsize=(maximum-d-1)/((d+1)(maximum-1));*)
(*newu=(1-stepsize)u;*)
(*newu[[j]]=newu[[j]]+stepsize;*)
(*err=Norm[newu-u];*)
(*u=newu;*)
(*];*)
(*u*)
(*]*)


(* ::Text:: *)
(*If[$CCompiler == {},*)
(* $CCompiler = {"Compiler" -> GenericCCompiler, "CompilerInstallation" -> "C:\\TDM-GCC-64\\bin", "CompilerName" -> "x86_64-w64-mingw32-gcc.exe"};*)
(* ]*)


$CCompiler={"Compiler" -> GenericCCompiler, "CompilerInstallation" -> "C:\\TDM-GCC-64\\bin", "CompilerName" -> "x86_64-w64-mingw32-gcc.exe"}


khachiyan=Block[{u,M={{},{}},X={{},{}},j,maximum,stepsize,newu},
Compile[{{Q,_Real,2},{uin,_Real,1},{d,_Integer,0},{tol,_Real}},Module[{err=2tol,count=0},
u=uin;
M={{},{}};
X={{},{}};
While[err>0.01tol,
X=Q.DiagonalMatrix[u].Transpose[Q];
(*M=Diagonal[Transpose[Q].PseudoInverse[X].Q];*)
M=Total[PseudoInverse[X].Q*Q,1];
j=Last[Ordering[M]];
maximum=M[[j]];
stepsize=(maximum-d-1)/((d+1)(maximum-1));
newu=(1-stepsize)u;
newu[[j]]+=stepsize;
err=Norm[u-newu];
u=newu;
];
u
],CompilationTarget->"C",Parallelization->True]
]


(*MinimumVolumeEllipse[P_,tol_]:=Block[{d,Np,Q,count,err,u,U,A,c,ans},*)
(*{d,Np}=Dimensions[P];*)
(*Q=ConstantArray[0,{d+1,Np}];*)
(*Q[[1;;d]]=P[[1;;d,1;;Np]];*)
(*Q[[d+1]]=ConstantArray[1,{Np}];*)
(*count=1;*)
(*err=1;*)
(*u=(1/Np)*ConstantArray[1,{Np}];*)
(*{timing,u}=AbsoluteTiming[khachiyan[N[Q],N[u],d,tol]];*)
(*U = DiagonalMatrix[u];*)
(*A = (1/d)*PseudoInverse[P.U.Transpose[P] - Transpose[{P.u}].{P.u}];*)
(*c=P.u;*)
(*{A,c}*)
(*]*)


cMinimumVolumeEllipse=Module[{u,d,Np,Q,count,err,U},
Compile[{{P,_Real,2},{tol,_Real}},
c=u={};
A=U={{},{}};
{d,Np}=Dimensions[P];
Q=ConstantArray[0,{d+1,Np}];
Q[[1;;d]]=P[[1;;d,1;;Np]];
Q[[d+1]]=ConstantArray[1,{Np}];
count=1;
err=1;
u=(1/Np)*ConstantArray[1,{Np}];
timing=AbsoluteTiming[u=khachiyan[N[Q],N[u],d,tol];];
U = DiagonalMatrix[u];
A = (1/d)*PseudoInverse[P.U.Transpose[P] - Transpose[{P.u}].{P.u}];
c=P.u;
,CompilationTarget->"C",Parallelization->True]]


Clear[MinimumVolumeEllipse];
MinimumVolumeEllipse[P_,tol_]:=Block[{d,Np,Q,count,err,u,U,A,c},
cMinimumVolumeEllipse[P,tol];
{A,c}
]


Clear[MinimumVolumeEllipsePlot];


MinimumVolumeEllipsePlot[A_,c_List,opts___Rule]:=Block[{uu,qq,vv,a,b},
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
theta=Range[0,2\[Pi]+1/20,1/20];
ListPlot[{Transpose[P],c+#&/@Transpose[vv.{a Cos[theta],b Sin[theta]}]},opts,Joined->{False,True},PlotRange->All]
]


(*MinimumVolumeEllipsePlot[P_,tol_Real|tol_Integer,opts___Rule]:=Block[{A,uu,qq,vv,a,b,c},*)
(*{A,c}=MinimumVolumeEllipse[N[P],N[tol]];*)
(*{uu,qq,vv}=SingularValueDecomposition[A];*)
(*{a,b}=1/Sqrt[Diagonal[qq]];*)
(*theta=Range[0,2\[Pi]+1/20,1/20];*)
(*ListPlot[{Transpose[P],c+#&/@Transpose[vv.{a Cos[theta],b Sin[theta]}]},opts,Joined->{False,True},PlotRange->All]*)
(*]*)


MinimumVolumeEllipsePlot[P_,tol_Real|tol_Integer,opts___Rule]:=Block[{data,A,uu,qq,vv,a,b,c},
{A,c}=MinimumVolumeEllipse[N[P],N[tol]];
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
addEpilogGraphics[ListPlot[{Transpose[P]},FilterRules[{opts},Options[ListPlot]],Joined->{False},PlotRange->All],{{Opacity[0.2],Translate[Rotate[Circle[{0,0},{b,a}],{{1,1},vv.{1,1}}],c]}},opts]
]


MinimumVolumeEllipsePlotList[Plist_,tol_Real|tol_Integer,opts___Rule]:=Block[{data,A,uu,qq,vv},
data=Block[{P=#,a,b,c},
{A,c}=MinimumVolumeEllipse[N[P],N[tol]];
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
{Transpose[P],a,b,c,vv.{1,1}}]&/@Plist;
addEpilogGraphics[ListPlot[data[[All,1]],FilterRules[{opts},Options[ListPlot]],PlotRange->All],Block[{p,a,b,c,d},{p,a,b,c,d}=#;{Opacity[0.2],Translate[Rotate[Circle[{0,0},{b,a}],{{1,1},d}],c]}]&/@data,opts]
]


MinimumVolumeEllipsePlotDisk[P_,tol_Real|tol_Integer,opts___Rule]:=Block[{data,A,uu,qq,vv,a,b,c},
{A,c}=MinimumVolumeEllipse[N[P],N[tol]];
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
addGraphics[ListPlot[{Transpose[P]},FilterRules[{opts},Options[ListPlot]],Joined->{False},PlotRange->All],{{Opacity[0.2],Translate[Rotate[Disk[{0,0},{b,a}],{{1,1},vv.{1,1}}],c]}},opts]
]


MinimumVolumeEllipsePlotDiskList[Plist_,tol_Real|tol_Integer,opts___Rule]:=Block[{data,A,uu,qq,vv},
data=Block[{P=#,a,b,c},
{A,c}=MinimumVolumeEllipse[N[P],N[tol]];
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
{Transpose[P],a,b,c,vv.{1,1}}]&/@Plist;
addGraphics[ListPlot[data[[All,1]],FilterRules[{opts},Options[ListPlot]],PlotRange->All],Block[{p,a,b,c,d},{p,a,b,c,d}=#;{Opacity[0.2],Translate[Rotate[Disk[{0,0},{b,a}],{{1,1},d}],c]}]&/@data,opts]
]


MinimumVolumeEllipseLogPlotDiskList[Plist_,tol_Real|tol_Integer,opts___Rule]:=Block[{data,A,uu,qq,vv,logplot,ticks,range,gr},
data=Block[{P=N[#],a,b,c},
{A,c}=MinimumVolumeEllipse[N[Log[P]],N[tol]];
{uu,qq,vv}=SingularValueDecomposition[A];
{a,b}=1/Sqrt[Diagonal[qq]];
{Transpose[Log[P]],a,b,c,vv.{1,1},Transpose[P]}]&/@Plist;
Off[NumberForm::iprf];
logplot=ListLogLogPlot[data[[All,-1]],FilterRules[{opts},PlotRange]];
ticks=Quiet[AbsoluteOptions[logplot,Ticks]][[1,2]];
range=Quiet[AbsoluteOptions[logplot,PlotRange][[1]]];
gr=addEpilog[ListPlot[data[[All,1]],Ticks->ticks,range,FrameTicks->Evaluate@{{ticks[[2]],None},{ticks[[1]],None}},FilterRules[{opts},Options[ListPlot]],PlotRange->All],Block[{p,a,b,c,d,p2},{p,a,b,c,d,p2}=#;{Opacity[0.2],Translate[Rotate[Scale[Disk[{0,0},{1,1}],{b,a}],{{1,1},d}],c]}]&/@data,Sequence@@{range,opts}];
gr
]


End[]


EndPackage[]
