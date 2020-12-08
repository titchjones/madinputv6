(* ::Package:: *)

BeginPackage["emittanceEllipse`",{"numberRight`","addEpilog`","Optimise`SimplexOptimise`","CCompilerDriver`","CCompilerDriver`GenericCCompiler`"}]


emittanceEllipse::usage="emitans=emittanceEllipse[emitans,P,tolerance,fraction,opts] attempts to find the minimum area ellipse that covers <\!\(\*
StyleBox[\"fraction\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\">\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"%\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)of the beam distribution.
The function uses a Simplex-based optimiser, and is thus not guarenteed to be a global minimum. Data \!\(\*
StyleBox[\"P\",\nFontSlant->\"Italic\"]\) should be in the form {<x data>,<px data>} and should be centered on {0,0}.
The function can be re-initialised with the previous answer as a starting point using the form shown here. The number of simplex runs can be set using option MaxIterations->n.
The function returns: {{emittance [m-rad], fraction},{major axis, minor axis, rotation},{\[Alpha],\[Beta],\[Gamma]}}
Example: 
sddsBinaryInterpret[\"test.0337.sdds\"];
P={x,xp};
emitans=emittanceEllipse[emitans,P,\!\(\*SuperscriptBox[\(10\), \(-6\)]\),0.95];";


emittanceEllipsePlot::usage="emittanceEllipsePlot[P,emitans,plotopts] plots data P with the fitted fractional emittance ellipse determined by emittanceEllipse. Plot options can be specified.";


emittanceGaussian::usage="emittanceGaussian[P] returns emittance, ellipse parameters and Twiss parameters for the data P using Covariance matrices, which are valid for normally distributed phase-spaces."


Begin["`Private`"]


(* ::Text:: *)
(*If[$CCompiler == {},*)
(* $CCompiler = {"Compiler" -> GenericCCompiler, "CompilerInstallation" -> "C:\\TDM-GCC-64\\bin", "CompilerName" -> "x86_64-w64-mingw32-gcc.exe"};*)
(* ]*)


(* ::Text:: *)
(*$CCompiler = CCompilers[][[1]]*)


cellipse=Compile[{{x,_Real},{y,_Real},{a,_Real},{b,_Real},{\[Alpha],_Real},{x0,_Real},{y0,_Real}},
(((x-x0)*Cos[\[Alpha]]+(y-y0)*Sin[\[Alpha]])/a)^2+(((x-x0)*Sin[\[Alpha]]-(y-y0)*Cos[\[Alpha]])/b)^2,CompilationTarget->"C",Parallelization->True
];


(* ::Text:: *)
(*Clear[cellipse]*)
(*cellipse = Compile[{{x, _Real}, {y, _Real}, {a, _Real}, {b, _Real}, {\[Alpha], _Real}, {x0, _Real}, {y0, _Real}},*)
(*   (((x - x0)*Cos[\[Alpha]] + (y - y0)*Sin[\[Alpha]])/a)^2 + (((x - x0)*Sin[\[Alpha]] - (y - y0)*Cos[\[Alpha]])/b)^2, CompilationTarget -> "C", Parallelization -> True*)
(*   ];*)


Clear[cellipse]
cellipse = Compile[{{x, _Real, 1}, {y, _Real, 1}, {a, _Real}, {b, _Real}, {\[Alpha], _Real}, {x0, _Real}, {y0, _Real}},
   (((x - x0)*Cos[\[Alpha]] + (y - y0)*Sin[\[Alpha]])/a)^2 + (((x - x0)*Sin[\[Alpha]] - (y - y0)*Cos[\[Alpha]])/b)^2, CompilationTarget -> "C", Parallelization -> True
   ];


Clear[cellipseParticles]
cellipseParticles = Compile[{{x, _Real, 1}, {y, _Real, 1}, {a, _Real}, {b, _Real}, {\[Alpha], _Real}, {x0, _Real}, {y0, _Real}},
Transpose[{x,y,(((x - x0)*Cos[\[Alpha]] + (y - y0)*Sin[\[Alpha]])/a)^2 + (((x - x0)*Sin[\[Alpha]] - (y - y0)*Cos[\[Alpha]])/b)^2}], CompilationTarget -> "C", Parallelization -> True
];


Clear[cellipseCut];
cellipseCut=Compile[{{M,_Real,2},{a,_Real},{b,_Real},{\[Alpha],_Real},{x0,_Real},{y0,_Real}},
Select[cellipse[M[[1]],M[[2]],a,b,\[Alpha],x0,y0],#<=1&],CompilationTarget->"C",Parallelization->True
];


Clear[cellipseCutParticles];
cellipseCutParticles=Compile[{{M,_Real,2},{a,_Real},{b,_Real},{\[Alpha],_Real},{x0,_Real},{y0,_Real}},
Select[cellipseParticles[M[[1]],M[[2]],a,b,\[Alpha],x0,y0],#[[-1]]<=1&][[All,{1,2}]],CompilationTarget->"C",Parallelization->True
];


Clear[minFunc];
minFunc=Compile[{{P,_Real,2},{a,_Real},{b,_Real},{c,_Real},{x0,_Real},{px0,_Real},{confidence,_Real},{normaliser,_Real}},
Abs[(\[Pi] a b)/normaliser]+1000(N[(Length[cellipseCut[P,N[Abs[a]],N[Abs[b]],N[c],x0,px0]]/.{0->1})/Length[P[[1]]]]-confidence)^2,CompilationTarget->"C",Parallelization->True
];


Clear[emittanceEllipseOptimise];


emittanceEllipseOptimise[{ain_Real,bin_Real,cin_Real, x0in_Real:0, px0in_Real:0},P_,tol_,confidence_,opts___Rule]:=Block[{a=ain,b=bin,c=cin,x0=x0in,px0=px0in,func, simplexans,normaliser,maxcount,count=0,x,y,z},
maxcount=Global`Iterations/.{opts}/.{Global`Iterations->20};
simplexans={a,b,c,x0,px0};
normaliser=confidence \[Pi] (Max[P[[1]]]-Min[P[[1]]])(Max[P[[2]]]-Min[P[[2]]]);
func=minFunc[P, Sequence @@ #, confidence,normaliser] &;
simplexfit=func[{ain,bin,cin,x0,px0}];
{simplexfit, simplexans} = SimplexOptimise3[Length[simplexans], {\[Pi]{Min[P[[1]]],Max[P[[1]]]},\[Pi]{Min[P[[2]]],Max[P[[2]]]},{0,2\[Pi]},{Min[P[[1]]],Max[P[[1]]]},{Min[P[[2]]],Max[P[[2]]]}}, 1000, func, Verbose -> False,Global`StartSimplex->simplexans];
Prnt[simplexans];
count+=1;
While[Abs[(\[Pi] a b)/normaliser] simplexfit>tol&&count<maxcount,
  {simplexfit, simplexans} = SimplexOptimise3[Length[simplexans], {0.5,2}#&/@simplexans, 1000, func, Verbose -> False,Global`StartSimplex->simplexans];
count+=1;
];
simplexans]


emittanceEllipseOptimise[P_,tol_,confidence_,opts___Rule]:=Block[{func,simplexfit, simplexans,normaliser,a, b, c, x0, px0},
normaliser=confidence \[Pi] (Max[P[[1]]]-Min[P[[1]]])(Max[P[[2]]]-Min[P[[2]]]);
func=minFunc[P, Sequence @@ #, confidence,normaliser] &;
{simplexfit, simplexans} = SimplexOptimise3[5, {{0, Max[P[[1]]]}, {0, Max[P[[2]]]}, {-0.1,0.1}, {Min[P[[1]]],Max[P[[1]]]},{Min[P[[2]]],Max[P[[2]]]}}, 1000, func, Verbose -> False,Global`StartSimplex->simplexans];
  {a, b, c, x0, px0} = simplexans;
emittanceEllipseOptimise[{a,b,c, x0, px0},P,tol,confidence,opts]
]


emittanceEllipseOptimise[{a_,b_,c_,rest___},P_,tol_,confidence_,opts___Rule]/;NumericQ[{a,b,c}]===False:=emittanceEllipseOptimise[P,tol,confidence,opts]


emittanceEllipseOptimise[Null,P_,tol_,confidence_,opts___Rule]/;NumericQ[{a,b,c}]===False:=emittanceEllipseOptimise[P,tol,confidence,opts]


Clear[emittanceEllipseParameters];


emittanceEllipseParameters[P_,{a_,b_,c_,x0_:0.,px0_:0.},confidence_]:=Block[{xans,yans,emit,\[Beta],\[Gamma],\[Alpha],conf},
xans=1/Sqrt[Cos[c]^2/a^2+Sin[c]^2/b^2];
yans=1/Sqrt[Cos[c]^2/b^2+Sin[c]^2/a^2];
emit=Abs[(a b)];
\[Beta]=emit/yans^2;
\[Gamma]=emit/xans^2;
\[Alpha]=Sign[c] Sqrt[\[Beta] \[Gamma]-1];
conf=N@(Length[cellipseCut[P, N[Abs[a]], N[Abs[b]], N[c], x0, px0]]/Length[P[[1]]]);
{{emit,conf},{a,b,c, x0, px0},{\[Alpha],\[Beta],\[Gamma]}}
]


Clear[emittanceEllipse];


emittanceEllipse[{a_,b_,c_,x0_:0.,px0_:0.},P_,tol_,confidence_,opts___Rule]/;NumericQ[{a,b,c}]===False&&ListQ[{a,b,c}]===False:=Block[{},
emittanceEllipse[P,tol,confidence,opts]
]


emittanceEllipse[Null,P_,tol_,confidence_,opts___Rule]:=Block[{},
emittanceEllipse[P,tol,confidence,opts]
]


emittanceEllipse[a_Symbol,P_,tol_,confidence_,opts___Rule]:=Block[{},
emittanceEllipse[P,tol,confidence,opts]
]


emittanceEllipse[{{emit_,conf_},{a_Real,b_Real,c_Real, x0_Real:0., px0_Real:0.},{\[Alpha]_,\[Beta]_,\[Gamma]_}},P_,tol_,confidence_,opts___Rule]:=Block[{},
emittanceEllipse[{a,b,c,x0,px0},P,tol,confidence,opts]
]


emittanceEllipse[{ain_Real,bin_Real,cin_Real, x0in_Real:0., px0in_Real:0.},P_,tol_,confidence_Real | confidence_Integer,opts___Rule]:=Block[{a=ain,b=bin,c=cin,x0=x0in,px0=px0in,xans,yans,emit,\[Beta],\[Gamma],\[Alpha]},
{a,b,c,x0,px0}=emittanceEllipseOptimise[{ain,bin,cin, x0in, px0in},P,tol,confidence,opts];
emittanceEllipseParameters[P,{a,b,c,x0,px0},confidence]
]


emittanceEllipse[P_,tol_,confidence_Real | confidence_Integer,opts___Rule]:=Block[{a,b,c,x0=0,px0=0,xans,yans,emit,\[Beta],\[Gamma],\[Alpha]},
{a,b,c,x0,px0}=emittanceEllipseOptimise[P,tol,confidence,opts];
emittanceEllipseParameters[P,{a,b,c,x0,px0},confidence]
]


emittanceGaussian[P_]:=Block[{cov,eigenvalues,eigenvectors,maxeigenvecpos,maxeigenvec,maxeigenval,angle,ellipseparams,twissparams},
cov=Covariance[Transpose[P]];
{eigenvalues,eigenvectors}=Eigensystem[cov];
maxeigenvecpos=Position[eigenvalues,Max[eigenvalues]][[1,1]];
maxeigenvec=eigenvectors[[maxeigenvecpos]];
maxeigenval=eigenvalues[[maxeigenvecpos]];
angle=ArcTan[maxeigenvec[[1]],maxeigenvec[[2]]];
ellipseparams=Join[Sqrt[eigenvalues],{angle, 0., 0.}];
twissparams={-#[[1,2]],#[[1,1]],#[[2,2]]}&[cov/Sqrt[Det[cov]]];
{{Sqrt[Det[cov]],N[1-Exp[-1^2/2]]},ellipseparams,twissparams}
]


Clear[emittanceEllipsePlot];
emittanceEllipsePlot[P_,{{emit_,confidence_},{a_,b_,c_,x0_,px0_},{\[Alpha]_,\[Beta]_,\[Gamma]_}},opts___] := Block[{in,out,\[Epsilon]=emit/\[Pi]},
in=cellipseCutParticles[P, N[Abs[a]], N[Abs[b]], N[c], x0, px0];
out=Complement[Transpose[P],in];
addGraphics[ListPlot[{in,out}, FilterRules[{opts}, Options[ListPlot]], Joined -> {False}, PlotRange -> All,PlotLabel->ToString[N[100 Length[in]/Length[P[[1]]]]]<>"% Emittance = "<>ToString[numberRight[ 10^6 emit,5]]<>" mm-mrad"], {{Opacity[0.2], Translate[Rotate[Disk[{0,0}, Abs@{a, b}], c], {x0, px0}]
(*,Red,Opacity[1],PointSize[0.02],Point[-{Sqrt[\[Epsilon] \[Beta]],-\[Alpha]Sqrt[\[Epsilon]/\[Beta]]}],Point[-{-\[Alpha]Sqrt[\[Epsilon]/\[Gamma]],Sqrt[\[Epsilon] \[Gamma]]}],Point[-{0,Sqrt[\[Epsilon]/\[Beta]]}],Point[-{Sqrt[\[Epsilon]/\[Gamma]],0}]*)}}, opts]
  ]


End[]


EndPackage[]
