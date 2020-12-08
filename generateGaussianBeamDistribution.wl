(* ::Package:: *)

BeginPackage["generateGaussianBeamDistribution`"]


generateGaussianBeamDistribution::usage="generateGaussianBeamDistribution[n,{\!\(\*SubscriptBox[\(\[Epsilon]\), \(x\)]\),\!\(\*SubscriptBox[\(\[Epsilon]\), \(y\)]\),ct,\!\(\*FractionBox[\(\[Delta]p\), \(p\)]\)},{\!\(\*SubscriptBox[\(\[Alpha]\), \(x\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(x\)]\),\!\(\*SubscriptBox[\(\[Alpha]\), \(y\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(y\)]\)},{\!\(\*SubscriptBox[\(\[Eta]\), \(x\)]\),\!\(\*SubscriptBox[\(\[Eta]\), \(x'\)]\),\!\(\*SubscriptBox[\(\[Eta]\), \(y\)]\),\!\(\*SubscriptBox[\(\[Eta]\), \(y'\)]\)},{\!\(\*SubscriptBox[\(C\), \(xx'\)]\),\!\(\*SubscriptBox[\(C\), \(\[Placeholder]\)]\)] plots data P with the fitted fractional emittance ellipse determined by emittanceEllipse. Plot options can be specified.";


Begin["`Private`"]


DispersionMatrix[{dx_,dpx_,dy_,dpy_}]:={{1,0,0,0,0,dx},{0,1,0,0,0,dpx},{0,0,1,0,0,dy},{0,0,0,1,0,dpy},{0,0,0,0,1,0},{0,0,0,0,0,1}}


BetaMatrix[{betx_,bety_}]:={{Sqrt[betx],0,0,0,0,0},{0,Sqrt[1/betx],0,0,0,0},{0,0,Sqrt[bety],0,0,0},{0,0,0,Sqrt[1/bety],0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}}


AlfaMatrix[{alfx_,alfy_}]:={{1,0,0,0,0,0},{-alfx,1,0,0,0,0},{0,0,1,0,0,0},{0,0,-alfy,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}}


CouplingMatrix[{xy_,xyp_,xpy_,xpyp_}]:={{1,0,xy,xyp,0,0},{0,1,xpy,xpyp,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}}


Unprotect[randomVariate];
Clear[randomVariate];
randomVariate[NormalDistribution[0,0],n_]:=ConstantArray[0,n]
randomVariate[dist_,n_]:=RandomVariate[dist,n];
Protect[randomVariate];


eMatrix[{\[Epsilon]x_,\[Epsilon]y_,\[Delta]p_,ct_},n_]:=Block[{x,y},
{x=randomVariate[NormalDistribution[0,Sqrt[\[Epsilon]x]],n],randomVariate[NormalDistribution[0,Sqrt[\[Epsilon]x]],n],y=randomVariate[NormalDistribution[0,Sqrt[\[Epsilon]y]],n],randomVariate[NormalDistribution[0,Sqrt[\[Epsilon]y]],n],
randomVariate[NormalDistribution[0,ct],n],randomVariate[NormalDistribution[0,\[Delta]p],n]}
]


GaussianBeamGenerator[D_,B_,A_,C_,e_]:=D.B.A.C.e


generateGaussianBeamDistribution[n_:1,emittances_List:{1,1,0,10^-3},twiss_List:{0,1,0,1},dispersion_List:{0,0,0,0},coupling_List:{0,0,0,0}]:=Block[{},
Dinitial=DispersionMatrix[dispersion];
Binitial=BetaMatrix[twiss[[{2,4}]]];
Ainitial=AlfaMatrix[twiss[[{1,3}]]];
Cinitial=CouplingMatrix[coupling];
emittance=eMatrix[emittances,n];
Transpose[GaussianBeamGenerator[Dinitial,Binitial,Ainitial,Cinitial,emittance]]
]


End[]


EndPackage[]


beam=generateGaussianBeamDistribution[100,{1,1,0,10^-3},{0,1,0,1},{0,0,0,0},{0,0,0,0}];


Dinitial


Binitial


Ainitial


Cinitial


ListPlot[beam[[All,{1,2}]]]
