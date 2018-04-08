(***********************************************************************
This file was generated automatically by the Mathematica front end.
It contains Initialization cells from a Notebook file, which typically
will have the same name as this file except ending in ".nb" instead of
".m".

This file is intended to be loaded into the Mathematica kernel using
the package loading commands Get or Needs.  Doing so is equivalent to
using the Evaluate Initialiation Cells menu command in the front end.

DO NOT EDIT THIS FILE.  This entire file is regenerated automatically 
each time the parent Notebook file is saved in the Mathematica front end.
Any changes you make to this file will be overwritten.
***********************************************************************)





































































































BeginPackage["Madtomma`Mfs`ColorListPlot`"]



colorListPlot::usage = "colorListPlot[list] behaves like ListPlot but varies the colour (or other attributes) of successive points according to its option ColourScale.\nIf the data list is a list of {x,y,z} values then the points are coloured according to the z value.";

rainbow::usage = 
  "rainbow is the default value of ColorScale, an option for the function \
colorListPlot.  It varies the colour progressively from red to violet.";

grayfade::usage =
  "grayfade is a possible value of ColorScale, an option for the function \
colorListPlot.  It varies the colour progressively from black to a light \
grey.";

ColorScale::usage=
  "ColorScale is an option for colorListPlot.  It is a function mapping the \
interval [0,1] to a graphics directive that will be applied to successive \
points.  It's default is the function rainbow.\nIt is ignored in the case of \
3-dimensional data.";







Options[colorListPlot]=
  Join[Options[Graphics],{ColorScale->rainbow,PlotJoined->False}];

SetOptions[colorListPlot,Prolog->{ PointSize[0.02]},Axes->True,
  Background->RGBColor[1,1,1]];



Begin["`Private`"]



Needs["Utilities`FilterOptions`"]





rainbow[z_]:=Hue[.8 z]

grayfade[z_]:=GrayLevel[.8z]





colorListPlot[x:{{_, _}..},opts___Rule]:=Module[{ptdots,joins,pcolr},
		joins=PlotJoined/.{opts}/.Options[colorListPlot];
		pcolr = ColorScale/.{opts}/.Options[colorListPlot];
		If[joins,
			(	ptdots=
          MapIndexed[(Flatten[{pcolr[First[#2]/Length[x]],Line[#1]}])&,
            Partition[x,2,1]];),
			(	ptdots=
          MapIndexed[(Flatten[{pcolr[First[#2]/Length[x]],Point[#1]}])&,
            x];)];
		Show[Graphics[ptdots],FilterOptions[Graphics, ##]& @@
			      Flatten[{opts, Options[colorListPlot]}]]
		]



colorListPlot[x_/;Length[Dimensions[x]]==1,opts___]:=
  colorListPlot[MapIndexed[{First[#2],#1}&,x],opts]



colorListPlot[data3:{{_, _,_}..},opts___Rule]:=
	Module[{data2,dataz,zhuecolor,ldata,a,b},
		ldata=Length[data3];
		data2=Take[#,2]& /@data3;
		dataz=N[Last[#]& /@ data3];
		a=Min[dataz];b=Max[dataz];
		dataz=(dataz-a)/(b-a);
		zhuecolor[z_]:=Hue[0.8 dataz[[Floor[1+z( ldata-1)]]] ];
		colorListPlot[data2,ColorScale->zhuecolor,opts]
		]



End[ ]









Protect[ colorListPlot]





EndPackage[ ]





































































































































































