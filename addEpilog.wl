(* ::Package:: *)

BeginPackage["addEpilog`"]


addEpilog::usage="addEpilog[plot, epilog, opts] adds graphics <epilog> to plot <plot> with additional options <opts>.";


addGraphics::usage="addGraphics[plot, gr, opts] shows graphics <gr> to plot <plot> with additional options <opts>.";


Begin["`Private`"]


addEpilog[g_Legended,epi_List,opts___Rule]:=Quiet@With[{styles=Cases[g[[1]],{dir__,__GeometricTransformation|__Point|__Line}:>Directive[dir],-5]},
Show[g,Epilog->Flatten[{styles,epi},{2}],FilterRules[{opts},Options[Show]]]]


addEpilog[g_Graphics,epi_List,opts___Rule]:=Quiet@With[{styles=Cases[g[[1]],{dir__,__GeometricTransformation|__Point|__Line}:>Directive[dir],-5]},
Show[g,Epilog->Flatten[{styles,epi},{2}],FilterRules[{opts},Options[Show]]]]


addGraphics[g_Legended,epi_List,opts___Rule]:=Quiet@With[{styles=Cases[g[[1]],{dir__,__GeometricTransformation|__Point|__Line}:>Directive[dir],-5]},
Show[g,Graphics[Flatten[{styles,epi},{2}]],FilterRules[{opts},Options[Show]],PlotRange->All]]


addGraphics[g_Graphics,epi_List,opts___Rule]:=Quiet@With[{styles=Cases[g[[1]],{dir__,__GeometricTransformation|__Point|__Line}:>Directive[dir],-5]},
Show[g,Graphics[Flatten[{styles,epi},{2}]],FilterRules[{opts},Options[Show]],PlotRange->All]]


End[]


EndPackage[]
