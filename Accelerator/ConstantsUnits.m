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



BeginPackage["Accelerator`ConstantsUnits`",{"Notation`","Units`","PhysicalConstants`"}]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Alpha]", "s"],
"NotationTemplateTag"]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["r", "e"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Lambda]", "e"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox[
OverscriptBox["\[Lambda]", "_"], "e"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["G", "e"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Mu]", "0"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Epsilon]", "0"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "e"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "p"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "n"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "d"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "\[Mu]"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "Z"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["m", "W"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["a", "\[Infinity]"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Mu]", "B"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Mu]", "N"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["r", "p"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["Z", "0"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Sigma]", "T"],
"NotationTemplateTag",
Editable->True]\)]


Symbolize[\!\(\*
TagBox[
SubscriptBox["\[Sigma]", "SB"],
"NotationTemplateTag",
Editable->True]\)]


ConstantsUnits::usage ="ConstantsUnits.m is a package that provides notations and values for constants occurring in accelerator physics."


TeV::usage="TeV is a unit of energy.";
GeV::usage="GeV is a unit of energy.";
MeV::usage="MeV is a unit of energy.";
keV::usage="keV is a unit of energy.";


c::usage="c is a symbol for the speed of light in vacuum; it evaluates numerically with N[ ].";


\[HBar]::usage="\[HBar] is a symbol for the reduced Planck constant; it evaluates numerically with N[ ].";


Subscript[\[Alpha], s]::usage="\!\(\*SubscriptBox[\(\[Alpha]\), \(s\)]\) is a symbol for the fine structure constant; it evaluates numerically with N[ ].";


e::usage="e is a symbol for the electron charge; it evaluates numerically with N[ ].";


Subscript[r, e]::usage="\!\(\*SubscriptBox[\(r\), \(e\)]\) is a symbol for the classical radius of the electron; it evaluates numerically with N[ ].";


Subscript[\[Lambda], e]::usage="\!\(\*SubscriptBox[\(\[Lambda]\), \(e\)]\) is a symbol for the Compton wavelength of the electron; it evaluates numerically with N[ ].";


Subscript[
\!\(\*OverscriptBox[\(\[Lambda]\), \(_\)]\), e]::usage="\!\(\*SubscriptBox[OverscriptBox[\(\[Lambda]\), \(_\)], \(e\)]\) is a symbol for the reduced Compton wavelength of the electron; it evaluates numerically with N[ ].";


Subscript[G, e]::usage="\!\(\*SubscriptBox[\(G\), \(e\)]\) is a symbol for the anomalous magnetic moment of the electron; it evaluates numerically with N[ ].";


Subscript[\[Mu], 0]::usage="\\ \ \ TagBox[\!\(\*SubscriptBox[\(\[Mu]\), \(0\)]\),\ NotationBoxTag,\ Editable->True] is a symbol for the permeability of the vacuum; it evaluates numerically with N[ ].";


Subscript[\[Epsilon], 0]::usage="\ TagBox[\!\(\*SubscriptBox[\(\[Epsilon]\), \(0\)]\),NotationBoxTag,Editable->True] is a symbol for the permittivity of the vacuum; it evaluates numerically with N[ ].";


Subscript[m, e]::usage="\!\(\*SubscriptBox[\(m\), \(e\)]\) is a symbol for the mass of the electron; it evaluates numerically with N[ ].";


Subscript[m, p]::usage="\!\(\*SubscriptBox[\(m\), \(p\)]\) is a symbol for the mass of the proton; it evaluates numerically with N[ ].";


Subscript[m, n]::usage="\!\(\*SubscriptBox[\(m\), \(n\)]\) is a symbol for the mass of the neutron; it evaluates numerically with N[ ].";


Subscript[m, d]::usage="\!\(\*SubscriptBox[\(m\), \(d\)]\) is a symbol for the mass of the deuteron; it evaluates numerically with N[ ].";


Subscript[m, \[Mu]]::usage="SubscriptBox[m,\[Mu] \ ] is a symbol for the mass of the muon; it evaluates numerically with N[ ].";


Subscript[m, Z]::usage="\!\(\*SubscriptBox[\(m\), \(Z\)]\) is a symbol for the mass of the Z-particle; it evaluates numerically with N[ ].";


Subscript[m, W]::usage="SubscriptBox[m,W \ \ ]is a symbol for the mass of the W-particle; it evaluates numerically with N[ ].";


Subscript[a, \[Infinity]]::usage="\:f3aais a symbol for the Bohr radius; it evaluates numerically with N[ ].";


Subscript[\[Mu], B]::usage="\:f3aais a symbol for the Bohr magneton; it evaluates numerically with N[ ].";


Subscript[\[Mu], N]::usage="\!\(\*SubscriptBox[\(\[Mu]\), \(N\)]\) is a symbol for the nuclear magneton; it evaluates numerically with N[ ].";


Subscript[r, p]::usage="\!\(\*SubscriptBox[\(r\), \(p\)]\) is a symbol for the classical radius of the proton; it evaluates numerically with N[ ].";


Subscript[Z, 0]::usage="\!\(\*SubscriptBox[\(Z\), \(0\)]\) is a symbol for the impedance of free space; it evaluates numerically with N[ ].";


Subscript[\[Sigma], T]::usage="\!\(\*SubscriptBox[\(\[Sigma]\), \(T\)]\) is a symbol for the Thomson cross section; it evaluates
numerically with N[ ].";


Subscript[\[Sigma], SB]::usage="\!\(\*SubscriptBox[\(\[Sigma]\), \(SB\)]\) is a symbol for the Stefan-Boltzmann constant; it evaluates numerically with N[ ].";


ToFundamentalSI::usage="toFundamentalSI[f] converts the quantity f to fundamental SI units.  Generally this helps to simplify the expressions of the units.";


ClassicalProtonRadius::usage="ClassicalProtonRadius is the classical radius of the proton."


DimensionCheck::usage="DimensionCheck[f] returns an expression showing the dimensional consistency of f, based on the internal database of symbols and units maintained by the ConstantsUnits package.\nThe alternative form DimensionCheck[f,symbolsUnits] uses a list of {symbol,unit} pairs given as second argument instead of the internal database.";


NumericalValueUnits::usage="NumericalValueUnits[f] interprets f as the product of a numerical value fn and symbolic units fu and returns {fn,fu}.";


PhysicalUnits::usage="PhysicalUnits[f] interprets f as the product of a numerical value fn and symbolic units fu and returns fu.";


NumericalValue::usage="NumericalValue[f] interprets f as the product of a numerical value fn and symbolic units fu and returns fn.";


IntroduceSymbol::usage="IntroduceSymbol[symbol,usagetext,units] introduces a symbol, registers its units for purposes of dimensional analysis and defines a usage message for it.";


SymbolsUnits::usage="SymbolsUnits[] returns a list of all symbols and units registered in the internal database.";


PhysicalUnitsPlot::usage="PhysicalUnitsPlot[f,{x,xmin,xmax}] works like Plot but removes the physical units of f and x before plotting.  Further, it creates labels for the axes from the symbolic forms of f and x and indicates their units.  It has all options of Plot except AxesLabel.";


ConstantsUnits::badarg = "You called `1` with argument `2`!"


SpeedOfLight=299792458 Meter/Second


ThomsonCrossSection=0.66524616/10^28Meter^2


ClassicalProtonRadius=Convert[ElectronCharge^2/((4\[Pi] VacuumPermittivity) ProtonMass SpeedOfLight^2),Meter]


Begin["`Private`"]


minGodelPrimeIndex = 10;


ToFundamentalSI[f_]:=SI[f]//.Units`Private`$ToFundamental


DimensionCheck[f_,symbolsWithUnits_List]:=Expand[f//.MapIndexed[(#1[[1]]->Prime[minGodelPrimeIndex+First[#2]] #1[[2]]&),Union[symbolsWithUnits]]];
DimensionCheck[f_]:=DimensionCheck[f,Accelerator`ConstantsUnits`Private`$SymbolsUnits]


TeV= Tera ElectronVolt;GeV=Giga ElectronVolt;
MeV=Mega ElectronVolt;keV=Kilo ElectronVolt;eV=ElectronVolt;


N[c]=SpeedOfLight


N[\[HBar]]=PlanckConstantReduced


N[Subscript[\[Alpha], s]]=FineStructureConstant


N[e]=ElectronCharge


N[Subscript[r, e]]=ClassicalElectronRadius


N[Subscript[\[Lambda], e]]=ElectronComptonWavelength


N[Subscript[
\!\(\*OverscriptBox[\(\[Lambda]\), \(_\)]\), e]]=N[ElectronComptonWavelength/(2 \[Pi])]


N[Subscript[G, e]]=If[$VersionNumber>=4.1,(-ElectronGFactor-2)/2,ElectronGFactor-1]


N[Subscript[\[Mu], 0]]=Convert[VacuumPermeability,Henry/Meter]


N[Subscript[\[Epsilon], 0]]=Convert[VacuumPermittivity,Farad/Meter]


N[Subscript[m, e]]=ElectronMass


N[Subscript[m, p]]=ProtonMass


N[Subscript[m, n]]=NeutronMass


N[Subscript[m, d]]=DeuteronMass


N[Subscript[m, \[Mu]]]=MuonMass


N[Subscript[m, Z]]=Convert[N[91.187GeV/(SpeedOfLight^2)],Kilogram]


N[Subscript[m, W]]=Convert[N[80.4GeV/(SpeedOfLight^2)],Kilogram]


N[Subscript[a, \[Infinity]]]=BohrRadius



N[Subscript[\[Mu], B]]=Convert[N[(ElectronCharge PlanckConstantReduced)/(2 ProtonMass)],Joule/Tesla]


N[Subscript[\[Mu], N]]=Convert[N[(ElectronCharge PlanckConstantReduced)/(2 ProtonMass)],Joule/Tesla]


N[Subscript[r, p]]=ClassicalProtonRadius


N[Subscript[Z, 0]]=Convert[N[VacuumPermeability SpeedOfLight],Ohm]


N[Subscript[\[Sigma], T]]=ThomsonCrossSection


N[Subscript[\[Sigma], SB]]=StefanConstant


NumericalValueUnits[Times[a_/;NumericQ[a],b__]]:={a,Times@b};
NumericalValueUnits[a_/;NumericQ[a]]:={a,1};
NumericalValueUnits[a_]:={1,a}


NumericalValue[f_]:=First[NumericalValueUnits[f]]


PhysicalUnits[f_]:=Last[NumericalValueUnits[f]]


SetAttributes[PhysicalUnitsPlot,HoldAll];
Options[PhysicalUnitsPlot]=Options[Plot];
PhysicalUnitsPlot[f_,{var_,varmin_,varmax_},opts___?OptionQ]/;PhysicalUnits[ToFundamentalSI[varmin]]==PhysicalUnits[ToFundamentalSI[varmax]]:=
	Module[{vunits,funits,nvar},
		vunits=PhysicalUnits[N[varmin]];funits=PhysicalUnits[N[f/.var->varmin]];
		fnew=f/.var->nvar vunits;
		If[FreeQ[funits,Plus],Plot[Evaluate[fnew/funits],{nvar,ToFundamentalSI[varmin/vunits],ToFundamentalSI[varmax/vunits]},AxesLabel->{StandardForm[HoldForm[var] [vunits]],StandardForm[HoldForm[f][ funits]]},opts],Print["Units of ordinate appear to be inconsistent."]]]


$SymbolsUnits=({#1,PhysicalUnits[N[#1]]}&)/@ToExpression[Names["Accelerator`ConstantsUnits`*"]]


$SymbolsUnits=Select[$SymbolsUnits,First[#1]=!=Last[#1]&& !(NumericQ[First[#1]/Last[#1]])&]


IntroduceSymbol[sym_Symbol,usagetext_String,unit_]:=($SymbolsUnits=Append[Select[$SymbolsUnits,(First[#1]=!=sym)&],
{sym,unit}];
sym::"usage"=ToString[sym]<>" "<>usagetext<>" (units: "<>ToString[unit]<>")")


SymbolsUnits[]:=$SymbolsUnits


(* Sin/: Sin[x_]^2 := 1 - Cos[x]^2 *)


(* Protect[ Evaluate[protected] ] *)


End[ ]


Protect[Evaluate[$Context <> "*"]]


EndPackage[ ]


(*If[$Remote,Null,NotebookOpen["\\\\srdserve1\\astec\\software\\mathematica\\accelerator\\ConstantsUnitsPalette.nb"]]*)



