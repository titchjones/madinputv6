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



BeginPackage["Madtomma`Mfs`TrackTable`",
{"Madtomma`Mfs`Mfs`",
\!\(InputForm\`"\<Madtomma`Algebra`Symplectic`\>"\),
"Miscellaneous`SIUnits`",
"Statistics`DataManipulation`"}]


TrackTable::usage = "TrackTable.m is a package for working with TRACK tables saved by MAD. The complete contents of such tables are held in TrackTable[] objects.";


SurvivalTable::usage = "SurvivalTable[] objects contain information on the initial conditions of tracked particles and the number of turns they survive.  They are often derived from TrackTable[] objects.";


makeTrackTable::usage="makeTrackTable[mfsdata] transforms an mfs data object created from a MAD TRACK table into a trackTable data object.";


sJ3::usage ="sJ3 is the 6 by 6 symplectic unit matrix in 3 dimensions.";


mfsColumn::usage =ToString[mfsColumn::usage]<>
		"\n mfsColumn[TrackTabledata,n,colname] extracts a list of coordinates of particle n labelled by the string colname from a TrackTable data object. A list of colnames may also be given to return a set of lists.  If colname is absent the entire block of coordinates is returned. If the particle number is absent a list of the requested column for all the particles is returned.\n mfsColumn[SurvivalData,colname], mfsColumn[SurvivalData,particle,colname], mfsColumn[SurvivalData] are the obvious extensions to SurvivalTable objects.";


notLost::usage ="notLost[list] takes elements from the beginning of list until a non-numerical item is encountered.";


strobe::usage ="strobe[list,{int1,int2}] samples list giving every int1-th element starting from the element number int2.";


intensity::usage ="intensity[TrackTabledata] returns a list of fractions of +the initial number of particles left after each turn.";


turnsSurvived::usage ="turnsSurvived[TrackTabledata,particle] returns the number of turns made by the particle before being lost.";


toNormalCoords::usage ="toNormalCoords[TrackTabledata] transforms a TrackTable data object into another TrackTable data object with normalised coordinates (projections on the eigenmodes). Each coordinate then comes in units of \!\(\*SqrtBox[\(Meter\)]\).\n toNormalCoords also transforms SurvivalTable objects analogously.";


toActionAngle::usage ="toActionAngle[TrackTabledata] transforms a TrackTable data object into another TrackTable data object with action-angle coordinates.\ntoActionAngle also transforms SurvivalTable objects analogously.";


toSurvival::usage ="toSurvival[TrackTabledata] transforms a TrackTable data object into a SurvivalTable object, containing only the initial conditions and numbers of turns survived.";


centreOfGravity::usage="centreOfGravity[TrackTabledata,colname] returns a list of coordinates of the centre-of-gravity motion labelled by the string colname. ";


powerSpectrum::usage="powerSpectrum[TrackTabledata,particle,colname] returns a power spectrum of particle coordintes labelled by colname in a TrackTabledata object. A list of colnames may also be given to return a set of lists.\n
		powerSpectrum[list] returns a power spectrum of a one-dimensional list.";


\!\(InputForm\`phaseSpectrum\)::usage="phaseSpectrum[TrackTabledata,particle,colname] returns a phase spectrum of particle coordintes labelled by colname in a TrackTabledata object. A list of colnames may also be given to return a set of lists.\n
		powerSpectrum[list] returns a phase spectrum of a one-dimensional list.";


toNormalCoords::warning ="The data already in normalised coordinates.";


toNormalCoords::badarg ="Wrong data type (action-angle coordinates).";


toActionAngle::warning ="The data is already in action-angle coordinates.";


Begin["`Private`"]


protected =Unprotect[mfsTypes,mfsKeyValue,mfsKeyNames,mfsColumnNames,mfsInterpretKeys,mfsAddKey,mfsColumn];


orbtemplate = {"X","PX","Y","PY","T","PT"};


emittemplate={"EX","EY","ET"};


eigtemplate=Table["E" <> ToString[i] <> ToString[ j]  ,{i,1,6},{j,1,6}];


makeTrackTable[qpmfs_mfs]:=Module[{qpcol,turns,particles},
		If[mfsKeyValue[qpmfs,"TYPE"]=="TRACK",
			(qpcol = mfsColumn[ qpmfs];
				turns=Max[First/@  qpcol]+1;		particles=Max[(#[[2]])& /@ qpcol];
				qp=Table[Null,{turns},{particles},{6}];		( qp[[  #[[1]]+1 , #[[2]] ]]=Drop[#,2] )& /@ qpcol;
				TrackTable[
					Join[
						Select[First[qpmfs],
							Not[MemberQ[Flatten[eigtemplate\[Union] orbtemplate\[Union]emittemplate],First[#]]]&
								],
						{{"TURNS",turns},
							{"PARTICLES",particles},
							{"EMITTANCES",mfsKeyValue[qpmfs,emittemplate] Meter},
							{"ORBIT",mfsKeyValue[qpmfs,orbtemplate]},
							{"EIGENVECTORS",mfsKeyValue[qpmfs,eigtemplate]}
						}
					],
					Drop[qpmfs[[2]],2],
					qp
					]
				)
			,Null]
		];
makeTrackTable[qp_]:=Null


(* symplecticJ[n_Integer/;(n>0)]:=Block[{s},
		s=Table[0,{2n},{2n}];
		s=ReplacePart[s,1,Table[{2k-1,2k},{k,1,n}]];
		ReplacePart[s,-1,Table[{2k,2k-1},{k,1,n}]]
	]*)


(* sJ3=SymplecticJ[3] *)
sJ3={{0,1,0,0,0,0},{-1,0,0,0,0,0},{0,0,0,1,0,0},{0,0,-1,0,0,0},{0,0,0,0,0,1},{0,0,0,0,-1,0}}


toNormalCoords[qp_TrackTable]:=Module[{ieigenvect},
If[qp[[2,1]]=="X",
				(
				ieigenvect=Transpose[sJ3].Transpose[mfsKeyValue[qp,"EIGENVECTORS"]].sJ3;
		TrackTable[
			First[qp],
			{"XN","PXN","YN","PYN","TN","PTN"},
			Map[
(If[First[#]=!=Null,
					(ieigenvect.#),#])&,
				Last[qp],{2}]
		]
				),
				(If[qp[[2,1]]=="XN",
					(Message[toNormalCoords::warning];qp),
					(Message[toNormalCoords::badarg];Null)
				]
					)
				]
	]


toNormalCoords[qp_SurvivalTable]:=
Module[{ieigenvect},ieigenvect=Transpose[sJ3].Transpose[mfsKeyValue[qp,"EIGENVECTORS"]].sJ3;Which[qp[[2,1]]=="X",SurvivalTable[First[qp],{"XN","PXN","YN","PYN","TN","PTN","SURVIVED"},(Flatten[{ieigenvect.Take[#1,6],Last[#1]}]&)/@Last[qp]],qp[[2,1]]=="XN",True]]


toActionAngle[{x_,px_}]:={(x^2+px^2)/2,-(ArcTan[x,px]/(2\[Pi]))}


toActionAngle[xpx_List/;EvenQ[Length[xpx]]]:=Flatten[toActionAngle/@Partition[xpx,2]]


toActionAngle[xpx_List/;OddQ[Length[xpx]]]:=Flatten[{toActionAngle[Drop[xpx,-1]],Last[xpx]}]


toActionAngle[qp_TrackTable]:=Module[{qptemp},
		If[qp[[2,1]]!="IX",
			(qptemp=Last[If[qp[[2,1]]=="XN",qp,toNormalCoords[qp]]];
				qptemp=Map[(Partition[#,2])&,qptemp,{2}];
				TrackTable[
					First[qp],
					{"IX","PHIX","IY","PHIY","IT","PHIT"},(qptemp=Map[(If[First[#]=!=Null,
							{(Plus@@(#^2))/2,-ArcTan@@#/(2\[Pi])},#])&,
							qptemp,{3}];
						Map[(Join[ #[[1]],#[[2]],#[[3]] ])&,qptemp,{2}]
		)
		]
		),
			Message[toActionAngle::warning];qp
		]
	]


toActionAngle[qp_SurvivalTable]:=Module[{},
		Which[
			qp[[2,1]]=="X",toActionAngle[toNormalCoords[qp]],
			qp[[2,1]]=="XN",SurvivalTable[First[qp],{"IX","PHIX","IY","PHIY","IT","PHIT","SURVIVED"},
				toActionAngle/@Last[qp]
			],
			qp[[2,1]]=="IX",qp,
			True,(* message, leave alone *)	qp		
			]
		]


powerSpectrum[data_List]:=Module[{trans},
		trans=Abs[Fourier[notLost@data]]^2;
		trans=Transpose[{
					Table[f/Length[trans],{f,0,Length[trans]-1}],
					trans}];
				Take[trans,IntegerPart[ Length[trans]/2+1]]
		]


powerSpectrum[qp_TrackTable,particle_Integer,key_String]:=powerSpectrum[notLost@mfsColumn[qp,particle,key]]


phaseSpectrum[data_List]:=Module[{trans},
		trans=Arg[Fourier[notLost@data]]^2;
		trans=Transpose[{
					Table[f/Length[trans],{f,0,Length[trans]-1}],
					trans}];
				Take[trans,IntegerPart[ Length[trans]/2+1]]
		]


phaseSpectrum[qp_TrackTable,particle_Integer,key_String]:=phaseSpectrum[notLost@mfsColumn[qp,particle,key]]


strobe[ll_List,{m_Integer,n_Integer}]:=Transpose[Partition[ll,m] ] [[ Mod[n-1,m]+1]]


notLost[x_List]:=TakeWhile[x,NumberQ]


turnsSurvived[qp_TrackTable,particle_Integer]:=Length[notLost@mfsColumn[qp,particle,"X"]]


turnsSurvived[qp_TrackTable,particles_List]:=(Length[notLost@mfsColumn[qp,#,"X"]])&/@particles


intensity[qp_TrackTable]:=Module[{ll,ptcles},
		ptcles=mfsKeyValue[qp,"PARTICLES"];
		ll=Map[
	(If[First[#]=!=Null,First[#],Null]
		)&,Last[qp],{2}];
		ll=Map[(Select[#,NumberQ])&,ll,{1}];
		Map[(Length[#]/ptcles)&,ll]
		]


toSurvival[qp_TrackTable]:=Module[{np,turns},
		np=mfsKeyValue[qp,"PARTICLES"];
		turns=mfsKeyValue[qp,"TURNS"];
		SurvivalTable[First[qp],
			Append[Part[qp,2],"SURVIVED"],
			Transpose[Join[
					Transpose[First[mfsColumn[qp]]],
					{(turnsSurvived[qp,#]& /@ Range[1,np])}
				]
			]
		]
	]


centreOfGravity[qp_TrackTable,key_String]:=(Plus @@ #)& /@ Transpose[mfsColumn[qp,key]/.Null->0]/mfsKeyValue[qp,"PARTICLES"]


centreOfGravity[qp_TrackTable,keys_List]:=centreOfGravity[qp,#]&/@keys


mfsTypes=Union[mfsTypes,{TrackTable,SurvivalTable}];


mfsKeyValue[qp_TrackTable,keyss_]:=mfsKeyValue[mfs @@ qp,keyss]


mfsKeyValue[qp_SurvivalTable,keyss_]:=mfsKeyValue[mfs @@ qp,keyss]


mfsKeyNames[qp_TrackTable]:=mfsKeyNames[mfs @@ qp]


mfsKeyNames[qp_SurvivalTable]:=mfsKeyNames[mfs @@ qp]


mfsColumnNames[qp_TrackTable]:=mfsColumnNames[mfs@@qp]


mfsColumnNames[qp_SurvivalTable]:=mfsColumnNames[mfs@@qp]


mfsInterpretKeys[qp_TrackTable]:=mfsInterpretKeys[mfs @@ qp]


mfsInterpretKeys[qp_SurvivalTable]:=mfsInterpretKeys[mfs @@ qp]


mfsAddKey[qp_TrackTable,{key_String,value_}]:=TrackTable@@ mfsAddKey[mfs @@ qp,{key,value}]


mfsAddKey[qp_SurvivalTable,{key_String,value_}]:=SurvivalTable@@
mfsAddKey[mfs @@ qp,{key,value}]


mfsColumn[qp_TrackTable,particle_Integer,key_String]:=Flatten[(#[[particle,Flatten[Position[mfsColumnNames[qp],key]]]])& /@Last[qp]]


mfsColumn[qp_SurvivalTable,key_String]:=mfsColumn[mfs@@qp,key]


mfsColumn[qp_SurvivalTable,particle_Integer,key_String]:=mfsColumn[mfs@@qp,key][[particle]]


mfsColumn[qp_TrackTable,particle_Integer,keys_List]:=mfsColumn[qp,particle,#]& /@ keys


mfsColumn[qp_SurvivalTable,keys_List]:=mfsColumn[qp,#]& /@ keys


mfsColumn[qp_SurvivalTable,particle_Integer,keys_List]:=mfsColumn[qp,particle,#]& /@ keys


mfsColumn[qp_TrackTable,key_String]:=mfsColumn[qp,#,key]&/@Table[i,{i,mfsKeyValue[qp,"PARTICLES"]}]


mfsColumn[qp_TrackTable]:=Last[qp]


mfsColumn[qp_SurvivalTable]:=Last[qp]


Protect[ Evaluate[protected] ];


End[ ]


Protect[makeTrackTable,notLost,strobe,intensity,turnsSurvived,toNormalCoords,toActionAngle,centreOfGravity,powerSpectrum];


EndPackage[ ]


If[$Remote,Null,NotebookOpen[ToFileName[packageSourceDirectory["TrackTable"],"TrackTablePalette.nb"]]]


Print["Version 3.1 of Madtomma`Mfs`TrackTable` loaded.  Name change for symplecticJ->SymplecticJ"]



