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



BeginPackage["Optimise`EPOptimiseRealv3`"];


EPOptimiseReal::usage="EPOptimiseReal[Objective Function, No Of Chromosomes, Iterations, Prob. of Crossover, Prob. of Mutation, Genes per Chrom., Gene Length (\!\(\*SuperscriptBox[\"2\", 
RowBox[{\"Gene\", \" \", \"Length\"}]]\)), Start of range, End of range, Options (see EPOptimiseReal[Options])]\nEPOptimiseReal is a MAXIMISING Function!";


EPOptimiseReal::Options="EPOptimiseReal supports 5 possible options: SelectionMethod, BreedingPool, ReinsertionMethod, BreedingRate, MutationRate;\nSee EPOptimiseReal[<Option Name>];"


EPOptimiseReal::SelectionMethod="SelectionMethod specifies one of 4 methods for selecting the possible breeding population.\nThe options are:\nRoulette\nUniversal\nTruncation\nTournament"


EPOptimiseReal::Roulette="In the Roulette SelectionMethod The individuals are mapped to contiguous segments of a line, such that each individual's segment is equal in size to its fitness. A random number is generated and the individual whose segment spans the random number is selected. This is repeated to select the breeding population required."


EPOptimiseReal::Universal="In the Universal SelectionMethod the individuals are mapped to contiguous segments of a line, such that each individual's segment is equal in size to its fitness exactly as in Roulette selection. Here equally spaced pointers are placed over the line as many as there are individuals to be selected, and the breeding population thus chosen."


EPOptimiseReal::Truncation="In Truncation selection individuals are sorted according to their fitness. Only the best individuals are selected for parents. The parameter for truncation selection is the TruncationThreshold. TruncationThreshold indicates the proportion of the population to be selected as parents and takes values ranging from 50%-10%. Individuals below the truncation threshold do not produce offspring."


EPOptimiseReal::TruncationThreshold="TruncationThreshold indicates the proportion of the population to be selected as parents in the Truncation SelectionMethod, and takes values ranging from 50%-10%."


EPOptimiseReal::Tournament="In Tournament selection a number TournamentNumber of individuals is chosen randomly from the population and the best individual from this group is selected as a parent. This process is repeated as often as individuals must be chosen."


EPOptimiseReal::TournamentNumber="TournamentNumber is the number of individuals to be tested in each tournament round in the Tournament SelectionMethod." 


EPOptimiseReal::BreedingPool="The BreedingPool is the size of the breeding population at each iteration. This is the total possible breeding population, and not all individuals chosen will breed. See EPOptimiseReal[Breeding]."


EPOptimiseReal::Breeding="When breeding individuals, the breeding population is chosen using the SelectionMethod variable and the BreedingPool size. The chance of these individuals then breeding is determined randomly using the Crossover Probability. The breeding is then performed using one of the CrossoverMethod's (Not yet implemented), and reinsertion of the resulting offspring is performed according to the ReinsertionMethod."


EPOptimiseReal::ReinsertionMethod="ReinsertionMethod specifies the algorithm for reinserting the offspring back into the main population. It is dependant on the choice of BreedingPool relative to the population size. The methods are:\nBreedingPool = Population Size  \[Rule]  Pure\nBreedingPool < Population Size  \[Rule]  Elitist or Uniform\nBreedingPool > Population Size  \[Rule]  Fitness"


EPOptimiseReal::Pure="In Pure reinsertion the parents are completely replaced with their offspring. Because not all parents reproduce (see EPOptimiseReal[Breeding]), some offspring are clones of their parents."


EPOptimiseReal::Elitist="In Elitist reinsertion the worst parents are replaced by the offspring. Elitist reinsertion requires BreedingPool to be less than the population Size."


EPOptimiseReal::Uniform="In Uniform reinsertion the offspring randomly replace some of the parents. This is analogous to Pure reinsertion when BreedingPool < Population Size."


EPOptimiseReal::Fitness="In Fitness reinsertion the fittest offspring replace all of the parents. This requires BreedingPool > Population Size. The Fitness ReinsertionMethod has a penalty in terms of computational efficiency as the fitness of all offspring must be evaluated in addition to that required in evaluating the chosen SelectionMethod."


EPOptimiseReal::BreedingRate="Option to pass a function to vary the BreedingRate (0\[Rule]1) with iteration number. Arguments should be of the form:\n\nBreedingRate\[Rule]BreedingRateFunction,  where BreedingRateFunction is a standard Mathematica function with one input, or\nBreedingRate\[Rule](<algebraic function of #>&), where # is the iteration number argument and & denotes a pure function.\n\nThe output of either expression should be a number between 0 and 1.\nThe variable \"NumberIterations\" can be used as a replacement value for the input total number of iterations."


EPOptimiseReal::MutationRate="Option to pass a function to vary the MutationRate (0\[Rule]1) with iteration number. Arguments should be of the form:\n\nMutationRate\[Rule]MutationRateFunction,  where MutationRateFunction is a standard Mathematica function with one input, or\nMutationRate\[Rule](<algebraic function of #>&), where # is the iteration number argument and & denotes a pure function.\n\nThe output of either expression should be a number between 0 and 1.\nThe variable \"NumberIterations\" can be used as a replacement value for the input total number of iterations."


GetIterationNumber::usage="Returns Latest Iteration Number from EPOptimiseReal."


Begin["`Private`"]


EPOptimiseReal[]:=?EPOptimiseReal


EPOptimiseReal[help_]:=Print[ToExpression[Evaluate["EPOptimiseReal::"<>ToString[help]]]]


RandomUnion[no_]:=Block[{randlist=Range[no],rand,randno},Table[rand=RandomInteger[{1,Length[randlist]}];randno=randlist[[rand]];randlist=Drop[randlist,{rand}];randno,{no}]]


RandomUnion[no_,subno_]:=Block[{randlist=Range[no],rand,randno},Table[rand=RandomInteger[{1,Length[randlist]}];randno=randlist[[rand]];randlist=Drop[randlist,{rand}];randno,{subno}]]


randomnumbers[number_]:=(SeedRandom[];RandomReal[{0,1},number])


bin6[b_,c_]:=b[[#]]&/@Split[Ordering[c],c[[#1]]==c[[#2]]&];


CreateChrom[nochroms_,nogenes_,start_Real|start_Integer,end_Real|end_Integer]:=RandomReal[{start,end},{nochroms,nogenes}]


CreateChrom[nochroms_,nogenes_,start_List,end_Real|end_Integer]/;Length[start]>=nogenes:=Transpose[RandomReal[{#,end},{nochroms}]&/@start[[1;;nogenes]]]


CreateChrom[nochroms_,nogenes_,start_List,end_List]:=Transpose[RandomReal[#,{nochroms}]&/@Transpose[{start,end}][[1;;nogenes]]]


TurnToBin[list_List,length_Integer,number_Integer,start_,range_]:=Flatten[Block[{a=#},PadLeft[RealDigits[Ordering[Abs[list[[a]]-#]][[1]]-1,2][[1]],length,0]&[Range[start[[a]],range[[a]],(range[[a]]-start[[a]])/(2^length-1)]]]&/@Range[number]]


TurnToRange[binnumber_,length_Integer,number_Integer,start_Integer,range_Integer]:=(N[start+(FromDigits[Take[binnumber,{#,#+(length)-1}],2]*((range-start)/(2^(length)-1)))]&/@Range[1,length*number,(length)])


TurnToRange[binnumber_,length_Integer,number_Integer,start_List,range_Integer]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1]],#[[1]]+(length)-1}],2]*((range-#[[2]])/(2^(length)-1)))]&/@MapThread[List,{Range[1,length*number,(length/number)],start}])


TurnToRange[binnumber_,length_Integer,number_Integer,start_Integer,range_List]:=(N[start+(FromDigits[Take[binnumber,{#[[1]],#[[1]]+(length)-1}],2]*((#[[2]]-start)/(2^(length/number)-1)))]&/@MapThread[List,{Range[1,length*number,(length/number)],range}])


TurnToRange[binnumber_,length_Integer,number_Integer,start_List,range_List]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1]],#[[1]]+(length)-1}],2]*((#[[3]]-#[[2]])/(2^(length)-1)))]&/@MapThread[List,{Range[1,length*number,length],start,range}])


TurnToRange[binnumber_,length_List,number_Integer,start_Integer,range_Integer]:=(N[start+(FromDigits[Take[binnumber,{#[[1]],#[[1]]+#[[2]]-1}],2]*((range-start)/(2^#[[2]]-1)))]&/@MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}])


TurnToRange[binnumber_,length_List,number_Integer,start_List,range_Integer]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((range-#[[2]])/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],start}])


TurnToRange[binnumber_,length_List,number_Integer,start_Integer,range_List]:=(N[start+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((#[[2]]-start)/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],range}])


TurnToRange[binnumber_,length_List,number_Integer,start_List,range_List]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((#[[3]]-#[[2]])/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],start,range}])


TurnToRange[binnumber_,length_List,number_List,start_Integer,range_Integer]:=(N[start+(FromDigits[Take[binnumber,{#[[1]],#[[1]]+#[[2]]-1}],2]*((range-start)/(2^#[[2]]-1)))]&/@MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}])


TurnToRange[binnumber_,length_List,number_List,start_List,range_Integer]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((range-#[[2]])/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],start}])


TurnToRange[binnumber_,length_List,number_List,start_Integer,range_List]:=(N[start+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((#[[2]]-start)/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],range}])


TurnToRange[binnumber_,length_List,number_List,start_List,range_List]:=(N[#[[2]]+(FromDigits[Take[binnumber,{#[[1,1]],#[[1,1]]+#[[1,2]]-1}],2]*((#[[3]]-#[[2]])/(2^#[[1,2]]-1)))]&/@MapThread[List,{MapThread[List,{Drop[FoldList[Plus,1,length],-1],length}],start,range}])


FitnessAll[list_]:=(FitnessValue[#]&/@list)


FitnessAll[list_,rawdata_,fitdata_]:=(If[Position[Most[rawdata],#]=!={},fitdata[[Sequence@@Position[Most[rawdata],#][[1]]]],FitnessValue[#]]&/@list)


CumulativeProbabilities[nondominated_,crowdingdistance_,function_]:=Block[{a},
a=crowdingdistance;
fitdata={Sequence@@fitdata,a};
Rest[FoldList[Plus,0,a/Total[a]]]
]


selectionmethods={Global`Roulette,Global`Universal,Global`Truncation,Global`Tournament,Global`Different}


SelectionRoulette[list_,probab_,number_,opts___]:=Block[{a,random=randomnumbers[number]},
list[[breedinglist=(a=#;
Length[Select[probab,#<a&]]+1)&/@random]]]


SelectionDifferent[list_,probab_,number_,opts___]:=Block[{a,probabs},
probabs=Prepend[probab[[#]]-probab[[#-1]]&/@Range[2,Length[probab]],First[probab]];
a=Length[list]-Range[number]+1;
list[[breedinglist=Ordering[probabs][[a]]]]
]


SelectionUniversal[list_,probab_,number_,opts___]:=Block[{a,linear=Range[1/number,1,1/number]},list[[breedinglist=((a=#1;Length[Select[probab,#1<a&]]+1)&)/@linear]]]


SelectionAll[list_,probab_,number_,opts___]:=Block[{},
breedinglist=RandomUnion[number];
list[[breedinglist]]
]


SelectionTruncation[list_,probab_,number_,opts___]:=Block[{a,b,threshold,c},
threshold=Global`TruncationThreshold/.{opts}/.{Global`TruncationThreshold->0.5`};
If[printedsetting=!=True,Print["Using TruncationThreshold \[Rule] "<>ToString[threshold]];printedsetting=True];
a=MapThread[Subtract,{probab,Prepend[Most[probab],0]}];
b=Sort[Transpose[{a,list}],#1[[1]]<#2[[1]]&];
c=Take[b,-Round[threshold Length[list]]];
breedinglist=(Position[list,#1][[1,1]]&)/@c[[All,2]];c[[All,2]][[RandomInteger[{1,Length[c]},Length[c]]]]]


SelectionTournament[list_,probab_,number_,opts___]:=Block[{a=MapThread[Subtract,{probab,Prepend[Most[probab],0]}],b=a,tour},
tour=Global`TournamentNumber/.{opts}/.{Global`TournamentNumber->Ceiling[number/5]};
If[printedsetting=!=True,Print["Using TournamentNumber \[Rule] "<>ToString[tour]];printedsetting=True];Table[((a=ReplacePart[a,0,#1];AppendTo[breedinglist,#1];list[[#1]])&)[(Position[a,Max[#1]][[1,1]]&)[a[[RandomUnion[number,tour]]]]],{number}]]


Clear[SelectforCrossover]


SelectforCrossover[list_,numbers_:5,pc_:0.25`]:=Block[{a,positionsofcross,randompos,breeding,notbreeding},
randompos=RandomUnion[numbers];
a=#<pc&/@randomnumbers[numbers];
breeding:=Position[a,True][[All,1]];
notbreeding:=Position[a,False][[All,1]];
If[OddQ[Length[breeding]],
If[RandomInteger[{1,2}]===1||Length[breeding]===Length[a],
a=ReplacePart[a,breeding[[RandomInteger[{1,Length[breeding]}]]]->False];,
a=ReplacePart[a,notbreeding[[RandomInteger[{1,Length[notbreeding]}]]]->True];
];
];
positionsofcross=MapThread[List,{randompos,a}];
crossoverlist=positionsofcross;
(list[[#[[1]]]]&)/@positionsofcross]


outsiderange[value_,start_,end_]:=Which[(2start-value)>end,end,(2end-value)<start,start,value<start,outsiderange[2start-value,start,end],value>end,outsiderange[2end-value,start,end],value>=start&&value<=end,value]


CheckRange[data_,start_,end_]:=Block[{ans},
ans=outsiderange[Sequence@@#]&/@Transpose[{#,If[ListQ[start],start,Table[start,{Length[#]}]],If[ListQ[end],end,Table[end,{Length[#]}]]}]&/@data;
ans]


crossovermethods={"Biased","Interpolated","SBX"}


InvDistC[x_,n_]:=Switch[Random[Integer,{0,1}],0,1^(1/(1+n)) x^(1/(1+n)),1,Abs[ 2^(-(1/(1+n))) (0.5-1 x)^(-(1/(1+n)))]]


CrossoverSBX[datalistin_,crosslist_,start_,end_]:=Block[{datalist=datalistin,lengths,crossingpositions,randomcrossingpositions,pairedcrossingpositions,crosseddata},
crossingpositions=Select[crosslist,#[[2]]===True&][[All,1]];
randomcrossingpositions=RandomUnion[Length[crossingpositions]];
pairedcrossingpositions=Partition[crossingpositions[[randomcrossingpositions]],2];
crosseddata=Block[{chrom=datalistin[[#]]},SBX[chrom]]&/@pairedcrossingpositions;
((datalist=ReplacePart[datalist,#[[1]]->#[[2]]])&/@#)&/@MapThread[Transpose[{##}]&,{pairedcrossingpositions,crosseddata}];
CheckRange[datalist,start,end]]


SBX[inputin_]:=Block[{absmeans,relmeans,orders,a,input=Sort[inputin]},
absmeans=Mean[#]&/@Transpose[input];
relmeans=Mean[#-Min[#]]&/@Transpose[input];
orders=Ordering[#]&/@Transpose[input];
betas=InvDistC[Random[Real,{0,1}],$betafactor];
a=Switch[Random[Integer,{0,1}],1,{#[[1]]-(#[[2]]*betas),#[[1]]+(#[[2]]*betas)},0,#[[4]]]&/@Transpose[{absmeans,relmeans,orders,Transpose[input]}];
Transpose[a]
]


CrossoverBiased[datalistin_,crosslist_,start_,end_]:=Block[{datalist=datalistin,lengths,crossingpositions,randomcrossingpositions,pairedcrossingpositions,crosseddata},
crossingpositions=Select[crosslist,#[[2]]===True&][[All,1]];
randomcrossingpositions=RandomUnion[Length[crossingpositions]];
pairedcrossingpositions=Partition[crossingpositions[[randomcrossingpositions]],2];
(*Print[pairedcrossingpositions];*)
crosseddata=Transpose[Module[{chrom=datalistin[[#]]},If[RandomReal[{0,1}]<$parentbias,#,Reverse[#]]&/@Transpose[chrom]]]&/@pairedcrossingpositions;
(*Print[MapThread[Transpose[{##}]&,{pairedcrossingpositions,crosseddata}]];*)
((datalist=ReplacePart[datalist,#[[1]]->#[[2]]])&/@#)&/@MapThread[Transpose[{##}]&,{pairedcrossingpositions,crosseddata}];datalist]


CrossoverInterpolated[datalistin_,crosslist_,start_,end_]:=Block[{datalist=datalistin,lengths,crossingpositions,randomcrossingpositions,pairedcrossingpositions,crosseddata,p},
crossingpositions=Select[crosslist,#[[2]]===True&][[All,1]];
randomcrossingpositions=RandomUnion[Length[crossingpositions]];
pairedcrossingpositions=Partition[crossingpositions[[randomcrossingpositions]],2];
(*Print[pairedcrossingpositions];*)
crosseddata=Transpose[Module[{chrom=datalistin[[#]]},{#[[1]](p=RandomReal[{-1,1}])+(1-p)#[[2]],#[[2]](p=RandomReal[{-1,1}])+(1-p)#[[1]]}&/@Transpose[chrom]]]&/@pairedcrossingpositions;

((datalist=ReplacePart[datalist,#[[1]]->#[[2]]])&/@#)&/@MapThread[Transpose[{##}]&,{pairedcrossingpositions,crosseddata}];CheckRange[datalist,start,end]]


reinsertionmethods={"Elitist","Uniform","Pure","Fitness"}


ReinsertPure[olddata_,newdata_,positions_,jds___]:=Block[{a=RandomUnion[Length[positions]]},
newdata[[a]]
]


ReinsertFitness[olddata_,newdata_,null_,genelength_,nogenes_,start_,end_]/;Length[newdata]>=Length[olddata]:=Block[{a,random=RandomUnion[Length[olddata]],bnewdata=newdata,fitdata2,positions},
a=olddata;
fitdata2=FitnessValue[#]&/@bnewdata;
(*Print["Fitdata offspring = ",fitdata2];
Print["Fitdata ordered = ",Ordering[fitdata2,-Length[olddata]]];*)
positions=Ordering[fitdata2,-Length[olddata]];
(a=ReplacePart[a,#[[1]]->#[[2]]])&/@Transpose[{random,newdata[[positions]]}];a]


ReinsertFitness[olddata_,newdata_,positions_,genelength_,nogenes _,start_,end_]/;Length[newdata]<Length[olddata]:=
(Print["Not enough offspring, using Reinsertion Method \[Rule] Elitist"];
ReinsertElitist[olddata,newdata])


ReinsertUniform[olddata_,newdata_,Null___]:=Block[{a,positions=RandomUnion[Length[olddata],Length[newdata]]},
(*Print["Uniform Positions = ",positions];*)
a=olddata;
(a=ReplacePart[a,#[[2]],#[[1]]])&/@Transpose[{positions,newdata}];
a]


ReinsertElitist[olddata_,newdata_,Null___]:=Block[{a,random=RandomUnion[Length[newdata]],positions},
a=olddata;
positions=Ordering[fitdata[[-1]]][[Range[Length[newdata]]]][[random]];
(a=ReplacePart[a,#[[1]]->#[[2]]])&/@Transpose[{positions,newdata}];a]


mutationmethods={"Gaussian","Scaled"}


MutateGaussian[value_,start_,end_,mutationsize_]:=outsiderange[value+Random[NormalDistribution[0,mutationsize]],start,end]


MutateScaled[value_,start_,end_,mutationsize_]:=outsiderange[value*RandomReal[{-2,2}],start,end]


Mutate[list_,mr_,start_,end_,mutationmethod_,mutationsize_]:=Block[{},
Module[{data=Transpose[{#,If[ListQ[start],start,Table[start,{Length[#]}]],If[ListQ[end],end,Table[end,{Length[#]}]],If[ListQ[mutationsize],mutationsize,Table[mutationsize,{Length[#]}]]}]},
If[RandomReal[{0,1}]<mr,mutationmethod[Sequence@@##],#[[1]]]&/@data]&/@list
]


GetIterationNumber[]:=iterationno


Clear[Dominates]


SaveFit[mfunc_,p_]:=Block[{},
If[ValueQ[Fitness[mfunc][p]],
Fitness[mfunc][p],
Fitness[mfunc][p]=mfunc[p]]
]


Dominates[p_,q_,m_]:=Dominates[p,q,m]=Block[{},
pans=SaveFit[#,p]&/@m;
qans=SaveFit[#,q]&/@m;
And@@{Or@@(#[[1]]>#[[2]]&/@Transpose[{pans,qans}]),
Or@@(#[[1]]>=#[[2]]&/@Transpose[{pans,qans}])}
]


FindNonDominatedFront[P_,m_]:=Block[{Pdash={}},
AppendTo[Pdash,P[[1]]];
Block[{a=#,delete={}},
If[And@@(If[Dominates[a,Pdash[[#]],m],AppendTo[delete,{#}];True,False]&/@Range[Length[Pdash]]),AppendTo[Pdash,a]];
Delete[Pdash,delete]]&/@Rest[P];
Pdash
]


FastNonDominatedSort[Pin_,m_,N_]:=Block[{P=Pin,F={},i=0,Fnew},
While[Length[P]>0,
Fnew=FindNonDominatedFront[P,m];
If[Length[Flatten[F,1]]+Length[Fnew]<N,
F=AppendTo[F,Fnew];
(P=Delete[P,{Position[P,#][[1,1]]}])&/@Fnew;,
Break[]];
i++;
];
{F,Fnew}
]


DominationRank[P_List,m_List]:=Block[{P2},
P2=Outer[Dominates[#1,#2,m]&,P,P,1];
Count[#,True]&/@P2
]


PartialOrder[i_List,j_List]:=Module[{irank,jrankidistance,jdistance},
{{irank,jrank},{idistance,jdistance}}=Transpose[{i,j}];
irank>jrank||(irank===jrank&&idistance>jdistance)
]


ObjectiveSort[I_,mfunc_]:=Module[{ans},
ans=SaveFit[mfunc,#]&/@I;
I[[Ordering[ans]]]
]


Clear[CrowdingDistanceAssignment]


CrowdingDistanceAssignment[Pin_List,m_List]:=Module[{Idistance,P=Pin},
Idistance=Table[0,{Length[P]}];
Idistance+=Block[{a=#,mfunc=m[[#]],ans,anspos,Id=Table[0,{Length[P]}]},
ans=SaveFit[mfunc,#]&/@P;
anspos=Ordering[ans];ans=Sort[ans];
Id[[1]]=Id[[-1]]=100;
(Id[[#]]=(ans[[#+1]]-ans[[#-1]]))&/@Range[2,Length[P]-1];
Id[[anspos]]]&/@Range[Length[m]];
Idistance
]


DominationSortingList[P_,distances_,m_]:=Module[{ranks},
ranks=DominationRank[P,m];
Ordering[Transpose[{ranks,distances}],All,PartialOrder[#1,#2]&]
]


DominationSorting[P_,distances_,m_]:=Module[{ranks},
ranks=DominationRank[P,m];
P[[Ordering[Transpose[{ranks,distances}],All,PartialOrder[#1,#2]&]]]
]


EPOptimiseReal[function_List,nochroms_,iters_,pcstart_:0.25,mrstart_:0.01,msstart_:2,nogenes_,start_,end_,opts___]:=
Block[{a,probs,b,c,alldata,current,best={-\[Infinity],{}},dynamicgraphics,startgene,converge,selectmethod,ruttingpopulationsize,reinsertmethod,population,breedingrate,mutationrate,cp,ruttingpopulation,breedingpopulation,newpopulation,keepbest,bestpos,minrawdata,mutationsize,mutatemethod,crossovermethod,dynamicupdate,nondominatedpopulation,restpopulation,populationcrowding,newpopulationsorting},
dynamicupdate=Global`DynamicUpdate/.{opts}/.{Global`DynamicUpdate->True};
dynamicgraphics=Global`DynamicGraphics/.{opts}/.{Global`DynamicGraphics->False};
startgene=Global`Start/.{opts}/.{Global`Start->Null};
converge=Global`Convergence/.{opts}/.{Global`Convergence->\[Infinity]};
$parentbias=Global`ParentBias/.{opts}/.{Global`ParentBias->0.5};
$betafactor=Global`SBXBeta/.{opts}/.{Global`SBXBeta->3};
selectmethod=Optimise`EPOptimiseRealv3`Private`SelectionAll;
selectmethod=ToExpression["Optimise`EPOptimiseRealv3`Private`Selection"<>ToString[If[MemberQ[selectionmethods,#],
If[dynamicupdate,Print["Using SelectionMethod \[Rule] "<>ToString[#]]];
#,If[dynamicupdate,Print["Selection Method not recognised: Using Roulette\nMethods \[Rule] Roulette, Universal, Truncation (TruncationThreshold) and Tournament (TournamentNumber)"]];
Global`Roulette]&[Global`SelectionMethod/.{opts}/.{Global`SelectionMethod->Roulette}]]];printedsetting=False;
crossovermethod=ToExpression["Optimise`EPOptimiseRealv3`Private`Crossover"<>ToString[If[MemberQ[crossovermethods,ToString[#]],
If[dynamicupdate,Print["Using CrossoverMethod \[Rule] "<>ToString[#]]];
#,If[dynamicupdate,Print["Crossover Method not recognised: Using Biased\nMethods \[Rule] Biased (ParentBias), Interpolation, SBX (SBXBeta)"]];
Global`Biased]&[Global`CrossoverMethod/.{opts}/.{Global`CrossoverMethod->Global`Biased}]]];
ruttingpopulationsize=Global`BreedingPool/.{opts}/.{Global`BreedingPool->nochroms};
If[ruttingpopulationsize<nochroms||selectmethod===Optimise`EPOptimiseRealv3`Private`SelectionTruncation,reinsertmethod=ToExpression["Optimise`EPOptimiseRealv3`Private`Reinsert"<>ToString[If[MemberQ[reinsertionmethods,ToString[#]],
If[dynamicupdate,Print["Using ReinsertionMethod \[Rule] "<>ToString[#]<>"\nBreedingPool < Population \[Rule] Methods: Elitist and Uniform"]];#,
If[dynamicupdate,Print["Reinsertion Method not recognised: Using Elitist\nBreedingPool < Population \[Rule] Methods: Elitist and Uniform"]];
Global`Elitist]&[Global`ReinsertionMethod/.{opts}/.{Global`ReinsertionMethod->Elitist}]]],
If[ruttingpopulationsize>nochroms,
If[dynamicupdate,Print["Using Reinsertion Method \[Rule] Fitness (BreedingPool > Population)"]];
reinsertmethod=Optimise`EPOptimiseRealv3`Private`ReinsertFitness,
If[dynamicupdate,Print["Using Reinsertion Method \[Rule] Pure (BreedingPool == Population)"]];
reinsertmethod=Optimise`EPOptimiseRealv3`Private`ReinsertPure;
]
];
mutatemethod=ToExpression["Optimise`EPOptimiseRealv3`Private`Mutate"<>ToString[If[MemberQ[mutationmethods,ToString[#]],
If[dynamicupdate,Print["Using MutationMethod \[Rule] "<>ToString[#]]];
#,If[dynamicupdate,Print["Mutation Method not recognised: Using Gaussian\nMethods \[Rule] Gaussian (MutationMagnitude), Scale"]];
Global`Gaussian]&[Global`MutationMethod/.{opts}/.{Global`MutationMethod->Global`Gaussian}]]];
keepbest=Global`KeepBest/.{opts}/.{Global`KeepBest->False};
SeedRandom[];
alldata=Global`AllData/.{opts}/.{Global`AllData->True};
FitnessValue[x___]:=function[x];

If[startgene===Null,population=CreateChrom[nochroms,nogenes,start,end],
population=Join[{startgene},CreateChrom[nochroms-1,nogenes,start,end]]
];

fitdata={};rawdata={};
(*Print["Starting population = ",population];*)
DynamicModule[{fitevaluate,fitmeasure,meanmeasure,i=0,minvalue,mindata,pc=pcstart,mr=mrstart,ms=msstart},
fitevaluate:=Global`StepEvaluate/.{opts}/.{Global`StepEvaluate->""};
fitmeasure:=Global`FitnessMeasure/.{opts}/.{Global`FitnessMeasure->(Max[#]&)};
meanmeasure:=Global`MeanMeasure/.{opts}/.{Global`MeanMeasure->(Mean[#]&)};

If[(mutationrate=Global`MutationRate/.{opts})==Global`MutationRate,mutationrate=mr&];
If[(breedingrate=Global`BreedingRate/.{opts})==Global`BreedingRate,breedingrate=pc&];
If[(mutationsize=Global`MutationMagnitude/.{opts})==Global`MutationMagnitude,mutationsize=ms&];

While[i<iters,

pc=breedingrate[i]/.{Global`NumberIterations->iters};
mr=mutationrate[i]/.{Global`NumberIterations->iters};
ms=mutationsize[i]/.{Global`NumberIterations->iters};
rawdata={Sequence@@rawdata,population};
SeedRandom[];
breedinglist={};

(*Join Parent amd Siblings*)
If[i>1,population=Join[population,{rawdata[[-2]]}],population=Join[population,rawdata[[-1]]]];

(*Create Non-Dominated Fronts*)
{nondominatedpopulation,restpopulation}=FastNonDominatedSort[population,function,nochroms];
(*Print["non dominated population = ",nondominatedpopulation];*)
nondominatedpopulation=If[Length[nondominatedpopulation]<1,#,Flatten[Join[nondominatedpopulation,{#}],1]]&[Take[DominationSorting[restpopulation,CrowdingDistanceAssignment[restpopulation,function],function],nochroms-Length[Flatten[nondominatedpopulation,1]]]];
(*Print["final nondominatedpopulation = ",nondominatedpopulation];*)
newpopulationsorting=CumulativeProbabilities[nondominatedpopulation,CrowdingDistanceAssignment[nondominatedpopulation,function],function];
(*Print["newpopulationsorting = ",newpopulationsorting];*)

ruttingpopulation=selectmethod[nondominatedpopulation,newpopulationsorting,nochroms,opts];
(*Print["ruttingpopulation = ",ruttingpopulation];*)
(*Print["Length rutting pop = ",Length[ruttingpopulation]];*)

If[Length[breedinglist]>1,
(*Print["breedinglist = ",breedinglist];*)
breedingpopulation=SelectforCrossover[ruttingpopulation,Length[breedinglist],pc];
(*Print["breeding population = ",breedingpopulation];*)
(*Print["crossoverlist = ",crossoverlist];*)
newpopulation=crossovermethod[breedingpopulation,crossoverlist,start,end];,
crossoverlist={};
newpopulation=ruttingpopulation;
];

(*Print["crossed population = ",newpopulation];*)

(*population=reinsertmethod[population,newpopulation,breedinglist[[crossoverlist[[All,1]]]],genelength,nogenes,start,end];*)
(*Print["population = ",population];*)
(*Print["New population 2 = ",FitnessValue[TurnToRange[#,genelength,nogenes,start,end]]&/@population];*)
population=Mutate[newpopulation,mr,start,end,mutatemethod,ms];
(*Print["Final population = ",population];*)

iterationno=i;
i++];
];

(*If[alldata,{Union[Extract[rawdata,#]&/@Position[fitdata,Max[fitdata]]],MapThread[List,{#[[1]],#[[2]]}]&/@MapThread[List,{rawdata,fitdata}]}];
If[dynamicupdate,Print["Best Results: "]];best[[{1,2}]]*)
]


End[]


EndPackage[]
