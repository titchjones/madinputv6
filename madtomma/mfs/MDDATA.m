(***********************************************************************
This file was generated automatically by the Mathematica front end.
It contains Initialization cells from a Notebook file, which typically
will have the same name as this file except ending in ".nb" instead of
".m".

This file is intended to be loaded into the Mathematica kernel using
the package loading commands Get or Needs.  Doing so is equivalent to
using the Evaluate Initialization Cells menu command in the front end.

DO NOT EDIT THIS FILE.  This entire file is regenerated automatically 
each time the parent Notebook file is saved in the Mathematica front end.
Any changes you make to this file will be overwritten.
***********************************************************************)















































































































BeginPackage["Madtomma`Mfs`MDdata`","Madtomma`Mfs`Mfs`"]



readThousandTurns::usage = "readThousandTurns[file] returns an mfs data object containing all the information in a file.";

parseHeader::usage = "parseHeader[file] returns the information in the header block of a file as a structured list. It is normally used inside readThousandTurns.";









Begin["`Private`"]



protected =Unprotect[tfsRead];





fixDateFormat[str_String]:=Module[{},
StringReplace[
      str,{"Jan"->"/1/","Feb"->"/2/","Mar"->"/3/","Apr"->"/4/","May"->"/5/",
        "Jun"->"/6/","Jul"->"/7/","Aug"->"/8/","Sep"->"/9/","Oct"->"/10/",
        "Nov"->"/11/","Dec"->"/12/"}]
	]





parseHeader[file_String]:=
  Module[{inpstr,ll,el,line1,line2,descriptors,columns,lastpos},
inpstr=OpenRead[file];
ll=Find[inpstr,"Dataset"];
el=ReadList[StringToStream[ll],Word,WordSeparators->" "];
el=ReplacePart[el,el[[-3]]<>" "<>el[[-2]],-3];
line1=Partition[Delete[el,-2],2];
		line1=Map[({ToUpperCase[StringDrop[First[#],-1]],Last[#]})&,line1];
ll=Find[inpstr,"Energy"];
el=Flatten@
        ReadList[StringToStream[ll],{Word,Number,Word,Word,Number,Word},
          WordSeparators->{" "}];
el=ReplacePart[el,el[[-4]]<>" "<>el[[-3]],-3]//Delete[#,-4]&;
line2=ReplacePart[el,el[[-2]]*ToExpression[el[[-1]]],-2]//Delete[#,-1]&//
        Partition[#,2]&;
		line2=Map[({ToUpperCase[StringDrop[First[#],-1]],Last[#]})&,line2];
descriptors=Join[line1,line2];
		ll=Find[inpstr,"turn"];
		columns=
      ToUpperCase/@
        ReadList[StringToStream[ll],Word,WordSeparators->{","," "}];
		lastpos=StreamPosition[inpstr];
		Close[inpstr];
		{descriptors,columns,lastpos}
		]



readThousandTurns[mdFile_String]:=Module[{hh,ts,data,str,dat,test},
	hh=parseHeader[mdFile];
  ts=OpenRead[mdFile];
		SetStreamPosition[ts,hh[[-1]]];
		data=Partition[ReadList[ts,Word,WordSeparators->{","," "}],6];
		data=Map[ToExpression,data,{2}];
		Close[ts];
		str=StringDrop[hh[[1,1,2]],4];
		dat=ReadList[StringToStream[str],Word,WordSeparators->{"_","."}];
test=mfs[First[hh],
			hh[[2]],data];
		test=mfsAddKey[test,{"DATE",First[dat]<>"/"<>dat[[2]]<>"/"<>dat[[3]]}];
		mfsAddKey[test,{"TIME",dat[[4]]<>":"<>dat[[5]]<>":"<>"00"}]
		]



(*Q_FFT*)
tfsRead[tfsFile_String,opts___Rule]:=Module[{tempStream,tempFile,tempMfs},
		tempStream=OpenTemporary[];	
removeUnwantedLines[tfsFile,tempStream,"#",Verbose->False];
		Close[tempStream];
		tempFile=First[tempStream];
		tempMfs=tfsRead[tempFile,opts];
		DeleteFile[tempFile];
		tempMfs=
        mfsAddKey[
          tempMfs,{"TIME",StringDrop[mfsKeyValue[tempMfs,"TIMESTAMP"],3]}];
		tempMfs=mfsDeleteKey[tempMfs,"TIMESTAMP"];
		tempMfs=mfsAddKey[tempMfs,{"TFSFILENAME", tfsFile}];
tempMfs=mfsAddKey[tempMfs,{"DATE",fixDateFormat@StringTake[tfsFile,
{StringLength[tfsFile]-17,Last@Flatten@StringPosition[tfsFile,"97"]}]}];
		tempMfs
		] /; StringMatchQ[tfsFile,"Q_FFT*"]

(*XY*)
tfsRead[tfsFile_String,opts___Rule]:=Module[{tempStream,tempFile,tempMfs},
		tempStream=OpenTemporary[];	
removeUnwantedLines[tfsFile,tempStream,"#",Verbose->False];
		Close[tempStream];
		tempFile=First[tempStream];
		tempMfs=tfsRead[tempFile,opts];
		DeleteFile[tempFile];
		tempMfs=mfsAddKey[tempMfs,{"TFSFILENAME", tfsFile}];
		tempMfs=mfsAddKey[tempMfs,{"DATE",fixDateFormat@StringTake[tfsFile,
{StringLength[tfsFile]-17,Last@Flatten@StringPosition[tfsFile,"97"]}]}];
		tempMfs
		] /; StringMatchQ[tfsFile,"XY*"]

(*Q_TNH*)
tfsRead[tfsFile_String,opts___Rule]:=
  Module[{tempStream,tempFile,ll,tempMfs,newCols,bc,newDescr},
	tempStream=OpenRead[tfsFile];
	ll = Find[tempStream,"$", AnchoredSearch -> True];
	Close[tempStream];
	tempStream=OpenTemporary[];
		WriteString[tempStream,ll<>" %10.5f %10.5f"<>"\n"];
		removeUnwantedLines[tfsFile,tempStream,"#",Verbose->False];
		Close[tempStream];
		tempFile=First[tempStream];
		tempMfs=tfsRead[tempFile,opts];
		DeleteFile[tempFile];
		newCols=(Drop[#,-2])&/@Last[tempMfs];
		tempMfs=ReplacePart[tempMfs,newCols,3];
		tempMfs=mfsAddKey[tempMfs,{"TFSFILENAME", tfsFile}];
		tempMfs=mfsAddKey[tempMfs,{"DATE",fixDateFormat@StringTake[tfsFile,
{StringLength[tfsFile]-17,Last@Flatten@StringPosition[tfsFile,"97"]}]}];
bc=First[tempMfs][[5]];
newDescr=ReplacePart[
          First[tempMfs],{First[bc],{bc[[2]],bc[[3]],bc[[4]],bc[[5]]}},5];		
ReplacePart[tempMfs,newDescr,1]
] /; StringMatchQ[tfsFile,"Q_TNH*"]



Protect[ Evaluate[protected] ];



End[ ]







Protect[readThousandTurns,parseHeader]



EndPackage[ ]













































































































































































































