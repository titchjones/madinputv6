(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["ASTRARun`"]


(* ::Input::Initialization:: *)
$ASTRAExecutable="Astra.exe";


(* ::Input::Initialization:: *)
ASTRARun::usage="ASTRARun[filename]"


(* ::Input::Initialization:: *)
ASTRARunInteractive::usage"ASTRARunInteractive[filename,opts];\nOutput\[Rule]True prints output from ASTRA to the screen"


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Input::Initialization:: *)
ASTRARun[filename_]:=Block[{dir,output},
dir=Directory[];
SetDirectory[DirectoryName[filename]];
output=ReadList["!"<>$ASTRAExecutable<>" \""<>FileBaseName[filename]<>"\"",Record];
SetDirectory[dir];
output
]


(* ::Input::Initialization:: *)
Clear[ASTRARunInteractive];
ASTRARunInteractive[filename_,opts___Rule]:=Block[{dir,output,process,verbose,nolines=0},
scrollposition={1,1};
verbose=Global`Output/.{opts}/.{Global`Output->True};
processString="";
dir=Directory[];
SetDirectory[DirectoryName[filename]];
process=StartProcess[{$ASTRAExecutable,"\""<>FileBaseName[filename]<>"\""}];
If[verbose,
Quiet@Print[Column[{
Pane[Dynamic[processString],ImageSize->{800,400},Scrollbars->{False,True},ScrollPosition->Dynamic[scrollposition]],
Button["Kill!",KillProcess[process];]
}]];
Pause[.1];
While[ProcessStatus[process,"Running"],
processString=processString<>Quiet[Check[ReadString[process,EndOfBuffer],""]];
If[StringCount[processString,"\n"]>nolines,
nolines=StringCount[processString,"\n"];
(*scrollposition={-1,nolines*33};*)
];
Pause[.1];
];
];
SetDirectory[dir];
]


(* ::Input::Initialization:: *)
End[]


(* ::Input::Initialization:: *)
EndPackage[]
