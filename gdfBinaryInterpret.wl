(* ::Package:: *)

BeginPackage["gdfBinaryInterpret`"];


gdfBinaryInterpret::usage="gdfBinaryInterpret[\"filename\"] interprets an GDF Binary-Formatted File";


Begin["`Private`"]


hex2dec[str_]:=FromDigits[ToExpression[Characters[str]],16]


t$ascii  = hex2dec["0001"];
t$s32    = hex2dec["0002"];
t$dbl    = hex2dec["0003"];
t$undef  = hex2dec["0000"];
t$null	 = hex2dec["0010"];
t$u8	 = hex2dec["0020"];
t$s8	 = hex2dec["0030"];
t$u16	 = hex2dec["0040"];
t$s16	 = hex2dec["0050"];
t$u32	 = hex2dec["0060"];
t$u64	 = hex2dec["0070"];
t$s64	 = hex2dec["0080"];
t$flt	 = hex2dec["0090"];
(* Block types *)
t$dir    = hex2dec["0100"];
t$edir   = hex2dec["0200"];
t$sval   = hex2dec["0400"];
t$arr    = hex2dec["0800"];


GDFID  = 94325877;


$replacementrules={"@"->"\[Sterling]","|"->"$"};


gdfBinaryInterpret[filein_, opts___] := Block[{verbose, ans, append="", tmpans, tmpdata, tmparray,prepend,postpend},
verbose = Global`gdfVerbose /. {opts} /. {Global`gdfVerbose -> False};
prepend = Global`gdfPrefix /. {opts} /. {Global`gdfPrefix -> ""};
postpend = Global`gdfPostfix /. {opts} /. {Global`gdfPostfix -> ""};
file = OpenRead[filein, BinaryFormat -> True];
                      
ID = BinaryRead[file, "UnsignedInteger32"];
If[ID === GDFID,
$GDFTimeCreated = BinaryRead[file, "UnsignedInteger32"];
ans = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$GDFCreator = FromCharacterCode[Take[ans, Position[ans, 0][[1, 1]] - 1]];
ans = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$GDFDestination = FromCharacterCode[Take[ans, Position[ans, 0][[1, 1]] - 1]];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFVersion = ToString[major] <> "." <> ToString[minor];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFCreatorVersion = ToString[major] <> "." <> ToString[minor];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFDestinationVersion = ToString[major] <> "." <> ToString[minor];
Table[BinaryRead[file, "UnsignedInteger8"], {2}];
columnnames = parameternames = {};
level = 1;
lastarr = False;
$name = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$time = AbsoluteTime[];
While[Not[MemberQ[$name, EndOfFile]],
$name = FromCharacterCode[Take[$name, Position[$name, 0][[1, 1]] - 1]];
$name = StringReplace[$name, $replacementrules];
If[$name==="time",append="t",
If[$name==="position",append="p"]];
$name = StringJoin[prepend,$name, append,postpend];
$type = BinaryRead[file, "UnsignedInteger32"];
$size = BinaryRead[file, "UnsignedInteger32"];

isDir = BitAnd[$type, t$dir] > 0;
iseDir = BitAnd[$type, t$edir] > 0;
issVal = BitAnd[$type, t$sval] > 0;
isArray = BitAnd[$type, t$arr] > 0;
 datType = BitAnd[$type, 255];

If[issVal,Switch[datType,
t$dbl, tmpans = BinaryRead[file, "Real64"];,
t$null, tmpans = Null;,
t$ascii, tmpdata = BinaryReadList[file, "UnsignedInteger8", $size];tmpans = FromCharacterCode[Take[tmpdata, Position[tmpdata, 0][[1, 1]] - 1]],
t$s32, tmpans = BinaryRead[file, "Integer32"],
_, If[$size > 0,
tmpdata = BinaryReadList[file, "UnsignedInteger8", $size];
tmpans = FromCharacterCode[Take[tmpdata, Position[tmpdata, 0][[1, 1]] - 1]]
]];

If[MemberQ[parameternames, $name],
tmparr = Evaluate[Symbol@$name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {Sequence @@ tmparr, tmpans};,
AppendTo[parameternames, $name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {tmpans};
]
];

If[isArray,
tmpans = BinaryReadList[file, "Real64", $size/8];
If[MemberQ[columnnames, $name],
tmparr = Evaluate[Symbol@$name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {Sequence @@ tmparr, tmpans};,
AppendTo[columnnames, $name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {tmpans};
];
];
lastarr = isArray;
$name = Table[BinaryRead[file, "UnsignedInteger8"], {16}]
];
If[verbose,
Print["Variables (Re)Assigned: " <> ToString[Union[columnnames]]];
Print["Parameters (Re)Assigned: " <> ToString[Union[parameternames]]]
];
Close[file];,
Print["Not a Binary GDF File!"];
]
]


gdfBinaryInterpret[filesin_List, opts___] := Block[{verbose, ans, append="", tmpans, tmpdata, tmparray,prepend,postpend},
verbose = Global`gdfVerbose /. {opts} /. {Global`gdfVerbose -> False};
prepend = Global`gdfPrefix /. {opts} /. {Global`gdfPrefix -> ""};
postpend = Global`gdfPostfix /. {opts} /. {Global`gdfPostfix -> ""};

columnnames = parameternames = {};

Block[{},
file = OpenRead[#, BinaryFormat -> True];
ID = BinaryRead[file, "UnsignedInteger32"];
If[ID === GDFID,
$GDFTimeCreated = BinaryRead[file, "UnsignedInteger32"];
ans = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$GDFCreator = FromCharacterCode[Take[ans, Position[ans, 0][[1, 1]] - 1]];
ans = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$GDFDestination = FromCharacterCode[Take[ans, Position[ans, 0][[1, 1]] - 1]];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFVersion = ToString[major] <> "." <> ToString[minor];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFCreatorVersion = ToString[major] <> "." <> ToString[minor];
major = BinaryRead[file, "UnsignedInteger8"];
minor = BinaryRead[file, "UnsignedInteger8"];
$GDFDestinationVersion = ToString[major] <> "." <> ToString[minor];
Table[BinaryRead[file, "UnsignedInteger8"], {2}];
level = 1;
lastarr = False;
$name = Table[BinaryRead[file, "UnsignedInteger8"], {16}];
$time = AbsoluteTime[];
While[Not[MemberQ[$name, EndOfFile]], $name = FromCharacterCode[Take[$name, Position[$name, 0][[1, 1]] - 1]];     $name = StringReplace[$name, $replacementrules];
If[$name==="time",append="t",
If[$name==="position",append="p"]];
$name = StringJoin[prepend,$name, append,postpend];
$type = BinaryRead[file, "UnsignedInteger32"];
$size = BinaryRead[file, "UnsignedInteger32"];

isDir = BitAnd[$type, t$dir] > 0;
iseDir = BitAnd[$type, t$edir] > 0;
issVal = BitAnd[$type, t$sval] > 0;
isArray = BitAnd[$type, t$arr] > 0;
 datType = BitAnd[$type, 255];

If[issVal,Switch[datType,
t$dbl, tmpans = BinaryRead[file, "Real64"];,
t$null, tmpans = Null;,
t$ascii, tmpdata = BinaryReadList[file, "UnsignedInteger8", $size];tmpans = FromCharacterCode[Take[tmpdata, Position[tmpdata, 0][[1, 1]] - 1]],
t$s32, tmpans = BinaryRead[file, "Integer32"],
_, If[$size > 0,
tmpdata = BinaryReadList[file, "UnsignedInteger8", $size];
tmpans = FromCharacterCode[Take[tmpdata, Position[tmpdata, 0][[1, 1]] - 1]]
]];

If[MemberQ[parameternames, $name],
tmparr = Evaluate[Symbol@$name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {Sequence @@ tmparr, tmpans};,
AppendTo[parameternames, $name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {tmpans};
]
];

If[isArray,
tmpans = BinaryReadList[file, "Real64", $size/8];
If[MemberQ[columnnames, $name],
tmparr = Evaluate[Symbol@$name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {Sequence @@ tmparr, tmpans};,
AppendTo[columnnames, $name];
Clear[Evaluate[$name]];
Evaluate[(Symbol@$name)] = {tmpans};
];
];
lastarr = isArray;
$name = Table[BinaryRead[file, "UnsignedInteger8"], {16}]
];
Close[file];,
Print["Not a Binary GDF File!"];
]]&/@filesin;
If[verbose,
Print["Variables (Re)Assigned: " <> ToString[Union[columnnames]]];
Print["Parameters (Re)Assigned: " <> ToString[Union[parameternames]]]
];
]


End[]


EndPackage[]
