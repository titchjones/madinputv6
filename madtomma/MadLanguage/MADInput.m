(*******************************************************************
This file was generated automatically by the Mathematica front end.
It contains Initialization cells from a Notebook file, which
typically will have the same name as this file except ending in
".nb" instead of ".m".

This file is intended to be loaded into the Mathematica kernel using
the package loading commands Get or Needs.  Doing so is equivalent
to using the Evaluate Initialization Cells menu command in the front
end.

DO NOT EDIT THIS FILE.  This entire file is regenerated
automatically each time the parent Notebook file is saved in the
Mathematica front end.  Any changes you make to this file will be
overwritten.
***********************************************************************)



























































































































































BeginPackage["Madtomma`MadLanguage`MADInput`","JMJUtils`BumpEtc`"]





MADInput::usage = "MADInput.m is a package defining objects corresponding to elements of the MAD input language.";

MADcommand::usage=
    "MADcommand[mlist] converts the MADInput objects in mlist to \
corresponding MAD command strings. If mlist is a string then it is simply \
reproduced.  Note its option MADXsyntax.";

makeMADfile::usage=
    "makeMADfile[\"filename\",mlist] converts  the MADInput objects in mlist \
to a MAD input file.  If filename exists, a copy is saved before it is \
overwritten.";

MADcomment::usage=
    "MADcomment[string] is a MADInput object representing a comment.";

MADtag::usage=
    "MADtag[tagtext,madinput] is a MADInput object representing a structured \
comment wrapped around a MADInput object.";

MADtagNaked::usage=
    "MADtagNaked is an option of MADtag.  If True structure comments are not \
generated; the default is False.";

MADtitle::usage="MADtitle[string] is a MADInput object representing a title.";





MADtwiss::usage=
    "MADtwiss[filename,sequence,{columns},{{elementSelection1},{\
elementSelection2},...},{surveyOptions}] is a MAD input object that creates a \
TFS file containing the results of a TWISS calculation on the named sequence.\
\n{columns} is typically {\"NAME\",\"S\",\"BETX\"}.\n Each {elementSelection} \
should be a valid element selection for the MADselect function.\ntwissOptions \
can be any other valid options of MAD's TWISS command given in forms like, \
e.g., \"BETX\"->20.,\"DELTAP\"->{-0.001,0.001,0.0002}, \"CENTRE\"->\"True\".\n\
Quotation marks around the variables on the LHS and RHS of a rule like \"BETX\
\"->99.2 are strongly recommended.  They are essential when ValueQ[BETX] is \
True or for logical values.";







MADsurvey::usage=
    "MADsurvey[filename,sequence,{columns},{{elementSelection1},{\
elementSelection2},...},{twissOptions}] is a MAD input object that creates a \
TFS file containing the results of a SURVEY (geometry) calculation on the \
named sequence.\n{columns} is a subset of {\"NAME\",\"S\",\"L\",\"ANGLE\",\"X\
\",\"Y\",\"Z\",\"THETA\",\"PHI\",\"PSI\"}.\n Each {elementSelection} should \
be a valid element selection for the MADselect function.\nsurveyOptions can \
be any other valid options of MAD's TWISS command given in forms like, e.g., \
\"X0\"->\"20.\",\"THETA0\"->\"PI/2\".\nQuotation marks around the variables \
on the LHS and RHS of a rule like \"X0\"->\"20.\" are strongly recommended.  \
They are essential when ValueQ[X0] is True or for logical values.";

MADtwissColumns::usage="\!\(InputForm\`MADtwissColumns\)[tag] returns \
convenient sets of columns for MADtwiss; to find out what is available use \
??MADtwissColumns or domain[MADtwissColumns]; you can define further sets.";

MADsurveyColumns::usage="\!\(InputForm\`MADsurveyColumns\)[tag] returns \
convenient sets of columns for MADsurvey; to find out what is available use \
??MADtwissColumns or domain[MADtwissColumns]; you can define further sets.";

MAD8NamesCorrected::usage=
    "MAD8NamesCorrected is an option of MADtwissColumns.  When True, it \
specifies that the inconsistent MAD8 names for optics tables are to be \
corrected (\"X\" replaced by \"XC\", etc.).";

MADstart::usage=
    "MADstart[x,px,y,py,t,pt] is a MADInput object representing an initial \
condition for tracking specified in terms of absolute particle positions.";

MADstartN::usage=
    "MADstart[Fx,phix,Fy,phiy,Ft,phit] is a MADInput object representing an \
initial condition for tracking specified in terms of normalised particle \
positions.";

MADstartAA::usage=
    "MADstart[Ix,phix,Iy,phiy,It,phit] is a MADInput object representing an \
initial condition for tracking specified in terms of action angle coordinates \
of the particle.";

MADrun::usage=
    "MADrun[turns] is a MADInput object defining tracking for a given number \
of turns.";

MADtrack::usage=
    "MADtrack[trackcmds,options] is a MADInput object representing the \
tracking of the initial conditions  (usually MADstart objects) and number of \
turns  (usually MADrun objects) defined in trackcmds.";

Damp::usage=
    "Damp is an option for MADtrack which specifies whether radiation damping \
is to be switched on.  The default is Damp->False.";

OnePass::usage=
    "OnePass is an option for MADtrack which specifies whether tracking is to \
be for a single turn.  The default is OnePass->False.";

Quantum::usage=
    "Quantum is an option for MADtrack which specifies whether tracking is to \
be done with quantum fluctuations.  The default is Quantum->False.";

Deltap::usage=
    "Deltap is an option for MADtrack which specifies a momentum error on the \
TRACK command itself.  The default is none. N.B. this option is not used in \
some versions of MAD.";

MADarchive::usage=
    "MADarchive[tbl,\"filename\"]is a MADInput object representing the saving \
of a MAD table in a file called filename.  The table type tbl can be TRACK, \
EIGEN, TWISS, etc.  It is safer to put it in quotes.";

MADnewElement::usage=
    "MADnewElement[newel,{oldel,attrib1->val1,attrib2->val2}] defines a new \
element on the basis of an old one (or a \"class\" in MAD) with new values \
for some of its attributes.";

MADpoolLoad::usage=
    "MADpoolLoad[\"filename\"] is a MADInput object representing the loading \
of a pool file.";

MADpoolDump::usage=
    "MADpoolDump[\"filename\"] is a MADInput object representing the saving \
of a pool file.";

MADsystem::usage=
    "MADsystem[\"cmd\"] is a MADInput object representing an external system \
command.";

MADuse::usage=
    "MADuse[sequence] or MADuse[sequence,MADrange[...]] is a MADInput object \
representing MAD's USE command for a beam line or sequence. An optional \
argument MADrange[] can be given to restrict the range of elements provided \
that s2>s1 (see MADrange).";

MADoptics::usage=
    "MADoptics[\"filename\",columns->{\"col1\",\"col1\",...}] is a MADInput \
object representing the generation of an OPTICS table file.";

MADcall::usage=
    "MADcall[\"filename\"] is a MADInput object representing the switch to an \
alternative input file." ;

MADseqedit::usage=
    "MADseqedit[sequence,edits] is a MADInput object representing a complete \
sequence-editor script where edits is a list of MADInput objects \
corresponding the editing operations.";

MADinstall::usage=
    "MADinstall[newelement,AT->svalue,FROM->element] is a MADInput object \
representing an INSTALL command in the sequence-editor.";

MADremove::usage=
    "MADremove[element] is a MADInput object representing an REMOVE command \
in the sequence-editor.";

MADcycle::usage=
    "MADcycle[seq,el] is a MADinput object representing the cycling of a \
sequence to move its starting point to the element el.";

MADreflect::usage=
    "MADreflect[seq] is a MADinput object representing the reflection of a \
sequence.";

MADset::usage=
    "MADset[var,val] is a MADInput object representing a SET of the variable \
var to the value val.";

MADassign::usage=
    "MADassign[var,val] is a MADInput object representing the assignment (=)  \
of the value val to the variable var.";

MADsetDelayed::usage=
    "MADsetDelayed[var,val] is a MADInput object representing the delatyed \
assignment (:=)  of the value val to the variable var.";

MADXsyntax::usage=
    "MADXsyntax is an option of MADcommand that causes MAD commands to be \
generated with the syntax of MADX rather than the default MAD8.";

MADselect::usage=
    "MADselect[flag,{sel1,sel2,...}] is a MADInput object representing the \
MAD SELECT command, where sel1, sel2 are MAD selection objects (see MADclass, \
MADrange, MADpattern, MADfull, MADclear). Element selections are defined by  \
the first valid selection object of a given type, all taken together.  \
Further occurrences are ignored.  \nThe form \
MADselect[flag,{{sel11,sel12,..},{sel21,sel22,...}} generates multiple \
selections and the form MADselect[flag,sel] is equivalent to \
MADselect[flag,{sel}].";

MADuseMonitor::usage=
    "MADuseMonitor[switch,selectopts] is a MADInput object representing the \
MAD USEMONITOR command.  Switch should be True or False and selected monitors \
must satisfy all of the selectopts. These are similar to those of MADselect \
except that the FULL and CLEAR selections are not allowed.";

MADuseKick::usage=
    "MADuseKick[switch,selectopts] is a MADInput object representing the MAD \
USEMONITOR command.  Switch should be True or False and selected correctors \
must satisfy all of the selectopts. These are similar to those of MADselect \
except that the FULL and CLEAR selections are not allowed.";

MADselectionQ::usage=
    "MADselectionQ[sel] returns True if sel can be considered a valid \
selection option for MADselect.";

MADrange::usage=
    "MADrange[el1] or MADrange[{el1,el2}]  or MADrange[{{el1,s1},{el2,s2}}] \
is a MADInput object that can be used to specify a single element or a range \
or elements in MADselect (or similar) commands; if s2<s1 in the last form of \
the command a range overlapping the start/end of the ring sequence will be \
generated.  The arguments el1, el2 must be strings.";

MADclass::usage=
    "MADrange[class] is a MADInput object that can be used to specify a \
single element or a range or elements in a MADselect command.";

MADpattern::usage=
    "MADpattern[patt] is a MADInput object that can be used to specify \
selection pattern in a MADselect command.  The argument is a regular \
expression string in the limited sense used in MAD, typed exactly as in a MAD \
input file, e.g., MADpattern[\"MQ\\.*\"].";

MADclear::usage=
    "MADclear[] is a MADInput object that can be used to specify clearing the \
selection in a MADselect command.";

MADfull::usage=
    "MADfull[] is a MADInput object that can be used to specify the selection \
of everything in a MADselect command.";

MADealign::usage=
    "MADealign[{\[Delta]x,\[Delta]y,\[Delta]s},{\[Delta]\[Phi],\[Delta]\
\[Theta],\[Delta]\[Psi]},{MREX,MREY},{MSCALX,MXCALY}] is a MadInput object \
corresponding to MAD's EALIGN command. Arguments may be omitted on the right \
at either level, e.g. MADealign[{1},{0},{2}] corresponds to the MAD command \
EALIGN,DX=1,MREX=2;";



MADInput::badarg = "You called `1` with argument `2`!";

MADrange::invalid="Invalid range specification `1`";

MADselect::unused=
    "Invalid or redundant selection objects\n`1`\nare being ignored.";

MADuse::cycling=
    "Range `1` overlaps end of machine. It is probably necessary to cycle \
sequence `2` to start at `3`.";







Begin["`Private`"]













MADrule[]="";
MADrule[{}]="";

MADrule[a_/;AtomQ[a]&&!NumberQ[a]\[Rule]b_?StringQ]:=
  ToUpperCase[ToString[a]]<>"="<>b

MADrule[a_/;(AtomQ[a]&&!NumberQ[a])\[Rule]b_Symbol]:=MADrule[a,ToString[b]]

MADrule[a_/;(AtomQ[a]&&!NumberQ[a])\[Rule]b_?NumberQ]:=
  MADrule[a\[Rule]ToString[FortranForm[b]]]

MADrule[a_/;(AtomQ[a]&&!NumberQ[a])\[Rule]b_List]:=
  MADrule[a\[Rule]
      StringReplace[
        ToString[FortranForm[N[b]]],{"List("\[Rule]"",")"\[Rule]"",
          "\""\[Rule]""}]]

MADrule[a_/;(AtomQ[a]&&!NumberQ[a])\[Rule]b_]:=
  MADrule[a\[Rule]ToString[FortranForm[b]]]



MADrule[r_Rule,rs__Rule]:=MADrule[r]<>","<>MADrule[rs]

MADrule[rl_List]:=
  StringReplace[ToString[MADrule/@rl],{"{"\[Rule]"","}"\[Rule]""}]





whileRules[lhss_List,xys_List]:=
  Module[{maxl=Min[Length/@{xys,lhss}]},
    MapThread[Rule,{Take[lhss,maxl],Take[xys,maxl]}]]





Options[MADcommand]={MADXsyntax\[Rule]False}







MADcommand[MADcomment[c_String]]^:={"! "<>ToString[c]<>";"}





MADtitle/:MADcommand[MADtitle[c_String],opts___?OptionQ]:=Module[{madxs},
    madxs=MADXsyntax/.Flatten[{opts}]/.Options[MADcommand];
    {" ","TITLE,\""<>
        If[madxs||StringLength[c]<75,
          c,
          StringTake[ToString[c],75]
          ]<>"\";"}
    ]



Options[MADtag]={MADtagNaked\[Rule]False}



MADcommand[MADtag[tag_,madin_,opts___?OptionQ]]^:=Module[{nkd},
    nkd=MADtagNaked/.Flatten[{opts}]/.Options[MADtag];
    If[nkd,
      MADcommand[madin],
      MADcommand[{"",MADcomment["<"<>tag<>">"],madin,
          MADcomment["</"<>tag<>">"],""}]
      ]
    ]





MADset/:MADcommand[MADset[var_,value_],opts___?OptionQ]:=
  Module[{madxs},
    madxs=MADXsyntax/.\[InvisibleSpace]Flatten[{opts}]/.\[InvisibleSpace]\
Options[MADcommand];
    If[madxs,MADcommand[MADassign[var,value]],
      "SET,"<>ToString[var]<>","<>ToString[value]<>";"]]

MADcommand[MADassign[var_,value_]]^:=ToString[var]<>"="<>ToString[value]<>";"

MADcommand[MADsetDelayed[var_,value_]]^:=
  ToString[var]<>":="<>ToString[value]<>";"





MADcommand[MADnewElement[newel_,{el_,newval__Rule}]]^:=
  ToString[newel]<>": "<>ToString[el]<>","<>MADrule[newval]<>";"



MADseqedit/:MADcommand[MADseqedit[seq_,edits_]]:={
		"seqedit,sequence="<>ToString[seq]<>";",
		MADcommand[edits],
		"endedit;"
	}

MADcommand[
    MADinstall[el_,
      rules__Rule]]^:=("install,element="<>ToString[el]<>","<>MADrule[rules])<>
    ";"

MADcommand[MADremove[el_]]^:=("remove,element="<>ToString[el])<>";"

MADcycle/:MADcommand[MADcycle[seq_String,el_String]]:=
  MADcommand[MADtag["Cycle "<>seq<>" to move start point to "<>el,
      (MADseqedit[seq,"CYCLE,start="<>el])]]

MADreflect/:MADcommand[MADreflect[seq_String]]:=
  MADcommand[MADtag["Reflect "<>seq,{MADseqedit[seq,"REFLECT"]}]]







(* Old version - to delete
        MADselect/:MADcommand[MADselect[flag_,opts___?OptionQ]]:=
    Module[{selopts},
      "SELECT,FLAG="<>ToString[flag]<>","<>MADrule[opts]
      ]
  *)



MADselectionHeads={MADclear,MADclass,MADfull,MADrange,MADpattern,
    madSelectSplit}



MADselectionQ[sel_]:=MemberQ[MADselectionHeads,Head[sel]]









madcommand[MADrange[el1_String]]^:="RANGE="<>el1;

madcommand[MADrange[{el1_String,el2_String}]]^:="RANGE="<>el1<>"/"<>el2;

MADrange[{{el1_String,s1_?NumberQ},{el2_String,s2_?NumberQ}}/;
        s2\[GreaterEqual]s1]:=MADrange[{el1,el2}];



MADrange[{{el1_String,s1_?NumberQ},{el2_String,s2_?NumberQ}}/;s2<s1]:=
    madSelectSplit[MADrange[{el1,"#E"}],MADrange[{"#S",el2}]];

madcommand[MADclass[cl_String]]^:="CLASS="<>cl;





madcommand[MADpattern[patt_String]]^:="PATTERN="<>"\""<>patt<>"\"";

madcommand[MADclear[]]^:="CLEAR"

madcommand[MADfull[]]^:="RANGE=#S/#E"



(* old stuff 
      MADselect/:MADcommand[MADselect[flag_,opts___Rule]]:=
    Module[{selopts},
      "SELECT,FLAG="<>ToString[flag]<>","<>MADrule[opts]<>";"];
  MADselect/:MADcommand[MADselect[flag_,sel_?MADselectionQ]]:=
    Module[{selopts},
      "SELECT,FLAG="<>ToString[flag]<>","<>MADcommand[sel]<>";"] 
  *)

MADselect/:MADcommand[MADselect[flag_,sel_?MADselectionQ]]:=
    MADcommand[MADselect[flag,{sel}]];

MADselect/:MADcommand[MADselect[flag_,{}]]:=
    MADcommand[MADselect[flag,MADclear[]]];

MADselect/:MADcommand[MADselect[flag_,{a___,madSelectSplit[r1_,r2_],b___}]]:=
  MADcommand/@{MADselect[flag,{a,r1,b}],MADselect[flag,{a,r2,b}]}

MADselect/:MADcommand[MADselect[flag_,sell:{__?MADselectionQ}]]:=
  Module[{selsep},
    selsep={{},sell}/.\[InvisibleSpace]{{x___},{a___,MADclass[y_String],
                        b___}}\[Rule]{{x,MADclass[y]},{a,
                        b}}/.\[InvisibleSpace]{{x___},{a___,
                      MADpattern[y_String],b___}}\[Rule]{{x,MADpattern[y]},{a,
                      b}}/.\[InvisibleSpace]{{x___},{a___,MADrange[y_String],
                    b___}}\[Rule]{{x,MADrange[y]},{a,
                    b}}/.\[InvisibleSpace]{{x___},{a___,
                  MADrange[{y1_String,y2_String}],b___}}\[Rule]{{x,
                  MADrange[{y1,y2}]},{a,b}}/.\[InvisibleSpace]{{x___},{a___,
                MADfull[],b___}}\[Rule]{{x,MADfull[]},{a,
                b}}/.\[InvisibleSpace]{{x___},{a___,MADclear[],
              b___}}\[Rule]{{x,MADclear[]},{a,b}};
    If[Last[selsep]\[NotEqual]{},
      Message[MADselect::"redundant",Last[selsep]]];
    StringJoin[
      Flatten[{"SELECT,FLAG=",
          ToString[flag],({",",madcommand[#1]}&)/@First[selsep],";"}]]]

MADselect/:MADcommand[MADselect[flag_,selll:{{__?MADselectionQ}..}]]:=
    MADcommand[MADselect[flag,#]]&/@selll;

MADselect/:
    MADcommand[
      MADselect[flag_,specialword_/;ToUpperCase[specialword]=="FULL"]]:=
    MADcommand[MADselect[flag,MADrange[{"#S","#E"}]]];

MADselect/:
  MADcommand[
    MADselect[flag_,specialword_/;ToUpperCase[specialword]=="CLEAR"]]:=
  "SELECT,FLAG="<>ToString[flag]<>",CLEAR;"





MADuseMonitor/:
    MADcommand[
      MADuseMonitor[switch_/;switch\[Element]Booleans,sel_?MADselectionQ]]:=
    MADcommand[MADuseMonitor[switch,{sel}]];

MADuseMonitor/:
  MADcommand[
    MADuseMonitor[
      switch_/;switch\[Element]Booleans,{a___,madSelectSplit[r1_,r2_],b___}]]:=
  MADcommand/@{MADuseMonitor[switch,{a,r1,b}],MADuseMonitor[switch,{a,r2,b}]}

MADuseMonitor/:
  MADcommand[
    MADuseMonitor[switch_/;switch\[Element]Booleans,sell:{__?MADselectionQ}]]:=
  Module[{selsep},
    selsep={{},
                sell}/.\[InvisibleSpace]{{x___},{a___,MADclass[y_String],
                    b___}}\[Rule]{{x,MADclass[y]},{a,
                    b}}/.\[InvisibleSpace]{{x___},{a___,MADpattern[y_String],
                  b___}}\[Rule]{{x,MADpattern[y]},{a,
                  b}}/.\[InvisibleSpace]{{x___},{a___,MADrange[y_String],
                b___}}\[Rule]{{x,MADrange[y]},{a,
                b}}/.\[InvisibleSpace]{{x___},{a___,
              MADrange[{y1_String,y2_String}],b___}}\[Rule]{{x,
              MADrange[{y1,y2}]},{a,b}};
    If[Last[selsep]\[NotEqual]{},
      Message[MADselect::"redundant",Last[selsep]]];
    StringJoin[
      Flatten[{"USEMONITOR",({",",madcommand[#1]}&)/@First[selsep],",STATUS=",
          If[switch,"ON","OFF"],";"}]]]

MADuseMonitor/:
    MADcommand[
      MADuseMonitor[switch_/;switch\[Element]Booleans,
        selll:{{__?MADselectionQ}..}]]:=
    MADcommand[MADuseMonitor[switch,#]]&/@selll;

MADuseKick/:MADcommand[MADuseKick[x__]]:=
  Map[StringReplace[#,"USEMONITOR"->"USEKICK"]&,
    MADcommand@MADuseMonitor[x],-1]









MADuse/:MADcommand[
    MADuse[period_String,
      madSelectSplit[MADrange[{el1_String,_}],
        MADrange[{_,el2_String}]]]]:=(Message[MADuse::"cycling",{el1,el2},
      period,el1];
    MADcommand[
      MADtag["MADuse: has the sequence been cycled to "<>el1<>
          " ?",{MADcommand[MADuse[period,MADrange[{el1,el2}]]]}]])

MADuse/:MADcommand[MADuse[period_String,MADrange[rng:{_String,_String}]]]:=
    StringJoin["USE,PERIOD=",period,",",madcommand[MADrange[rng]],";"];

MADuse/:MADcommand[MADuse[period_String]]:=
    StringJoin["USE,PERIOD=",period,";"];



















(* old version 
    MADtwiss/:
        MADcommand[
          MADtwiss[filename_String,seq_String,cols:{_String..},
            selectopts:{{__?MADselectionQ}...},twopts:{___Rule}]]:=
        If[MADXsyntax/.Options[MADcommand],
          Module[{twoptstr},
            twoptstr=MADrule@@Flatten[{twopts}];
            twoptstr=If[StringLength[twoptstr]>0,","<>twoptstr,""];
            
            MADcommand[
              MADtag[("MADtwiss for sequence: "<>seq<>", making file: "<>
                    filename),
                {"SHOW,BEAM%"<>seq,
                  MADselect["TWISS",MADclear[]],
                  (
                    StringReplace[
                            MADcommand[MADselect["TWISS",Sequence@@#]],
                            ";"->","]<>
                          MADrule["COLUMN"\[Rule]cols]&/@selectopts
                    ),
                  
                  "TWISS,SEQUENCE="<>seq<>",file=\""<>filename<>"\""<>
                    twoptstr}
                ]]
            ],
          Module[{},
            
            MADcommand[
              MADtag[("MADtwiss for sequence: "<>seq<>", making file: "<>
                    filename),
                {"SHOW,BEAM",
                  MADselect["OPTICS",MADclear[]],
                  
                  MADcommand[
                    MADselect["OPTICS",Sequence@@Flatten[selectopts]]],
                  
                  MADoptics[filename,
                    Sequence@@Flatten[{"COLUMNS"\[Rule]cols,twopts}]],
                  "TWISS,SAVE=twiss,"<>MADrule[twopts]
                  }
                ]]
            ]
          ];
  *)

MADtwiss/:MADcommand[
      MADtwiss[filename_String,seq_String,cols:{_String..},
        selectopts:{{__?MADselectionQ}...},twopts:{___Rule}]]:=
    If[MADXsyntax/.Options[MADcommand],
      Module[{twoptstr},
        twoptstr=MADrule@@Flatten[{twopts}];
        twoptstr=If[StringLength[twoptstr]>0,","<>twoptstr,""];
        MADcommand[
          MADtag[("MADtwiss for sequence: "<>seq<>", making file: "<>
                filename),
            {"SHOW,BEAM%"<>seq,
              MADselect["TWISS",MADclear[]],
              (Map[
                  StringReplace[#,";"->","<>
                          MADrule["COLUMN"\[Rule]cols]]&,
                  MADcommand[MADselect["TWISS",selectopts]],-1]
                ),
              
              StringJoin[
                Flatten[{"TWISS,SEQUENCE=",seq,",file=\"",filename,"\"",
                    twoptstr}]]}
            ]]
        ],
      Module[{},
        MADcommand[
          MADtag[("MADtwiss for sequence: "<>seq<>", making file: "<>
                filename),
            {"SHOW,BEAM",
              MADselect["OPTICS",MADclear[]],
              MADcommand[MADselect["OPTICS",selectopts]],
              
              MADoptics[filename,
                Sequence@@Flatten[{"COLUMNS"\[Rule]cols,twopts}]],
              
              StringJoin[
                Flatten[{"TWISS,SAVE=twiss",{",",MADrule[#]}&/@twopts}]]
              }
            ]]
        ]
      ];

MADtwiss[filename_String,seq_String,cols:{_String..},
      selectopts:{__?MADselectionQ},twopts:{___Rule}:{}]:=
    MADtwiss[filename,seq,cols,{selectopts},twopts];

MADtwiss[filename_String,seq_String,cols:{_String..},selectopts_]:=
    MADtwiss[filename,seq,cols,selectopts,{}];



Options[MADtwissColumns]={MAD8NamesCorrected\[Rule]False}



MADtwissColumns["Classic"]={"NAME","S","BETX","BETY","ALFX","ALFY","DX","DPX",
      "X","PX","Y","PY","MUX","MUY"};

MADtwissColumns["Basic"]={"NAME","KEYWORD","S","BETX","BETY","MUX","MUY","DX",
      "X","Y"};

MADtwissColumns["Orbit"]={"NAME","S","X","PX","Y","PY","T","PT"};

MADtwissColumns["Strength"]={"NAME","S","L","K0L","K1L","K2L","K3L","K4L"};

MADtwissColumns[x_]={"NAME","S"}



MADtwissColumns[tag_String,opts___?OptionQ]:=Module[{m8corr},
    m8corr=MAD8NamesCorrected/.Flatten[{opts}]/.Options[MAD8NamesCorrected];
    tc=MADtwissColumns[tag];
    If[m8corr,
      tc/.{"X"\[Rule]"XC","PX"\[Rule]"PXC","Y"\[Rule]"YC","PY"\[Rule]"PYC",
          "T"\[Rule]"TC",
          "DELTAP"\[Rule]"PTC"},
      tc]
    ]





MADoptics/:
  MADcommand[MADoptics[file_String,opticsopts__Rule],opts___?OptionQ]:=
  Module[{madxs},
    Print["MADoptics ",file];
    Print[opticsopts];
    Print[opts];
    madxs=MADXsyntax/.Flatten[{opts}]/.Options[MADcommand];
    If[madxs,
      {MADcomment["MADoptics not available for MADX."],"STOP"},
      "OPTICS,CENTRE,filename=\""<>toUnixFileName[file]<>"\","
        <>StringReplace[MADrule[opticsopts],{"{"\[Rule]"","}"\[Rule]""}]<>";"
      ]
    ]

MADoptics/:
  MADcommand[MADoptics[file_String,opticsopts__Rule],opts___?OptionQ]:=
  MADcommand[MADoptics[file,opticsopts],opts]

MADoptics/:
  MADcommand[MADoptics[file_String,opticsopts__Rule],opts___?OptionQ,{}]:=
  MADcommand[MADoptics[file,opticsopts],opts]







MADsurvey/:
    MADcommand[
      MADsurvey[filename_String,seq_String,cols:{_String..},
        selectopts:{{__?MADselectionQ}...},twopts:{___Rule}]]:=
    If[MADXsyntax/.Options[MADcommand],
      Module[{twoptstr},
        twoptstr=MADrule@@Flatten[{twopts}];
        twoptstr=If[StringLength[twoptstr]>0,","<>twoptstr,""];
        MADcommand[
          MADtag[("MADsurvey for sequence: "<>seq<>", making file: "<>
                filename),
            {"SHOW,BEAM%"<>seq,
              MADselect["SURVEY",MADclear[]],
              (Map[
                  StringReplace[#,";"->","<>
                          MADrule["COLUMN"\[Rule]cols]]&,
                  MADcommand[MADselect["SURVEY",selectopts]],-1]
                ),
              
              StringJoin[
                Flatten[{"SURVEY,SEQUENCE=",seq,",file=\"",filename,"\"",
                    twoptstr}]]}
            ]]
        ],
      Module[{},
        MADcommand[
          MADtag[("MADsurvey for sequence: "<>seq<>", making file: "<>
                filename),
            {MADselect["SURVEY",MADclear[]],
              MADcommand[MADselect["SURVEY",selectopts]],
              
              MADoptics[filename,
                Sequence@@Flatten[{"COLUMNS"\[Rule]cols,twopts}]],
              
              StringJoin[
                Flatten[{"SURVEY",{",",MADrule[#]}&/@twopts,",TAPE=",
                    filename}]]
              }
            ]]
        ]
      ];

MADsurvey[filename_String,seq_String,cols:{_String..},
      selectopts:{__?MADselectionQ},twopts:{___Rule}:{}]:=
    MADsurvey[filename,seq,cols,{selectopts},twopts];

MADsurvey[filename_String,seq_String,cols:{_String..},selectopts_]:=
    MADsurvey[filename,seq,cols,selectopts,{}];



Options[MADsurveyColumns]={MAD8NamesCorrected\[Rule]False}



MADsurveyColumns[]=MADsurveyColumns["Classic"];

MADsurveyColumns["Classic"]={"NAME","S","L","ANGLE","X","Y","Z","THETA","PHI",
      "PSI"};

MADsurveyColumns["Basic"]={"NAME","S","X","Y","Z"};

MADsurveyColumns["Geometric"]={"NAME","S","X","Y","Z","THETA","PHI","PSI"};

MADsurveyColumns[x_]={"NAME","S"};

MADsurveyColumns[tag_String,opts___?OptionQ]:=Module[{m8corr},
    m8corr=MAD8NamesCorrected/.Flatten[{opts}]/.Options[MAD8NamesCorrected];
    tc=MADsurveyColumns[tag];
    If[m8corr,tc/.{"X"\[Rule]"XG","Y"\[Rule]"YG","Z"->"ZG"},
      tc]
    ]



MADealign/:MADcommand[MADealign[dxys_:{},dangles_:{},mre_:{},mscal_:{}]]:=
    StringJoin[Flatten[{"EALIGN,",
          MADrule[Select[
              {whileRules[{"DX","DY","DS"},dxys],
                whileRules[{"DPHI","DTHETA","DPSI"},dangles],
                whileRules[{"MREX","MREY"},mre],
                whileRules[{"MSCALX","MSCALY"},mscal]},#=!={}&]]
          }]];
MADealign[{}]="";











MADcommand[
    MADstart[x_/;NumericQ[x],px_/;NumericQ[px],y_/;NumericQ[y],
      py_/;NumericQ[py],t_/;NumericQ[t],pt_/;NumericQ[pt]]]^:=
  Module[{tp},
    tp=If[MADXsyntax/.Options[MADcommand],
        "start,X=``,PX=``,Y=``,PY=``,T=``,PT=``",
        "start,X=``,PX=``,Y=``,PY=``,T=``,DELTAP=``"];
    ToString[
        StringForm["start,X=``,PX=``,Y=``,PY=``,T=``,DELTAP=``",
          Sequence@@(FortranForm/@N[{x,px,y,py,t,pt}])]]<>";"]

MADcommand[
	MADstartN[
		fx_/;NumericQ[fx],
		phix_/;NumericQ[phix],
		fy_/;NumericQ[fy],
		phiy_/;NumericQ[phiy],
		ft_/;NumericQ[ft],
		phit_/;NumericQ[phit]
		]
]^:=ToString[
      StringForm["start,FX=``,PHIX=``,FY=``,PHIY=``,FT=``,PHIT=``",
        Sequence@@(FortranForm/@ N[{fx,phix,fy,phiy,ft,phit}])]]<>";"

MADcommand[
	MADstartAA[
		ix_/;NumericQ[ix],
		phix_/;NumericQ[phix],
  	iy_/;NumericQ[iy],
		phiy_/;NumericQ[phiy],
		it_/;NumericQ[it],
		phit_/;NumericQ[phit]
		]
]^:=ToString[
      StringForm["start,FX=``,PHIX=``,FY=``,PHIY=``,FT=``,PHIT=``",
        Sequence@@(FortranForm/@ 
              N[{\[Sqrt](2*ix),phix,\[Sqrt](2*iy),phiy,\[Sqrt](2*it),
                  phit}])]]<>";"

MADcommand[
    MADrun[turns_Integer]]^:={"RUN,TURNS="<>ToString[turns]<>",table=TRACK"}

Options[MADtrack]={OnePass\[Rule]False,Damp\[Rule]False,Quantum\[Rule]False,
    Deltap\[Rule]0}



MADcommand[MADtrack[trackcmds_,opts___Rule]]^:=
  Module[{onepass,damp,quantum,dpspec,deltap},
		onepass=If[OnePass/.{opts} /. Options[MADtrack],",ONEPASS",",-ONEPASS"];
		damp=If[Damp/.{opts} /. Options[MADtrack],",DAMP",",-DAMP"];
		quantum=If[Quantum/.{opts} /. Options[MADtrack],",QUANTUM",",-QUANTUM"];
    deltap=Deltap/.{opts}/.Options[MADtrack];
    dpspec=If[deltap=!=0,",DELTAP="<>ToString[deltap],""];
    {ToString[StringForm["TRACK `` `` `` ``;",damp,quantum,onepass,dpspec]],
      		MADcommand[trackcmds],"ENDTRACK;"}
    				]















MADcommand[MADcall[fn_String]]^:=
  ToString/@ {StringForm["call,file=\"``\";",toUnixFileName[fn]]}  

MADcommand[
    MADarchive[tbl_,file_String]]^:={"archive,table="<>ToString[tbl]<>
      ",filename=\""<>toUnixFileName[file]<>"\";"}



MADpoolLoad/:MADcommand[MADpoolLoad[file_String],opts___?OptionQ]:=
  Module[{madxs},
    madxs=MADXsyntax/.Flatten[{opts}]/.Options[MADcommand];
    If[madxs,
      {MADcommand[MADcomment["POOLLOAD not possible in MADX.","STOP"]]},
      "poolload,\""<>toUnixFileName[file]<>"\";"]
    ]

MADcommand[MADpoolDump[file_String],opts___?OptionQ]:=Module[{madxs},
    madxs=MADXsyntax/.Flatten[{opts}]/.Options[MADcommand];
    If[madxs,
      {MADcommand[
          MADcomment[
            "POOLDUMP not possible in MADX.  Continuing regardless."]]},
      "pooldump,\""<>toUnixFileName[file]<>"\";"
      ]]

MADcommand[MADsystem[cmd_String]]^:="system,\""<>cmd<>"\""<>";"



SetAttributes[MADcommand,Listable]



MADcommand[c_String]:=c<>";"



MADcommand[x_]:="!MADcommand?? "<>ToString[InputForm[x]]







makeMADfile[madfile_String,madfunc_,opts___?OptionQ]:=Module[{mfile,madxs},
    madxs=
      MADXsyntax/.\[InvisibleSpace]Flatten[{opts}]/.\[InvisibleSpace]Options[
          MADcommand];
    bumpFile[madfile];
    mfile=
      OpenWrite[madfile,
        DOSTextFormat\[Rule]False];(WriteString[mfile,#1<>"\n"]&)/@
      Flatten[
        If[madxs,Identity,MADstringbreak]/@
          MADcommand[Join[madfunc,{"return","!"}]]];
    Close[mfile]
    ]







MADcommentQ[str_String]:=Module[{strn},
    strn=StringReplace[str," "->""];
    StringMatchQ[strn,
        "!*"]||((MADXsyntax/.Options[MADcommand])&&StringMatchQ[strn,"//*"])
    ]





MADstringbreak[
      cmd_String /; (Not[MADcommentQ[cmd]]&&StringLength[cmd] > 80 && 
            StringMatchQ[cmd,"*\"*\"*"])] :=
    Module[{break,qt="\"",before,quoted,after},
		break = First /@ StringPosition[cmd,qt];
		before=StringTake[cmd,break[[1]]-1];
		quoted=StringTake[cmd,{break[[1]],break[[2]]}];
      after= StringTake[cmd,{break[[2]]+1,StringLength[cmd]}];
			MADstringbreak/@Flatten[ {
						before<>"&",
									If[Complement[Characters[after],{" "}]=={},
              quoted,{quoted<>"&",after}]
					}]
				]; 









MADstringbreak[
    cmd_String /;(Not[MADcommentQ[cmd]]&& StringLength[cmd] > 80)] := 
  Module[{breakpoint}, 
		breakpoint = 
      Max[Select[
          First /@ StringPosition[cmd, {",", " ", "=", ":"}], #1 < 78 & ]];  
		Flatten[{
				StringJoin[StringTake[cmd, breakpoint], " &"], 
        MADstringbreak[ StringTake[cmd, {breakpoint + 1, StringLength[cmd]}]]
			}]];

MADstringbreak[
      cmds_List]:= (Flatten[
        MADstringbreak/@Select[ Flatten[cmds],(StringLength[#]>1)&]]);

MADstringbreak[cmd_String] := cmd



End[]









(* Protect[ Function1, Function2 ] *)



Protect[Evaluate[Context[]<>"*"]]









EndPackage[ ]





























































































































































































































































































































































































































































































































































































































































































































































