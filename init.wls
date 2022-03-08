#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input:: *)

SetAttributes[logWrite,HoldAllComplete];
logWrite[message_]:=WriteString[Streams["stdout"],message<>"\n"];
SetAttributes[logWriteDebug,HoldAllComplete];
logWriteDebug[message_]:=Null;(*logWrite[message];*)
logError[message_]:=(WriteString[Streams["stdout"],"<ERROR> "<>message];Exit[];)

cronAdd[func_]:=Module[{ev=<||>},
    ev = func;
    ev["time"] = Now;
    $crontasks[CreateUUID[]] = ev;
];

NullQ[x_]:=TrueQ[x == Null];

$crontasks = <||>;

logWrite["<INITIALIZATION STARTS>"];

logWrite["Autolaunch"];
$processes=Select[FileNames["*.wls"], Function[x, x != "init.wls"]];
logWrite[ToString[$processes]];
Do[Import[i],{i,$processes}];

logWrite["Cron started"]; 

While[True,
    (*all cron's tasks*)
    Do[
            With[{i = $crontasks[j]},
                If[Now - i["time"] > i["period"],
                    i["action"];
                    $crontasks[j]["time"] = Now;
                    If[KeyExistsQ[i,"onetime"], $crontasks[j]=.];
                ];
            ];
    , {j, $crontasks//Keys}];

    Pause[0.1];
]



