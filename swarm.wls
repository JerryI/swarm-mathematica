PacletInstall["https://github.com/JerryI/tcp-mathematica/blob/main/JTP.paclet?raw=true", ForceVersionInstall -> True];
<<JTP`;

jtp = JTPServer["host"->"83.220.175.48", "port"->257] // JTPServerStart;

$kernels = <||>;
$tasks = <||>;

NullQ[x_]:=TrueQ[x == Null];

(*cli functions*)

KernelsAvailable[]:=With[{},
    Length[$kernels]
];

AddTask[task_, OptionsPattern[]]:=Module[{id = CreateUUID[]},

    $tasks[id] = task;
    $tasks[id]["started at"] = Now;
    $tasks[id]["author"]   = socket;
    $tasks[id]["sendto"]   = socket;
    $tasks[id]["method"] = OptionValue[Method];
    $tasks[id]["webvisor"] = OptionValue[WebVisor];
    $tasks[id]["onchange"] = OptionValue[OnChange];
    $tasks[id]["status"]   = "nondistributed";

    id
];

Options[AddTask] = {WebVisor -> Null, OnChange -> Null, Method -> "CoarsestGrained"}

CheckTasks[]:=Module[{kernels = Keys[$kernels], torecover={}},
    Do[ 
        If[
            $tasks[i]["status"] == "finished", 
            Continue[];
        ];

        If[
            $tasks[i]["status"] == "nondistributed",

            StringTemplate["⏳   distributing for ``"][$tasks[i]["name"]]//Print;
            Module[{kernels, remaining, n, groups=<||>},
                kernels = KernelsAvailable[];
                StringTemplate["`` kernels are available"][kernels]//Print;
                If[kernels == 0, 
                    StringTemplate["🕷️   no kernels available for ``"][$tasks[i]["name"]]//Print;
                    Continue[];
                ];

                (*distribute the dataset*)
                If[Length[$tasks[i]["dataset"]] < kernels,
                    StringTemplate["    not so many datasets... splitted to the groups of ``"][Length[$tasks[i]["dataset"]]]//Print;
                    groups = Table[
                            With[{j=j}, 
                                j-><|"range"->{j;;j}, "kernel"->Keys[$kernels][[j]], "status"->"initialising"|>
                            ]
                        , {j,1,Length[$tasks[i]["dataset"]]}]//Association;
                ,

                    n = (Length[$tasks[i]["dataset"]]/kernels)//Floor;
                    remaining = Length[$tasks[i]["dataset"]] - n kernels;

                    StringTemplate["    `` data per kernel and `` is extra"][n, remaining]//Print;
                    groups = Table[
                            With[{j=j}, 
                                j-><|"range"->{j;;j+n-1}, "kernel"->Keys[$kernels][[Floor[j/n]+1]], "status"->"initialising"|>
                            ]
                        , {j,1,kernels n,n}]//Association;

                    If[remaining > 0,
                       groups["extra"]=
                            <|
                                "range"->{Length[$tasks[i]["dataset"]]-remaining + 1;;Length[$tasks[i]["dataset"]]},
                                "kernel"->RandomChoice[Keys[$kernels]],
                                "status"->"initialising",
                                "extra"->True
                            |>;
                    ];

                    StringTemplate["    splitted to the groups of ``"][Length[groups]]//Print;
                ];  

                $tasks[i]["groups"] = groups;  
                $tasks[i]["status"] = "waiting";            
            
                Continue[];
            ];

            
        ];

        Module[{integrity = 0, working = 0, waiting = 0},
            (*check all groups*)
             
            Map[  
                With[{group = $tasks[i]["groups"][#]},
                    $tasks[i]["groups"][#] = If[MemberQ[kernels, group["kernel"]],
                        Switch[group["status"],

                            "initialising",

                                (*init all*)
                                With[{},
                                    If[!TrueQ[# == "extra"],
                                        Do[
                                            With[{name = Extract[m[[1]], 1, Unevaluated], val = m[[2]]},
                                                JTPSend[group["kernel"], With[{}, Clear[name]; Set[name, val]; Print["✔   initialised!"];]];
                                            ]
                                        , {m, $tasks[i]["init"]}];
                                    ]

                                ];

                                Print["✔    all variabels are initialized (we just hope)"]; 
                                waiting = waiting + 1;
                                Append[group, "status"->"waiting"]
                            ,

                            "waiting",
                                StringTemplate["⏳   sending task for worker ``"][group["kernel"]]//Print;

                                With[{taskid = i, taskname=$tasks[i]["name"], groupid=#, piece = $tasks[i]["dataset"][[group["range"]//First]], exp = $tasks[i]["exp"]},
                                    Switch[$tasks[i]["method"],

                                        "CoarsestGrained",
                                        (*first method*)
                                        JTPSend[group["kernel"],
                                            Module[{},
                                                StringTemplate["⏳   task `` (group ``) added from server"][taskname, groupid]//Print;

                                                cronAdd@<|
                                                    "period" -> Quantity[0.1,"Seconds"],
                                                    "onetime" -> True,
                                                    "action":>                              
                                                            With[{result = exp@@ # & /@ piece},
                                                                StringTemplate["✔   task `` (group ``) has finished"][taskname, groupid]//Print;
                                                                JTPClientSend[swarm, With[{},
                                                                    $tasks[taskid]["groups"][groupid] = Append[$tasks[taskid]["groups"][groupid], {"status"->"finished", "result"->result}];

                                                                    If[!NullQ[$tasks[taskid]["onchange"]],  
                                                                        "🪄   calling handler"//Print;
                                                                        With[{res = $tasks[taskid]["onchange"][result]},
                                                                            JTPSend[$tasks[taskid]["sendto"], TaskPartialData[taskid,res]];
                                                                        ];
                                                                    ];        

                                                                ]];
                                                            ]
                                                |>;

                                            ]
                                        ];
                                        ,
                                        
                                        "FinestGrained",
                                        (*second method*)

                                        With[{pie = piece[[1]]},
                                            JTPSend[group["kernel"],
                                                Module[{},
                                                    StringTemplate["⏳   first piece task `` (group ``) added from server"][taskname, groupid]//Print;
                                                    cronAdd@<|
                                                        "period" -> Quantity[0.1,"Seconds"],
                                                        "onetime" -> True,
                                                        "action":>                              
                                                                With[{result = exp@@ pie},
                                                                    StringTemplate["✔   first piece task `` (group ``) has finished"][taskname, groupid]//Print;
                                                                    JTPClientSend[swarm, With[{},
                                                                        $tasks[taskid]["groups"][groupid] = Append[$tasks[taskid]["groups"][groupid], {"result"->{result}}];
                                                                        If[!NullQ[$tasks[taskid]["onchange"]],  
                                                                            "🪄   calling handler"//Print;
                                                                            With[{res = $tasks[taskid]["onchange"][{result}]},
                                                                                JTPSend[$tasks[taskid]["sendto"], TaskPartialData[taskid,res]];
                                                                            ];
                                                                        ];        
                                                                    ]];
                                                                ]
                                                    |>;
                                                ]
                                            ];
                                        ];

                                        Do[
                                            With[{pie = piece[[i]]},
                                                JTPSend[group["kernel"],
                                                    Module[{},
                                                        StringTemplate["⏳   piece task `` (group ``) added from server"][taskname, groupid]//Print;

                                                        cronAdd@<|
                                                            "period" -> Quantity[0.1,"Seconds"],
                                                            "onetime" -> True,
                                                            "action":>                              
                                                                    With[{result = exp@@ pie},
                                                                        StringTemplate["✔   piece task `` (group ``) has finished"][taskname, groupid]//Print;
                                                                        JTPClientSend[swarm, With[{partial = $tasks[taskid]["groups"][groupid]["result"]},
                                                                            $tasks[taskid]["groups"][groupid] = Append[$tasks[taskid]["groups"][groupid], {"result"->Append[partial, result]}];

                                                                            If[!NullQ[$tasks[taskid]["onchange"]],  
                                                                                "🪄   calling handler"//Print;
                                                                                With[{res = $tasks[taskid]["onchange"][{result}]},
                                                                                    JTPSend[$tasks[taskid]["sendto"], TaskPartialData[taskid,res]];
                                                                                ];
                                                                            ];        

                                                                        ]];
                                                                    ]
                                                        |>;

                                                    ]
                                                ];
                                            ];
                                        , {i, 2, Length[piece]-1}];

                                        With[{pie = piece//Last},
                                            JTPSend[group["kernel"],
                                                Module[{},
                                                    StringTemplate["⏳   last piece task `` (group ``) added from server"][taskname, groupid]//Print;
                                                    cronAdd@<|
                                                        "period" -> Quantity[0.1,"Seconds"],
                                                        "onetime" -> True,
                                                        "action":>                              
                                                                With[{result = exp@@ pie},
                                                                    StringTemplate["✔   last piece task `` (group ``) has finished"][taskname, groupid]//Print;
                                                                    JTPClientSend[swarm, With[{partial = $tasks[taskid]["groups"][groupid]["result"]},
                                                                        $tasks[taskid]["groups"][groupid] = Append[$tasks[taskid]["groups"][groupid], {"status"->"finished", "result"->Append[partial, result]}];
                                                                        If[!NullQ[$tasks[taskid]["onchange"]],  
                                                                            "🪄   calling handler"//Print;
                                                                            With[{res = $tasks[taskid]["onchange"][{result}]},
                                                                                JTPSend[$tasks[taskid]["sendto"], TaskPartialData[taskid,res]];
                                                                            ];
                                                                        ];        
                                                                    ]];
                                                                ]
                                                    |>;
                                                ]
                                            ];
                                        ];                                        

                                    ];
                                ];

                                working = working + 1;
                                Append[group, "status"->"working"]
                            ,
                            "working",
                                working = working + 1;
                                group
                            ,
                            "finished",
                                integrity = integrity + 1;
                                group
                        ]
                    
                    ,
                        StringTemplate["❌   the worker `` is absent... going to the recovery mode"][#1["kernel"]]//Print;
                        AppendTo[torecover, #];
                        Append[group, "status"->"failed"]
                    ] 
                ]&
                , $tasks[i]["groups"]//Keys
              ];   
              
              If[Length[torecover] > 0,
                 "🛠    entering the recovery mode..."//Print;
                Module[{kernelslocal = kernels, looser},
                    Do[
                        If[Length[kernels] == 0, Break[]];
                        looser = kernelslocal//Last;

                        StringTemplate["😉    Please, ``, do the group `` from ``."][$kernels[looser]["name"],k,$tasks[i]["name"]]//Print;
                        $tasks[i]["groups"][k]["kernel"] = looser;
                        $tasks[i]["groups"][k]["status"] = "initialising";

                        waiting = waiting + 1;
                    

                        kernelslocal = Drop[kernelslocal,-1];
                        If[Length[kernelslocal] == 0, kernelslocal = kernels];

                    , {k,torecover}];
                ];
              ];

              StringTemplate["🛠    We have `` working, `` waiting and `` has finished for task ``."][working,waiting,integrity,$tasks[i]["name"]]//Print;
              $tasks[i]["progress"] = integrity/Length[$tasks[i]["groups"]];
              If[integrity == Length[$tasks[i]["groups"]],
                StringTemplate["👍  The task `` was finished!"][$tasks[i]["name"]]//Print;
                $tasks[i]["status"] = "finished";
                $tasks[i]["finished at"] = Now;
                $tasks[i]["result"] = Join@@($tasks[i]["groups"][#]["result"]&/@Keys[$tasks[i]["groups"]]);
                With[{id = i, results = $tasks[i]["result"], time = $tasks[i]["finished at"] - $tasks[i]["started at"]},
                    JTPSend[$tasks[i]["author"], TaskFinished[id, <|"result"->results, "time"->time|>]];
                ];
                
              ];    
        ];
    , {i, Keys[$tasks]}];
];

HowIsIt[id_]:=Module[{},
    If[TrueQ[$tasks[id]["status"] == "finished"], $tasks[id]["result"] , $tasks[id]["status"]]
];


(*for kernels*)

RegisterMe[name_, desc_]:=Module[{},
    If[name == "At Riot A", Print["no!"]; Return[]];
    $kernels[socket] = <|"name"->name, "desc"->desc, "date"->Now|>;
    StringTemplate["Welcome, ``!"][name]//Print;
    
    "hello "<>name<>"!"
];

$pongs = <||>;

Pong[id_]:=Module[{},
    logWrite["🏓 from "<>$kernels[socket]["name"]];
    $kernels[socket]["uptime"] = Now - $kernels[socket]["date"]; 
    $pongs[id]["status"] = "ok";
    "good"
];

AliveQ:=If[MemberQ[$kernels//Keys, socket], Ja, FuckOffMan];

cronAdd@<|
    "period" -> Quantity[5,"Seconds"],
    "action":>CheckTasks[]
|>;

cronAdd@<|
    "period" -> Quantity[10,"Minutes"],
    "action":>Print[Now//DateString]
|>;

cronAdd@<|

    "period" -> Quantity[1,"Minutes"],
    "action":>(Print/@(jtp["log"]["Elements"]))
|>;

cronAdd@<|
    "period" -> Quantity[15,"Seconds"],
    "action":>With[{},
                Do[
                    With[{uid = CreateUUID[]},
                        $pongs[uid] = <|"status"->"waiting", "date"->Now, "kernel"->i|>;
                        JTPSend[i, Ping[uid]];

                    ];
                , {i, $kernels//Keys}];
             ]
|>;


cronAdd@<|
    "period" -> Quantity[3,"Seconds"],
    "action":>With[{},
                Do[
                    If[$pongs[i]["status"] == "ok",
                        $pongs[i] = .;
                    ,
                        If[Now - $pongs[i]["date"] > Quantity[6, "Seconds"],
                            
                            logWrite["😢    kernel "<>$kernels[$pongs[i]["kernel"]]["name"]<>" is dead..."];
                            $kernels[$pongs[i]["kernel"]]=.;
                            streams[$pongs[i]["kernel"]]=.;
                            Close[SocketObject[$pongs[i]["kernel"]]];
                            $pongs[i] = .;
                        ];
                    ];
                , {i, $pongs//Keys}];
             ]
|>;