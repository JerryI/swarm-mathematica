<|
    "package"->"tinyweb",
    "version"->0.1,
    "man"->With[{},
        {
        "Use 'init[address,path-to-public-folder]' to run the server",
        "Use 'stop[obj]' to stop and close all sockets",
        "Use 'loop' for dynamics on the client's. It should be called in the main loop of your program."
        }//TableForm],

    "start"->Function[{addr, path}, Module[{skipWSP=False,This=This[],handler},
        $clients = {};
        $tracked = <||>;

        $session = <||>;

        (*check the env*)
        If[TrueQ[(Cells[] // Length ) > 0],
            (*notebook*)
            $logWrite=Print;,
            (*wolframscript*)
            If[TrueQ[Information[WSP][[1]]["DownValues"] == None], $logWrite[message_]:=WriteString[Streams["stdout"],message<>"\n"]];
        ]

        "::Tiny Web Server::"//$logWrite;
        ("::V"<>ToString[%["version"]]<>"::")//$logWrite;

        If[TrueQ[Information[WSP][[1]]["DownValues"] == None],
            "No WSP module was found..."//$logWrite;
            "Only raw html will be shown,"//$logWrite;
            skipWSP = True;
        ];

        "cheking for updates..."//$logWrite;

        $connection = <||>;
        
        handler[message_]:=Module[{stream, header},
            $logWrite["handler"];
            (*$logWrite[message["Data"]];*)

            If[KeyExistsQ[$connection,message["SourceSocket"]],
                $logWrite["alive connection"];

                $logWrite["checking promises"];
                $logWrite[ToString[TrueQ[$connection[message["SourceSocket"]]["unfinished"]==True]]];
                $logWrite[$connection[message["SourceSocket"]]["unfinished"]//Compress];
                $logWrite["-----"];

                (*continue unfinished/multipart transactions*)
                If[TrueQ[$connection[message["SourceSocket"]]["unfinished"]==True],
                    $logWrite["found..."];

                    $connection[message["SourceSocket"]]["buffer"] = Join[$connection[message["SourceSocket"]]["buffer"],message["DataByteArray"]];
                    $logWrite["merge new data"];
                    $logWrite["length now: "<>ToString[Length[$connection[message["SourceSocket"]]["buffer"]]]];
                    $logWrite["condition"];
                    $logWrite[ToString[$connection[message["SourceSocket"]]["condition"]//ReleaseHold]];

                    If[$connection[message["SourceSocket"]]["condition"]//ReleaseHold,
                        $connection[message["SourceSocket"]] = Append[$connection[message["SourceSocket"]], <|"unfinished"->False|>];
                        $logWrite["status: "<>ToString[$connection[message["SourceSocket"]]["unfinished"]]];
                        $logWrite["goto promises"];
                        Do[i[message["SourceSocket"]],{i,$connection[message["SourceSocket"]]["promise"]}];

                        Return[];
                    ];

                ,

                    stream = message["Data"]//StringToStream;

                    header = ReadLine[stream];

                    $session = StringCases[header, RegularExpression["([A-Z]+) /(.*) "] :> {"request" -> "$1", "url" -> URLParse["$2"]}] // First // Association;

                    (*prepare stuff*)

                    $logWrite["http " <> $session["request"]];
           

                    $session = 
                        Join[$session, 
                        Join @@ ((StringCases[#, 
                                RegularExpression["(\\S*): (.*)"] :> <|"$1" -> "$2"|>] // 
                                First) & /@ (StringSplit[ReadString[stream, "\r\n\r\n"], 
                            "\r\n"]))];

                    If[$session["request"]=="GET", 

                        $connection[message["SourceSocket"]] = <|"session"->$session|>;
                        HTTP[message["SourceSocket"]]
                    ];

                If[$session["request"]=="POST",

                    If[StringContainsQ[$session["Content-Type"],"multipart/form-data"],
                        (*files and etc*)
                        boundary=StringCases[$session["Content-Type"],RegularExpression["(.*); boundary=(.*)"] :> <|"type" -> "$1", "boundary" -> "$2"|>][[1]]["boundary"];
                        ("boundary: "<>boundary)//$logWrite;

                        body = ReadString[stream];

                        $connection[message["SourceSocket"]] = <|"session"->$session, "boundary"->boundary, "buffer"->StringToByteArray@body, "unfinished"->True, 
                        "condition"->Hold[
                            ToExpression[$connection[message["SourceSocket"]]["session"]["Content-Length"]]==Length[$connection[message["SourceSocket"]]["buffer"]]-4
                        ],
                        "promise"->{
                                DecodeMultipart, 
                                HTTP
                            }
                        |>;

                        Close[stream];
                        Return[];

                    ,

                        (*for the forms*)
                        body = StringDrop[ReadString[stream], 4];

                        $logWrite["length received: "<>ToString[body//StringLength]];
                        $logWrite["length expected: "<>$session["Content-Length"]];

                        $logWrite["bodyform"];
                        $logWrite[body];
                        "-------------"//$logWrite;

                        If[StringLength@body != ToExpression[$session["Content-Length"]],
                            $logWrite["unfinished transaction... "];
                            $connection[message["SourceSocket"]] = <|"session"->$session, "buffer"->StringToByteArray@body, "unfinished"->True, 
                            "condition"->Hold[
                                ToExpression[$connection[message["SourceSocket"]]["session"]["Content-Length"]]==Length[$connection[message ["SourceSocket"]]["buffer"]]
                            ], 
                            "promise"-> {
                                    DecodeMultipartString, 
                                    HTTP
                                }
                            |>;

                            Close[stream];
                            Return[];
                        ];

                        $session["data"] = URLParse["?" <> body]["Query"]//Association;
                        $logWrite[$session["data"]//ToString];

                        Close[stream];

                        $connection[message["SourceSocket"]] = <|"session"->$session|>;
                        HTTP[message["SourceSocket"]]
                    ];
                 ];
                ];


            ,
                $logWrite["new connection"];
                stream = message["Data"]//StringToStream;

                header = ReadLine[stream];

                $session = StringCases[header, RegularExpression["([A-Z]+) /(.*) "] :> {"request" -> "$1", "url" -> URLParse["$2"]}] // First // Association;

            

                (*prepare stuff*)

                $logWrite["http " <> $session["request"]];
           

                $session = 
                    Join[$session, 
                     Join @@ ((StringCases[#, 
                            RegularExpression["(\\S*): (.*)"] :> <|"$1" -> "$2"|>] // 
                            First) & /@ (StringSplit[ReadString[stream, "\r\n\r\n"], 
                           "\r\n"]))];

                If[$session["request"]=="GET", 

                    $connection[message["SourceSocket"]] = <|"session"->$session|>;
                    HTTP[message["SourceSocket"]]
                ];

                If[$session["request"]=="POST",

                    If[StringContainsQ[$session["Content-Type"],"multipart/form-data"],
                        (*files and etc*)
                        (*multipart apriori*)
                        boundary=StringCases[$session["Content-Type"],RegularExpression["(.*); boundary=(.*)"] :> <|"type" -> "$1", "boundary" -> "$2"|>][[1]]["boundary"];
                        ("boundary: "<>boundary)//$logWrite;

                        body = ReadString[stream];

                        $connection[message["SourceSocket"]] = <|"session"->$session, "boundary"->boundary, "buffer"->StringToByteArray@body, "unfinished"->True, 
                        "condition"->Hold[
                            ToExpression[$connection[message["SourceSocket"]]["session"]["Content-Length"]]==Length[$connection[message["SourceSocket"]]["buffer"]]-4
                        ], 
                        "promise"-> {
                                DecodeMultipart, 
                                HTTP
                            }
                        |>;

                        Close[stream];
                        Return[];

                    ,

                        (*for the forms*)
                        body = StringDrop[ReadString[stream], 4];

                        $logWrite["length received: "<>ToString[body//StringLength]];
                        $logWrite["length expected: "<>$session["Content-Length"]];

                        $logWrite["bodyform"];
                        $logWrite[body];
                        "-------------"//$logWrite;

                        (*if content length mismatch. may be it will be in the next responce*)
                        If[StringLength@body != ToExpression[$session["Content-Length"]],
                            $logWrite["unfinished transaction... "];
                            $connection[message["SourceSocket"]] = <|"session"->$session, "buffer"->StringToByteArray[body], "unfinished"->True, 
                            "condition"->Hold[
                                ToExpression[$connection[message["SourceSocket"]]["session"]["Content-Length"]]==Length[$connection[message ["SourceSocket"]]["buffer"]]
                            ], 
                            "promise"-> {
                                    DecodeMultipartString, 
                                    HTTP
                                }
                            |>;

                            Close[stream];
                            Return[];
                        ];                        

                        $session["data"] = URLParse["?" <> body]["Query"]//Association;
                        $logWrite[$session["data"]//ToString];

                        Close[stream];

                        $connection[message["SourceSocket"]] = <|"session"->$session|>;
                        HTTP[message["SourceSocket"]]
                    ];
                ];
            ];            
        ];

        DecodeMultipartString[client_]:=Module[{buf},
            $logWrite["decoding multipart string..."];
            (*"connnection"//$logWrite;
            $connection//Compress//$logWrite;
            "client"//$logWrite;
            client//Compress//$logWrite;*)
            buf = $connection[client]["buffer"];
            If[!ByteArrayQ[buf], buf = ByteArray[buf]];
            

            $connection[client]["session"]["data"] = URLParse["?" <> ByteArrayToString@(buf)]["Query"]//Association;
            $logWrite["decoded"];
            $connection[client]["session"]["data"]//Compress//$logWrite;
        ];

        DecodeMultipart[client_]:=Module[
            {
                stream = $connection[client]["buffer"]// ByteArrayToString // StringToStream, 
                bondary =  $connection[client]["boundary"], info, sub, string
            },

            

            $logWrite["decoding multipart..."];
            $connection[client]["session"]["data"] = <||>;

            ReadString[stream, "--" <> bondary]
            While[TrueQ[(string = 
                ReadString[stream, 
                "--" <> bondary]) != 
                "--\r\n"] && ! TrueQ[string == EndOfFile],
 
              
                sub = string // StringToStream;
                info = Join @@ 
                    StringCases[ReadLine[sub], 
                        RegularExpression["((\\S*)=\"(\\S+)\")"] :> <|"$2" -> "$3"|>];
 
      
 
                If[KeyExistsQ[info, "filename"],
                    ReadLine[sub];
  
                    buf = Read[sub, Byte];
                    Read[sub, Character]; Read[sub, Character]; Read[sub, Character]; 
                    Read[sub, Character];
  
                    info = Append[info, <|"value" -> Drop[ReadByteArray[sub], -2]|>];
  
                ,
  
                    Read[sub, Character]; Read[sub, Character]; Read[sub, Character]; 
                    Read[sub, Character];
  
                    info = Append[info, <|"value" -> StringDrop[ReadString[sub], -2]|>];
  
  
  
                ];

                If[KeyExistsQ[$connection[client]["session"]["data"], info["name"]],
                    If[ArrayQ[$connection[client]["session"]["data"][info["name"]]],
                        $connection[client]["session"]["data"][info["name"]] = Append[$connection[client]["session"]["data"][info["name"]], info];
                    ,
                        $connection[client]["session"]["data"][info["name"]] = {$connection[client]["session"]["data"][info["name"]], info};
                    ];
                    
                ,
                    $connection[client]["session"]["data"][info["name"]] = info;
                ];
                
            ];

            Close[stream];
            $logWrite["success!"];
           
               
        
        ];

        WebSocketReceive[message_]:=Module[
            {
                client = message["SourceSocket"],
                key,response, data
            },

            data = If[#[[2]] - 128 <= 125,
                <|"length" -> #[[2]] - 128,  "key" -> #[[{3, 4, 5, 6}]], "payload" -> Drop[#, 6]|>,
                
                <|"length" -> #[[4]] + BitShiftLeft[#[[3]], 8],  "key" -> #[[{5, 6, 7, 8}]], "payload" -> Drop[#, 6]|>
    
            ] &@message["DataBytes"];
            
            data = MapIndexed[BitXor[#1, data["key"][[Mod[#2[[1]] - 1, 4] + 1]]] &, data["payload"]] // ByteArray // ByteArrayToString;
            $logWrite["websockets recieved: "<>data];

            ToExpression[data];

        ];

        WebSocketConnect[message_]:=Module[
            {
                client = message["SourceSocket"],
                key,response
            },
            
            (*handshake*)
            $logWrite["websockets handshake"];
            key = 
                StringDrop[
                    StringDrop[
                        StringCases[message["Data"], 
                            RegularExpression["Sec-WebSocket-Key:(.*)"]] // First, 19], -1];
            key = Hash[StringJoin[key, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"], "SHA", "Base64Encoding"];
            response = StringJoin @@ {  "HTTP/1.1 101 Switching Protocols\r\n",
                                        "Upgrade: websocket\r\n",
                                        "Connection: Upgrade\r\n",
                                        "Sec-WebSocket-Accept: " <> key <> "\n\n"};
            WriteString[client, response];
            AppendTo[$clients, client];     

        ];

        HTTP[client_]:=Module[
            {
                get, ext, type, data, length, part, chunklength, nchunks, size,
                contenttype, postdata, header
            },

            $session = $connection[client]["session"];
            

            $logWrite["making responce..."];

            get = StringRiffle[$session["url"]["Path"], "/"]; 


            $logWrite["need: "<>get];


            (*default index.wsp*)
            If[StringLength[get] == 0, get = "index.wsp"];

            (*mime type*)
            ext = FileExtension[get]; ext// $logWrite;
            type = Interpreter["MIMETypeString"][ext]; type// $logWrite;
            If[FailureQ[type], 
                type = "text/" <> ext;
                type = StringReplace[type, {"js" -> "javascript", "wsp" -> "html"}]; 
            ];

            (*check routes*)
            (*not implemented*)
            
            If[StringLength[path]==0, fullpath = get, fullpath = path<>"/"<>get];
            fullpath // $logWrite;
            (*check files*)
            If[FileExistsQ[fullpath] != True, $logWrite["404"]; WriteString[client,"HTTP/1.1 404 Not found\r\n"]; Close[client],


            (*need a stream like... solution*)
            $logWrite["extension: "<>ext];

            If[ext == "wsp" && !skipWSP, 
                (*enabled WSP machine*)
                $logWrite[fullpath];
                
                (*virtual env*)
                Block[{},
                    data = StringJoin[
                        Map[
                            If[#[[1]] == "HTML", 
                                #[[2]], 

                                (ToExpression[#[[2]]] /. {Null -> ""})
                            ] &, 
                            WSP[OpenRead[fullpath]]]]
                ]
                ,

                (*load files by default*)
                data = Import[fullpath, "String"];                
            ];

            (*chunked, because of a bug in Mathematica 13*)
            WriteString[client, 
                "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: \
                keep-alive\r\nContent-Type: " <> type <> "\n\n"];
            
            length = StringLength[data];
      	    chunklength = 256;
      	    nchunks = Ceiling[length/chunklength];
      
            Do[
       	        end = 1 + i chunklength + chunklength - 1;
       	        If[end > length, end = length];
       	        part = StringTake[data, {1 + i chunklength, end}];
       	        size = IntegerString[StringLength[part], 16] // ToUpperCase;
       	        WriteString[client, size <> "\r\n" <> part <> "\r\n"];
            , {i, 0, nchunks - 1}];

            WriteString[client, "0" <> "\r\n" <> "\r\n"];
            
            (*remove everything*)
            (*TODO FIX ME IT IS NOT A SESSIONS*)
            $connection[client] = .;

            ];
        ]; 

        LoadPage[p_]:=StringJoin[
                        Map[
                            If[#[[1]] == "HTML", 
                                #[[2]], 
                                ToExpression[#[[2]]] /. {Null -> ""}
                            ] &, 
                            WSP[OpenRead[path<>"/"<>p]]]];

        
        StringFix[str_]:=StringReplace[str,Uncompress["1:eJxTTMoPSmNiYGAoZgESQaU5qcGMQIYSmFQHAFYsBK0="]];

        StringUnfix[str_]:=StringReplace[str,Uncompress["1:eJxTTMoPSmNiYGAoZgESQaU5qcGMQIY6mFQCAFZKBK0="]];

        constructReply[data_] := Module[{json,payloadLength,jsonByteLength,buffer,lengthByteCount},
            json = data // StringToByteArray // Normal;
            jsonByteLength = data // StringLength;
            lengthByteCount = If[jsonByteLength < 126, 0, 2];
            payloadLength = If[lengthByteCount == 0, jsonByteLength, 126];
            buffer = Table[0, {i, 1, (2 + lengthByteCount)}];
            buffer[[1]] = FromDigits[{1, 0, 0, 0, 0, 0, 0, 1}, 2];
            buffer[[2]] = payloadLength;
  
          
            If[lengthByteCount > 0, 
                buffer[[3]] = BitAnd[(BitShiftRight[jsonByteLength, 8]), 255];
                buffer[[4]] = BitAnd[(jsonByteLength), 255];
            ];
  
            Join[buffer, json] // ByteArray
        ];

        WebDynamic[exp_, sym_, link_ : Automatic] := Module[{hash},
            If[! KeyExistsQ[$tracked, sym], 
                $tracked[sym] = <|"prev" -> ReleaseHold[sym], "expressions" -> {}|>;
            ];
            If[TrueQ[link == Automatic],
                hash = Hash[exp, "SHA", "Base64Encoding"];
                $tracked[sym]["expressions"] = 
                Append[$tracked[sym]["expressions"], <|"exp" -> exp, "id" -> hash,
                    "property" -> "innerHTML", "client"->$session["client"]|>];

                Return[
                    "<div class=\"dynamic\" id=\"" <> hash <> "\">" <> 
                    ToString[ReleaseHold[exp]] <> "</div>"
                ];
            ,
                
                $tracked[sym]["expressions"] = 
                Append[$tracked[sym]["expressions"], Join[<|"exp" -> exp|>, link, <|"client"->$session["client"]|>]];
                Return[ToString[ReleaseHold[exp]]];
                
            ];
        ];
       

        WebSlider[sym_, range_:{0,1}] := Module[{hash},
            If[!NumberQ[ReleaseHold[sym]], ToExpression[ToString[ReleaseHold[sym]]<>"="<>ToString[range[[1]]]]];

            If[! KeyExistsQ[$tracked, sym], 
                $tracked[sym] = <|"prev" -> ReleaseHold[sym], "expressions" -> {}|>;
            ];
            hash = Hash[sym, "SHA", "Base64Encoding"];

            

            $tracked[sym]["expressions"] = 
                Append[$tracked[sym]["expressions"], Join[<|"exp" -> sym|>, 
                <|"id"->hash, "property"->"value"|>,
                <|"client"->$session["client"]|>
                ]]; 

            "<input type=\"range\" min=\""<>ToString[range[[1]]]<>"\" max=\""<>ToString[range[[2]]]<>"\" value=\""<>ToString[ReleaseHold[sym]]<>"\" id=\""<>hash<>"\" oninput=\"eventcall('"<>ToString[sym]<>"', this.value)\">"
                       

        ];   

        WebDateObject[date_, i_,p_,f_] := Module[{},
            "<div class=\"date-obj\">"<>ToString[date[[2]]]<>"."<>ToString[date[[3]]]<>"."<>ToString[date[[1]]]<>" "<>ToString[date[[4]]]<>":"<>ToString[date[[5]]]<>"</div>"
        ];     
 

        <|"clients"->$clients,"socket"->SocketListen[addr, handler]|>
    ]],

    "loop":>Module[{node},
        Do[
            If[ReleaseHold@i == $tracked[i]["prev"], Continue[]];
            Do[
                $res = node;
                $res["exp"] = ReleaseHold[$res["exp"]] //ToString;
   
                BinaryWrite[#, 
                    constructReply[
                    ExportString[$res, "ExpressionJSON"] // ToString]] & /@ $clients;
   
            , {node, $tracked[i]["expressions"]}];
            $tracked[i]["prev"] = ReleaseHold@i ;
        
        , {i, $tracked // Keys}]
    ],


    "stop"->Function[lst, With[{},
        Close[lst["socket"]["Socket"]];
        DeleteObject[lst["socket"]];
        Close/@(lst["clients"]);
        $tracked = <||>;
    ]]
|>

