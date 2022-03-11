<|
    "package"->"wsp",
    "version"->0.1,
    "install"->With[{},
        WSP[s_, init_ : {}, ibra_ : 0, wrap_ : False] := Module[
        {Print(* =$logWrite for debug*), module, code = init, buffer = "", c = "", p = "", brackets,
            skipped = False, exp = "", bra = ibra,flag=False},
        (*extract everything before {{*)
        Print["begin"];
        (*like in C style, probably it will be slower, i dunno*)
        While[True,
            c = Read[s, "Character"];
            If[TrueQ[c == EndOfFile], Break[]];
            If[p <> c == "{{", Break[]];
            If[c == "{", skipped = True; p = c; Continue[];];
            If[skipped,
                buffer = buffer <> p <> c;
                skipped = False;,
                buffer = buffer <> c;
            ];
            p = c;
        ];
        buffer = StringTrim[buffer];
        (*pure HTML text*)
        AppendTo[code, "HTML" -> buffer];
        (*extract the WF expression*)
        skipped = False;
        p = "";
        
        While[True,
            c = Read[s, "Character"];
            If[TrueQ[c == EndOfFile], Break[]];
            If[p <> c == "}}", Break[]];
            If[c == "}", skipped = True; p = c; Continue[];];
            If[skipped,
                exp = exp <> p <> c;
                (*count all brackets to detect the embedded HTML code*)
            
                If[c == "[", bra++];
                If[c == "]", bra--];
                If[p == "[", bra++];
                If[p == "]", bra--];
                skipped = False;
            ,
                If[c == "[", bra++];
                If[c == "]", bra--];
                exp = exp <> c;
            ];
            p = c;
        ];
        exp = StringTrim[exp];
        (*detect if we have an HTML inside the expression*)
        (*basically by an extra [ or ]*)
        flag=False;

        If[bra != 0 && ibra == 0, 
            Print["module!"];
            (*parse the rest to find the last part of the expresssion we \
        parsed before*)
            module = WSP[s, {}, bra];
            Print["last exp:"];
            (*this is the last one*)
            Print[Last[module][[2]]];
            (*go to the inner branches*)
            (*principe: from outer to inner layers*)
            (*FIXME: we need the recursion, so, 
            we have so revert back all changes to the inner blocks*)
            (*omitting the last block*)
            substream = 
            StringJoin @@ 
            Map[If[#[[1]] == "HTML", #[[2]], "{{" <> #[[2]] <> "}}"] &, 
                Drop[module, -1]] // StringToStream;
            (*go deeper*)
            (*allow wrapping the output of the expression with a special \
        buffer*)
            (*$stdout = $stdout <> Evaluate[...]*)
            subcode = WSP[substream, {}, 0, True];
            Print["SUBCODE:"];
            Print[subcode];
            (*not really good solution*)
            (*warp everything into the module*)
            (*all html pieces will be treated as a string*)
            exp = 
            "Module[{$stdout=\"\"}," <> exp <> 
            StringJoin[
            Map[If[#[[1]] == "HTML", 
                "$stdout=$stdout<>\"" <> StringReplace[#[[2]],"\""->"\\\""] <> "\";",  #[[2]]] &, 
                subcode]] <> Last[module][[2]] <> "; $stdout]";
            (*if wrapping is allowed, 
            it will redirect output to the $stdout variable*)
            If[wrap && exp != "", exp = "$stdout=$stdout<>" <> exp <> ";"];
            (*FIXME: a bug, empty strings*)
            exp = StringReplace[exp, "ToString[]" -> "\"\""];
            ,
            (*forcely convert the output of the all cells to the string*)
            If[ibra == 0 && exp != "", exp = "ToString[" <> exp <> "]";];
            flag = True;
            
        ];
        Print["EXP:"];
        Print[exp];
        Print["bra is "<>bra]
        (*alternative handler for the single cells (no breaking by HTML)*)

        (*the initial number of brackets is the same as it was before*)
        
        If[bra - ibra == 0, Print["self"];
            If[wrap,
                exp = "$stdout=$stdout<>ToString[" <> exp <> "];";
            
            ];
        ];
        
        AppendTo[code, "WF" -> exp];
        (*when we have found the last expression in the broken code block*)

        (*return it immediately to the previous handler*)
        If[bra == 0 && ibra != 0, Return[code]];
        
        (*for the rest of the code lines*)
        If[TrueQ[c == EndOfFile],
            code
            , WSP[s, code, If[!flag,0, bra], wrap]]
        
        ];
    ]

|>