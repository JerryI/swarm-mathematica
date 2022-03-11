(* ::Web:: *)
logWrite["webserver:: init"];

$logWrite = logWrite;

webserver = Get["webserver/tinyweb.wls"];
wsp = Get["webserver/wsp.wls"];

wsp["install"];
webserver["start"]["83.220.175.48:81", "public"];