(* ::Web:: *)
logWrite["webserver:: init"];

$logWrite = logWrite;

webserver = Get["webserver/tinyweb.wls"];
wsp = Get["webserver/wsp.wls"];

wsp["install"];
webserver["start"]["0.0.0.0:81", "public"];
