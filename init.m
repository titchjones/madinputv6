AppendTo[$Path,"c:\\madinput"]

reallyBig[x_] := If[ByteCount[x] > 100000, Short[x,10], x];

  $Post = reallyBig;
