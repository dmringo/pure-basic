10 goto 40
15 let modulus = 1

20 wend      : rem while loop exits here when modulus = 0
30 print "line 30": end

40 let i = 10

50 while i < 0 : rem always false
60 print "in while, i= "; i : i = i - 1
70 if (i mod 2) = modulus then 20 

80 rem wend      : rem while loop exits here when modulus = 1
81             rem if we remove 80, what happens?

90 print "line 90": end
