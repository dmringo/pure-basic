10 print "Start test"
20 gosub 50
30 print "line 30, i = "; i : next
40 end
50 for i = 0 to 10
60 print "i is "; i
70 if i > 5 then return
80 next i  
  