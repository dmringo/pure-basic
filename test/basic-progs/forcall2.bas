10 print "Start test"
20 for i = 0 to 10
30 print "i is " ; i
40 if i > 5 then gosub 100
50 next
60 print "outside FOR? attempting return" : return
100 print "on line 100, i is " ; i : next

  