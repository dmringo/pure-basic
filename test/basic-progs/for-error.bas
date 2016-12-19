10 goto 30
20 next i : print "Exited for loop, i = "; i
30 for i = 1 to 0 : rem error: no next following the un-enterable loop
40 goto 20
50 rem uncomment next line to remove error
60 rem next i
70 end  