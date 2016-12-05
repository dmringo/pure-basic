10 ? 1, 2, 3; 4 5; 6, 8
20 print 9; tab(4) 8; tab(12); "hi"
30 print "Hello"; tab(-5) "J"
40 print 1    3
500 for I = 1 to 10
600 for J = I to I + 3
700 print 
800 next J, I: REM this breaks once the I loop exits
90 goto 110
100 wend(1): print "WEND 100": goto 500
110 while
120 I = I + 1
130 print I, "WHILE.."
140 if (I > 20) then goto 100
150 wend (I > 21)