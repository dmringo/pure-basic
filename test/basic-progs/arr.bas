10 N = 10
15 DIM A(N)
20 FOR I=1 to N
30 PRINT A(I);
40 A(I) = I / 2
50 PRINT " --> "; A(I)
60 NEXT I
70 ON N / 10 GOTO 80, 90
80 N = 20: GOTO 15
90 END

  
