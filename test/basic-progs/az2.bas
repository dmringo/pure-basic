10 PRINT TAB(28);"AMAZING PROGRAM"
20 PRINT TAB(15);"CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
30 PRINT:PRINT:PRINT:PRINT
100 INPUT "WHAT ARE YOUR WIDTH AND LENGTH";H,V
102 IF H<>1 AND V<>1 THEN 110
104 PRINT "MEANINGLESS DIMENSIONS.  TRY AGAIN.":GOTO 100
110 DIM W(H,V),V(H,V)
130 PRINT
120 PRINT
140 PRINT
150 PRINT
160 Q=0:Z=0:X=INT(RND(1)*H+1)
165 FOR I=1 TO H
170 IF I=X THEN 173
171 PRINT ".--";:GOTO 180
173 PRINT ".  ";
180 NEXT I
190 PRINT "."
195 C=1:W(X,1)=C:C=C+1
200 R=X:S=1:GOTO 260