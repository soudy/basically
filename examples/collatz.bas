10 REM COLLATZ CONJECTURE
20 N = 12
30 PRINT N
35 IF N = 1 THEN END
40 IF INT(N / 2) * 2 = N THEN N = N/2 : GOTO 30
50 N = 3*N+1 : GOTO 30
