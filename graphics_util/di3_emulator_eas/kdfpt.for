      SUBROUTINE KDFPT(PATNUM, PATNAM)
      IMPLICIT NONE
      INTEGER LS(6), PATNUM
      REAL LLEN, DEFACT
      LOGICAL CONTI, MATCH
      CHARACTER*4 PATNAM
      CHARACTER*3 STR
      INTEGER NPT
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      EXTERNAL ERRHAN
C
C    Linestyle initialization.
C
      GOTO (1, 2, 3, 4, 5, 6, 7, 8, 9), PATNUM
    1 CONTINUE
      LS(1) = 1
      LS(2) = 3
      NPT   = 2
      LLEN  = 0.04
      GOTO 99000
    2 CONTINUE
      LS(1) = 1
      LS(2) = 1
      NPT   = 2
      LLEN  = 0.10
      GOTO 99000
    3 CONTINUE
      LS(1) = 6
      LS(2) = 2
      LS(3) = 1
      LS(4) = 2
      NPT   = 4
      LLEN  = 0.10
      GOTO 99000
    4 CONTINUE
      LS(1) = 6
      LS(2) = 2
      LS(3) = 3
      LS(4) = 2
      NPT   = 4
      LLEN  = 0.10
      GOTO 99000
    5 CONTINUE
      LS(1) = 6
      LS(2) = 1
      LS(3) = 2
      LS(4) = 1
      LS(5) = 2
      LS(6) = 1
      NPT   = 6
      LLEN  = 0.10
      GOTO 99000
    6 CONTINUE
      LS(1) = 1
      LS(2) = 1
      NPT   = 2
      LLEN  = 0.20
      GOTO 99000
    7 CONTINUE
      LS(1) = 6
      LS(2) = 2
      LS(3) = 1
      LS(4) = 2
      NPT   = 4
      LLEN  = 0.20
      GOTO 99000
    8 CONTINUE
      LS(1) = 6
      LS(2) = 2
      LS(3) = 3
      LS(4) = 2
      NPT   = 4
      LLEN  = 0.20
      GOTO 99000
    9 CONTINUE
      LS(1) = 6
      LS(2) = 1
      LS(3) = 2
      LS(4) = 1
      LS(5) = 2
      LS(6) = 1
      NPT   = 6
      LLEN  = 0.20
      GOTO 99000
99000 CONTINUE
      DEFACT = MAX(UWIND(2)-UWIND(1),UWIND(4)-UWIND(3)) * 0.5
      CONTI  = .FALSE.
      MATCH  = .FALSE.
      LLEN   = LLEN * DEFACT
      KPTRN  = KPTRN + 1
      CALL KBLDN(KPTRN, STR)
      PATNAM = 'P'//STR
      CALL PDEFPA(PATNAM,NPT,LS,CONTI,MATCH,LLEN,ERRHAN)

      RETURN
      END
