      SUBROUTINE J1IGET(CODE, VALUE)
C
C    Purpose:
CD   This module returns an integer value based on the code requested.
CD   The parameter passed in (CODE) is an integer with a value of 1..46.
CD   This parameter determines what integer value to return through 
CD   the parameter VALUE.
C
CH    A. Virgo
CH    S. Abachi  completed and modified
C
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, LINATT-R, TEXATT-R
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
      INTEGER CODE, VALUE
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (CODE .LT. 1 .OR. CODE .GT. 46) THEN
         CALL ERROR('J1IGET: CODE NOT IN RANGE (1..46)')
      ENDIF
      VALUE = 0
      GOTO ( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     +      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     +      21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     +      31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     +      41, 42, 43, 44, 45, 46), CODE

    1 CONTINUE
      VALUE = SEGNUM
      IF(.NOT. SEGOPN) VALUE = -1
      GOTO 9900
    2 CONTINUE
      GOTO 9900
    3 CONTINUE
      GOTO 9900
    4 CONTINUE
      GOTO 9900
    5 CONTINUE
      GOTO 9900
    6 CONTINUE
      GOTO 9900
    7 CONTINUE
      GOTO 9900
    8 CONTINUE
      GOTO 9900
    9 CONTINUE
      GOTO 9900
   10 CONTINUE
      GOTO 9900
   11 CONTINUE
      GOTO 9900
   12 CONTINUE
      GOTO 9900
   13 CONTINUE
      GOTO 9900
   14 CONTINUE
      GOTO 9900
   15 CONTINUE
      GOTO 9900
   16 CONTINUE
      GOTO 9900
   17 CONTINUE
      GOTO 9900
   18 CONTINUE
      GOTO 9900
   19 CONTINUE
      GOTO 9900
   20 CONTINUE
      GOTO 9900
   21 CONTINUE
      GOTO 9900
   22 CONTINUE
      GOTO 9900
   23 CONTINUE
      GOTO 9900
   24 CONTINUE
      GOTO 9900
   25 CONTINUE
      GOTO 9900
   26 CONTINUE
      GOTO 9900
   27 CONTINUE
      GOTO 9900
   28 CONTINUE
      GOTO 9900
   29 CONTINUE
      GOTO 9900
   30 CONTINUE
      GOTO 9900
   31 CONTINUE
      GOTO 9900
   32 CONTINUE
      GOTO 9900
   33 CONTINUE
      GOTO 9900
   34 CONTINUE
      GOTO 9900
   35 CONTINUE
      GOTO 9900
   36 CONTINUE
      GOTO 9900
   37 CONTINUE
      GOTO 9900
   38 CONTINUE
      GOTO 9900
   39 CONTINUE
      GOTO 9900
   40 CONTINUE
      GOTO 9900
   41 CONTINUE
      GOTO 9900
   42 CONTINUE
      GOTO 9900
   43 CONTINUE
      VALUE = 0
      IF(RIGHT .LT. 0.0) VALUE = 1
      GOTO 9900
   44 CONTINUE
      VALUE = PRJTYP
      IF(.NOT. THREED) VALUE = 0
      GOTO 9900
   45 CONTINUE
      VALUE = 0
      IF(MODEL) VALUE = 1
      GOTO 9900
   46 CONTINUE
      VALUE = 0
      IF(BATCH) VALUE = 1
      GOTO 9900
C
C  Exit
C
 9900 CONTINUE
      RETURN
      END
