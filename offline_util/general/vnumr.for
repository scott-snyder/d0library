      SUBROUTINE VNUMR (NA,NUMBER,LEFT,D,RIGHT,STRING,NS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a string which is a vector of numbers
C-                         bounded by strings LEFT and RIGHT and
C-                         delimited with string D.
C-
C-   Inputs:   NA          [I]  Number of Reals
C-             NUMBER      [R*] Array of Reals
C-             LEFT        [C*] Left boundary symbol
C-             D           [C*] Delimeter symbol
C-             RIGHT       [C*] Right boundary symbol
C-
C-   Outputs : STRING      String of numbers
C-             NS          Length of string
C-
C-   Created  10-MAR-1988   Harrison B. Prosper
C-   Updated  11-SEP-1989   Harrison B. Prosper
C-   swapped position of LEFT and RIGHT arguments!
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL          NUMBER(*),X,ABS
      CHARACTER*(*) LEFT,D,RIGHT,STRING
      INTEGER       I,J,K,N,NN,NA,NL,ND,NR,NS,NSMAX
      CHARACTER*16  NUMSTR,FORM
C----------------------------------------------------------------------
C
      NL = LEN (LEFT)
      ND = LEN (D)
      NR = LEN (RIGHT)
      NSMAX = LEN (STRING)
C
C ****  Create start of string
C
C ****  Determine a reasonable format
      X = ABS(NUMBER(1))
      IF ( (X .GE. 1.0) .AND. (X .LT. 1000.0) ) THEN
        FORM = '(F10.1)'
      ELSEIF ( (X .GE. 0.01) .AND. (X .LT. 1.0) ) THEN
        FORM = '(F10.3)'
      ELSEIF ( X .EQ. 0.0 ) THEN
        FORM = '(F3.1)'
      ELSE
        FORM = '(1PE10.2)'
      ENDIF
C
      WRITE(UNIT=NUMSTR,FMT=FORM) NUMBER(1)
      CALL WORD (NUMSTR,I,J,N)
      STRING = LEFT(1:NL)//NUMSTR(I:J)
C **** Current string length
      NS = NL+N
C
C ****  Create main body of string
C
      IF ( NA .GT. 1 ) THEN
        DO 10 K = 2, NA
C
C ****  Determine a reasonable format
          X = ABS(NUMBER(K))
          IF ( (X .GE. 1.0) .AND. (X .LT. 1000.0) ) THEN
            FORM = '(F10.1)'
          ELSEIF ( (X .GE. 0.01) .AND. (X .LT. 1.0) ) THEN
            FORM = '(F10.3)'
          ELSEIF ( X .EQ. 0.0 ) THEN
            FORM = '(F3.1)'
          ELSE
            FORM = '(1PE10.2)'
          ENDIF
C
          WRITE(UNIT=NUMSTR,FMT=FORM) NUMBER(K)
          CALL WORD (NUMSTR,I,J,N)
          NN = NS+ND+N
          IF ( NN .LE. NSMAX ) THEN
            STRING = STRING(1:NS)//D(1:ND)//NUMSTR(I:J)
            NS = NN
          ELSE
            RETURN
          ENDIF
   10   CONTINUE
      ENDIF
C
C ****  Create end of string
C
      NN = NS+NR
      IF ( NN .LE. NSMAX ) THEN
        STRING = STRING(1:NS)//RIGHT(1:NR)
        NS = NN
      ENDIF
C
  999 RETURN
      END
