      SUBROUTINE VNUMI (NA,NUMBER,LEFT,D,RIGHT,STRING,NS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a string which is a vector of numbers
C-                         bounded by strings LEFT and RIGHT and
C-                         delimited with string D.
C-
C-   Inputs:   NA          [I]  Number of Integers
C-             NUMBER      [I*] Array of integers
C-             LEFT        [C*] Left boundary symbol
C-             D           [C*] Delimeter symbol
C-             RIGHT       [C*] Right boundary symbol
C-
C-   Outputs : STRING      String of numbers
C-
C-   Created  10-MAR-1988   Harrison B. Prosper
C-   Updated  11-SEP-1989   Harrison B. Prosper   
C-   Swapped positions of LEFT and RIGHT!
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       NUMBER(*)
      CHARACTER*(*) LEFT,D,RIGHT,STRING
      INTEGER       I,J,K,N,NN,NA,NR,ND,NL,NS,NSMAX
      CHARACTER*16  NUMSTR,FORM
C----------------------------------------------------------------------
C
      NL = LEN (LEFT)
      ND = LEN (D)
      NR = LEN (RIGHT)
      NSMAX = LEN (STRING)
      FORM = '(I10)'
C
C ****  Create start of string
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
