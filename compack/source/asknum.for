      SUBROUTINE ASKNUM (PRT,NUMS,NUMBER,TYPE,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for list of numbers
C-
C-   Inputs:   PRT         Prompt
C-
C-   Outputs : NUMS        Number of values returned
C-             NUMBER(NN)  Array of values (returned as REAL)
C-             TYPE(NN)    Array giving number type:
C-                            1 INTEGER
C-                            2 REAL
C-                            3 REAL in exponential format
C-                           -1 INVALID number representation
C-             BACK        If true go back to upper menu level
C-
C-   Created  14-MAR-1988   Harrison B. Prosper
C-   Modified  9-MAY-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       OK,BACK
      INTEGER       NUMS
      REAL          NUMBER(*)
      INTEGER       TYPE(*)
      CHARACTER*(*) PRT
      INTEGER       NP
      CHARACTER*64  STRING
C----------------------------------------------------------------------
      NUMS = 0
      NP = LEN (PRT)
      CALL GETSTR (PRT(1:NP),STRING,OK,BACK)
      IF ( BACK ) GOTO 999
      IF ( OK  ) THEN
        CALL VALUES (STRING,NUMBER,TYPE,NUMS)
      ENDIF
  999 RETURN
      END
