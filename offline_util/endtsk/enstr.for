      SUBROUTINE ENSTR (STRING,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ENSTR' in main SRCP bank.
C-                         This routine can be used to pass strings 
C-                         from one program module to another.
C-
C-   Inputs  : STRING      80-character string
C-                         if SWITCH =-1
C-
C-   Outputs : STRING      80-character string
C-                         if SWITCH = 1
C-
C-   Controls: SWITCH      1 ---- Get stored string
C-                        -1 ---- Store string
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*80 STRING
      INTEGER      SWITCH
      INTEGER NNN(24)
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
C
      IF ( SWITCH .EQ. 1 ) THEN
        CALL EZGSET ('ENSTR',NNN(1),1)
        CALL UHTOC (NNN(1),4,STRING,80)
      ELSE
        CALL UCTOH (STRING,NNN(1),4,80)
        CALL EZGSET ('ENSTR',NNN(1),-1)
      ENDIF
C
      CALL EZRSET
C
  999 RETURN
      END
