      SUBROUTINE ENLOG (LOG,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ENLOG' in SRCP bank. This
C-                         routine can be used to pass ZEBRA LOG level 
C-                         codes from one program module to another.
C-
C-   Inputs  : LOG         ZEBRA LOG level code
C-                         if SWITCH =-1
C-
C-   Outputs : LOG         ZEBRA LOG level code
C-                         if SWITCH = 1
C-
C-   Controls: SWITCH      1 ---- Get stored number
C-                        -1 ---- Set number
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LOG
      INTEGER SWITCH
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
      CALL EZGSET ('ENLOG',LOG,SWITCH)
      CALL EZRSET
  999 RETURN
      END
