      SUBROUTINE ENLUN (LUN,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ENLUN' in SRCP bank. This
C-                         routine can be used to pass unit numbers
C-                         from one program module to another.
C-
C-   Inputs  : LUN         Logical unit number
C-                         if SWITCH =-1
C-
C-   Outputs : LUN         Logical unit number
C-                         if SWITCH = 1
C-
C-   Controls: SWITCH      1 ---- Get stored string
C-                        -1 ---- Set stored string
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*80 STRING
      INTEGER SWITCH,LUN
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
      CALL EZGSET ('ENLUN',LUN,SWITCH)
      CALL EZRSET
  999 RETURN
      END
