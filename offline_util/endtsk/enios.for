      SUBROUTINE ENIOS (IOS,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ENIOS' in SRCP bank. This
C-                         routine can be used to pass ZEBRA IOS codes
C-                         from one program module to another.
C-
C-   Inputs  : IOS         ZEBRA IOS code
C-                         if SWITCH =-1
C-
C-   Outputs : IOS         ZEBRA IOS code
C-                         if SWITCH = 1
C-
C-   Controls: SWITCH      1 ---- Get stored number
C-                        -1 ---- Set number
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOS
      INTEGER SWITCH
C----------------------------------------------------------------------
      CALL EZPICK ('SCPH')
      CALL EZGSET ('ENIOS',IOS,SWITCH)
      CALL EZRSET
  999 RETURN
      END
