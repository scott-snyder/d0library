      SUBROUTINE ZEBIOS (BANK,IOS,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ZEBRA/IOS_CODE' in SRCP bank.
C-                         This routine can be used to pass ZEBRA IOS codes
C-                         from one program module to another.
C-
C-   Inputs  : BANK        Name of SRCP bank
C-             IOS         ZEBRA IOS code
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
      CHARACTER*(*) BANK
      INTEGER IOS
      INTEGER SWITCH,L
C----------------------------------------------------------------------
      L = LEN (BANK)
      CALL SLSRCP (BANK(1:L))
      CALL GTSRCP ('ZEBRA/IOS_CODE',IOS,SWITCH)
      CALL RSSRCP
  999 RETURN
      END
