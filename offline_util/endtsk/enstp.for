      SUBROUTINE ENSTP (BANK,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access variable 'ENSTP' in SRCP bank.
C-
C-   Inputs  : BANK        Name of bank (usually STPO, STPC or STPN)
C-                         if SWITCH =-1
C-
C-   Outputs : BANK        Name of bank (usually STPO, STPC or STPN)
C-                         if SWITCH = 1
C-
C-   Controls: SWITCH      1 ---- Get bank name
C-                        -1 ---- Store bank name
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 BANK
      INTEGER     SWITCH
      INTEGER     N
C----------------------------------------------------------------------
C
      CALL EZPICK ('SCPH')
C
      IF ( SWITCH .EQ. 1 ) THEN
        CALL EZGSET ('ENSTP',N,1)
        CALL UHTOC (N,4,BANK,4)
      ELSE
        CALL UCTOH (BANK,N,4,4)
        CALL EZGSET ('ENSTP',N,-1)
      ENDIF
C
      CALL EZRSET
C
  999 RETURN
      END
