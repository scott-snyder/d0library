      SUBROUTINE DCHKT_EDGE(PARFIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check if the Track goes out of CDC from the 
C-                         edge of the CDC (side of the CDC)
C-
C-   Inputs  : PARFIT: fitted parameters for a track (see CDXYZL for
C-                     more details)
C-   Outputs : OK: true if the track goes out of CDC from the side
C-
C-   Created  28-OCT-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  GZDGEH
      REAL     PARFIT(10), ZHALF, ROUT, R0, RTRACK
      LOGICAL  OK, FIRST
      SAVE  FIRST
      DATA  FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK = .FALSE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF (LDGEH .LE. 0) LDGEH = GZDGEH()
        IF (LDGEH .LE. 0) GOTO 999
        ZHALF = C(LDGEH + 19)
        ROUT = C(LDGEH + 11)
      ENDIF
C
      IF (PARFIT(4) .EQ. 0.0) GOTO 999
      IF (PARFIT(3) .GT. 0 .AND. PARFIT(4) .LT. HALFPI) THEN
        R0 = SQRT(PARFIT(1)**2 + PARFIT(2)**2)
        RTRACK = R0 + TAN(PARFIT(4)) * (ZHALF - PARFIT(3))
        IF (ABS(RTRACK) .LE. ROUT) THEN
          OK = .TRUE.
        ENDIF
      ELSE
        IF (PARFIT(3) .LT. 0 .AND. PARFIT(4) .GT. HALFPI) THEN
          R0 = SQRT(PARFIT(1)**2 + PARFIT(2)**2)
          RTRACK = R0 - TAN(PARFIT(4)) * (ZHALF + PARFIT(3)) 
          IF (ABS(RTRACK) .LE. ROUT) THEN
            OK = .TRUE.
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
