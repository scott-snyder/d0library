      SUBROUTINE DCHKZ_EDGE(LAYER,ZPOS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if the Z positions are belong to the
C-                         "edge region" of the CDC chamber. Using vertex
C-                         information to determine the "edge region",
C-                         which means the tracks in that region may go 
C-                         out of the CDC from the endcap
C-
C-   Inputs  : LAYER: layer number
C-             ZPOS: array contains two Z information from the delay lines
C-   Outputs : OK: true if the ZPOS belong to this "edge region"
C-
C-   Created  24-SEP-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  LAYER, GZDGEH, LDRFT, GZDRFT, LVERH, GZVERH, LVERT
      INTEGER  RUN, RUNSAV, ID, IDSAV, I
      REAL     ZPOS(2), ZHALF, ZVERT, ZVERTE, ZLIMIT(2)
      REAL     RPOS(2), RCENT, ROUT
      LOGICAL  OK, GOOD(2), FIRST
      SAVE FIRST, RUNSAV, IDSAV
      DATA FIRST/.TRUE./
      DATA RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
C
      OK = .FALSE.
      IF (ZPOS(1) .GE. 999.0 .AND. ZPOS(2) .GE. 999.0) GOTO 999
      IF (ZPOS(1) .LT. 999.0 .AND. ZPOS(2) .LT. 999.0 
     &  .AND. (ZPOS(1) * ZPOS(2)) .LT. 0.) GOTO 999
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IF (LDGEH .LE. 0) LDGEH = GZDGEH()
        IF (LDGEH .LE. 0) GOTO 999
        ZHALF = C(LDGEH + 19)
        ROUT = C(LDGEH + 11)
      ENDIF
      IF (LDGEH .LE. 0) GOTO 999
      CALL VZERO(ZLIMIT,2)
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
        LVERH = GZVERH()
        IF (LVERH .GT. 0) THEN
          LVERT = LQ(LVERH - 1)
          IF (LVERT .GT. 0) THEN
            ZVERT = Q(LVERT + 5)
            ZVERTE = ABS(Q(LVERT + 8))
          ELSE
            ZVERT = 0.0
            ZVERTE = 1.4
          ENDIF
        ELSE
          ZVERT = 0.0
          ZVERTE = 1.4
        ENDIF
        LDRFT = GZDRFT()
        IF (LDRFT .LE. 0) GOTO 999
      ENDIF
      RCENT = C(LDRFT + 11 + LAYER * 2) 
      RPOS(1) = RCENT + C(LDRFT + 19)
      RPOS(2) = RCENT + C(LDRFT + 25)
      DO 100 I = 1, 2
        GOOD(I) = .TRUE.
        IF (ZPOS(I) .GE. 999.0) GOTO 100
        IF (ZPOS(I) .GE. 0.) THEN
          ZLIMIT(I) = RPOS(I) * (ZHALF - ZVERT) / ROUT + ZVERT
          IF (ZPOS(I) .LT. ZLIMIT(I)-ZVERTE) THEN
            GOOD(I) = .FALSE.
            GOTO 101
          ENDIF
        ELSE
          ZLIMIT(I) = -(RPOS(I) * (ZHALF + ZVERT) / ROUT - ZVERT)
          IF (ZPOS(I) .GT. ZLIMIT(I)+ZVERTE) THEN
            GOOD(I) = .FALSE.
            GOTO 101
          ENDIF
        ENDIF
  100 CONTINUE
C
C  set OK to be TRUE when:
C          - both Z positions are in the "edge region", or
C          - one Z is missing, but another one is in the "edge region"
C
  101 OK = GOOD(1) .AND. GOOD(2)
C
  999 RETURN
      END
