      SUBROUTINE DTRK_IN_VERT(TOTTRK,NTRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine the number of tracks that have been
C-   considered in the VERT fitting
C-
C-   Inputs  : none
C-   Outputs : TOTTRK = Total number of tracks
C-             NTRACK = Number of tracks in the VERT fitting region.
C-  TOTTRK-NTRACK yields the number of tracks not considered because the impact
C-  parameter in rz lied outside the fitting region.
C-
C-   Controls: 
C-
C-   Created  26-JUL-1995   Srini Rajagopalan
C-   Updated  13-SEP-1995   Srini Rajagopalan  Fix problem when LDTRH=0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDTRK.LINK'
C
      INTEGER IER
      INTEGER LDTRH,GZDTRH,LDTRK
      INTEGER TOTTRK,NTRACK
C
      REAL LEAD,TAIL
      PARAMETER (LEAD = -100.)
      PARAMETER (TAIL = 100.)
C
      REAL BEAM_POS(3),BXYLMT
      REAL PHI,TANPHI,THETA
      REAL X0,Y0,BEAMX,BEAMY
      REAL R0,ZPOSIT,IMPACT
C
      LOGICAL EZERROR
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('vertex_fix','DTRK_IN_VERT',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('BXYLMT',BXYLMT,IER)
        CALL EZGET_rarr('BEAM_POS',BEAM_POS,IER)
        CALL EZRSET
      ENDIF
C
      BEAMX = BEAM_POS(1)
      BEAMY = BEAM_POS(2)
C
      TOTTRK = 0
      NTRACK = 0
      LDTRH = GZDTRH()
      IF (LDTRH.LE.0) GO TO 999
      LDTRK = LQ(LDTRH - IZDTRK)
C
      DO WHILE (LDTRK.GT.0)
        PHI = Q(LDTRK + 6)
        TANPHI = TAN(PHI)
        THETA = Q(LDTRK+9)
        IF (THETA.EQ.0) GO TO 100
C
        X0 = Q(LDTRK + 7)
        Y0 = Q(LDTRK + 8)
        R0 = SQRT((X0-BEAMX)**2 + (Y0-BEAMY)**2)
        IMPACT = ABS(TANPHI * BEAMX - BEAMY + Y0 - TANPHI * X0)
     &           / SQRT(1 + TANPHI**2)
        IF (IMPACT .GT. BXYLMT) GO TO 100
        TOTTRK = TOTTRK + 1
C
        ZPOSIT = Q(LDTRK+11) - R0 / TAN(THETA)        
        IF (ZPOSIT .GE. LEAD .AND. ZPOSIT .LE. TAIL) NTRACK = NTRACK + 1
  100   CONTINUE
        LDTRK = LQ(LDTRK)
      ENDDO
C
  999 RETURN
      END
