      SUBROUTINE ZCONST_PEAK(LEAD,TAIL,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill histogram which contains Z information
C-                         from CDC tracks for vertex finding
C-
C-   Inputs  : HISID: the histogram ID
C-             LEAD: the low limit of the histogram
C-             TAIL: the high limit of the histogram
C-   Outputs : none
C-
C-   Created  21-JUL-1991   Qizhong Li-Demarteau
C-   Updated  23-NOV-1993   Srini Rajagopalan  Adapted for VFIT, similar to
C-                          zvertx_peak.
C-   Updated   3-MAR-1994   Srini Rajagopalan  Fix minor bug.
C-   Updated  12-JUL-1995   Srini Rajagopalan  Fetch BEAM_POS every new run 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LDTRH, LDTRK, GZDTRH
      INTEGER IER, HSTID, ILOW,IHI,ILEAD,ITAIL
      INTEGER I,J,JJ,K,KK,NENTRY,IPEAK(10)
      INTEGER RUN,RUNSAV,RUNNO
      REAL    LEAD, TAIL,CONTEN(100)
      REAL    X0, Y0, PHI, TANPHI, BEAMX, BEAMY, BXYLMT, IMPACT
      REAL    ZPOSIT, THETA, R0
      REAL    BEAM_POS(3), PEAK1
      LOGICAL FIRST, FLAG, CLUST1
      SAVE  FIRST
      DATA  FIRST/.TRUE./
      DATA RUNSAV /-1/
      PARAMETER (HSTID = 5999,ILOW=1,IHI=100)
C----------------------------------------------------------------------
C
      RUN = RUNNO()
      IF (RUN .NE. RUNSAV) THEN
        RUNSAV = RUN
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('BXYLMT',BXYLMT,IER)
        CALL EZGET('BEAM_POS',BEAM_POS,IER)
        CALL EZRSET
      ENDIF
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL HBOOK1(HSTID,'Z contents',100,-100.,100.,0.)
      ENDIF
C
      CALL HRESET(HSTID,' ')
      FLAG = .FALSE.
      BEAMX = BEAM_POS(1)
      BEAMY = BEAM_POS(2)
C
C get Z information from CDC TRACKS and fill Z into a histogram
C
      LDTRH = GZDTRH()
      IF (LDTRH .LE. 0) RETURN
      LDTRK = LQ(LDTRH - 1)
 100  CONTINUE
      IF (LDTRK .GT. 0 ) THEN
        IF (IQ(LDTRK+15).EQ.0) THEN
          THETA = Q(LDTRK+9)
          IF (THETA .NE. 0) THEN
            PHI = Q(LDTRK + 6)
            X0 = Q(LDTRK + 7)
            Y0 = Q(LDTRK + 8)
            TANPHI = TAN(PHI)
            IMPACT = ABS(TANPHI * BEAMX - BEAMY + Y0 - TANPHI * X0)
     &           / SQRT(1 + TANPHI**2)
            IF (IMPACT .GT. BXYLMT) GOTO 200
            R0 = SQRT((X0-BEAMX)**2 + (Y0-BEAMY)**2)
            ZPOSIT = Q(LDTRK+11) - R0 / TAN(THETA)

            IF (ABS(ZPOSIT) .GT. 100.0) GOTO 200
            CALL HFILL(HSTID,ZPOSIT,0.,1.)
          ENDIF
        ENDIF
 200    LDTRK = LQ(LDTRK)
        GOTO 100
      ENDIF
C
C Find peak ...
C
      CALL HNOENT(HSTID,NENTRY)	
      IF (NENTRY .LE. 2) GO TO 999
      CALL HUNPAK(HSTID,CONTEN,' ',1)
C
      CALL VZERO(IPEAK,10)
C
C  find the peak
C
      PEAK1 = 0.0
      CLUST1 = .FALSE.
      J = 1
C
      DO 201 I = ILOW, IHI-1
        IF (CONTEN(I) .GT. PEAK1) THEN
          PEAK1 = CONTEN(I)
          IPEAK(1) = I
          CALL VZERO(IPEAK(2),9)
          J = 1
        ELSE
          IF (CONTEN(I) .GT. 0.0 .AND. CONTEN(I) .EQ. PEAK1) THEN
            J = J + 1
            IF (J .LE. 10) IPEAK(J) = I
          ENDIF
        ENDIF
  201 CONTINUE
      IF (PEAK1 .LE. 0.0) GO TO 999
      IF (PEAK1 .EQ. 1.0) CLUST1 = .TRUE.
C
C determine the the peak region
C
      IF (CLUST1) THEN
        J = 0
        K = 0
        JJ = 1
        KK = 1
        DO 211 I = IPEAK(1), IHI
          IF (CONTEN(I) .GT. 0.0) THEN
            IF (J .EQ. 0) THEN
              J = I
            ELSE
              JJ = JJ + 1
            ENDIF
          ELSE
            IF (JJ .GT. KK) THEN
              KK = JJ
              K = J
            ENDIF
            J = 0
            JJ = 1
          ENDIF
  211   CONTINUE
        IF (KK .GT. 1) THEN
          KK = K + kk
          ILEAD = K 
          ITAIL = KK
          FLAG = .TRUE.
        ENDIF
        GOTO 204
      ENDIF
C
      ILEAD = 1
      ITAIL = 100
      FLAG = .TRUE.
      DO 202 I = IPEAK(1)+1, IHI-1
        IF (CONTEN(I) .EQ. 0.0 .AND. CONTEN(I+1) .EQ. 0.0) THEN
          ITAIL = I
          GOTO 205
        ENDIF
  202 CONTINUE
  205 DO 203 I = IPEAK(1)-1, ILOW+1, -1
        IF (CONTEN(I) .EQ. 0.0 .AND. CONTEN(I-1) .EQ. 0.0) THEN
          ILEAD = I
          GOTO 204
        ENDIF
  203 CONTINUE
C
C
  204 IF (FLAG) THEN
        CALL HIX(HSTID,ILEAD,LEAD)
        CALL HIX(HSTID,ITAIL,TAIL)
      ENDIF
C
  999 RETURN
      END
