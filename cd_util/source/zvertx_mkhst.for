      SUBROUTINE ZVERTX_MKHST(HSTID,LEAD,TAIL)
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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LDTRH, LDTRK, GZDTRH
      INTEGER IER, HSTID, HSTID1, HSTIDN
      INTEGER GDTRK, RUN, I
      INTEGER ID, RUNSAV
      INTEGER TOTTRK
      PARAMETER( TOTTRK = 200 )
      REAL    LEAD, TAIL
      REAL    X0, Y0, PHI, TANPHI, BEAMX, BEAMY, BXYLMT, IMPACT
      REAL    ZPOSIT, THETA, R0
      REAL    IMP(TOTTRK), ZPOS(TOTTRK), BESTIMP, BESTZ
      REAL    BEAM_POS(3)
      LOGICAL FIRST, EZERROR, HEXIST, HIST
      SAVE  FIRST
      DATA  FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV) THEN
        RUNSAV = RUN
        FIRST = .TRUE.
      ELSE
        FIRST = .FALSE.
      ENDIF
      IF ( FIRST ) THEN
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZVERTX_MKHST',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('BXYLMT',BXYLMT,IER)
        CALL EZGET('BEAM_POS',BEAM_POS,IER)
        CALL EZRSET
      ENDIF
      CALL VZERO(IMP,TOTTRK)
      CALL VZERO(ZPOS,TOTTRK)
      BESTIMP = 99.9
      BESTZ = 999.9
      GDTRK = 0
      BEAMX = BEAM_POS(1)
      BEAMY = BEAM_POS(2)
C
C get Z information from CDC TRACKS and fill Z into a histogram
C
      LDTRH = GZDTRH()
      IF (LDTRH .LE. 0) RETURN
      LDTRK = LQ(LDTRH - 1)
 100  CONTINUE
      IF (LDTRK .GT. 0) THEN
        THETA = Q(LDTRK+9)
        IF (THETA .NE. 0) THEN
          PHI = Q(LDTRK + 6)
          X0 = Q(LDTRK + 7)
          Y0 = Q(LDTRK + 8)
          TANPHI = TAN(PHI)
          IMPACT = ABS(TANPHI * BEAMX - BEAMY + Y0 - TANPHI * X0)
     &           / SQRT(1 + TANPHI**2)
          IF (IMPACT .GT. BXYLMT) GOTO 200
          GDTRK = GDTRK + 1
C          ZPOSIT = Q(LDTRK+11) - Q(LDTRK+10) / TAN(THETA)
          R0 = SQRT((X0-BEAMX)**2 + (Y0-BEAMY)**2)
          ZPOSIT = Q(LDTRK+11) - R0 / TAN(THETA)
          IF (GDTRK .GT. TOTTRK) GOTO 200
          IMP(GDTRK) = IMPACT
          ZPOS(GDTRK) = ZPOSIT
          IF (IMPACT .LT. BESTIMP) THEN
            BESTIMP = IMPACT
            BESTZ = ZPOSIT
          ENDIF
          IF (ZPOSIT .LT. LEAD .OR. ZPOSIT .GT. TAIL) GOTO 200
          CALL HFILL(HSTID,ZPOSIT,0.,1.)
        ENDIF
 200    LDTRK = LQ(LDTRK)
        GOTO 100
      ENDIF
      HSTID1 = HSTID
  999 RETURN
C
      ENTRY ZMKHST(LEAD,TAIL,HSTIDN)
C
      HSTID1 = HSTID1 - 1
C
      HIST = HEXIST(HSTID1)
      IF (.NOT. HIST) THEN
        CALL HBOOK1(HSTID1,'VERTEX SUBHISTGRAM$',100,-100.0,100.0,0.)
        CALL HIDOPT(HSTID1,'STAT')
      ELSE
        CALL HRESET(HSTID1,' ')
      ENDIF
      IF (LEAD .LT. 999.9) THEN
        DO 300 I = 1, GDTRK
          IF (ZPOS(I) .LT. LEAD .OR. ZPOS(I) .GT. TAIL) GOTO 300
          CALL HF1(HSTID1,ZPOS(I),1.)
  300   CONTINUE
      ELSE
        IF (BESTZ .GE. LEAD .AND. BESTZ .LE. TAIL) 
     &    CALL HF1(HSTID1,BESTZ,1.)
      ENDIF
      HSTIDN = HSTID1
C
      END
