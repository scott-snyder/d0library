      SUBROUTINE FIX_VERH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fix bug in VERH and recover BEAM_POS/BEAM_ERR values.
C-                         Also add Word 11 to VERH bank.
C-
C-   Inputs  : VERH, VERT bank
C-   Outputs : fixed VERH bank
C-   Controls:
C-
C-   Created   5-SEP-1995   Srini Rajagopalan
C-   Updated  18-SEP-1995   Srini Rajagopalan  Fix bad BEAM_ERR with RCP #s.
C-   Updated  26-SEP-1995   Srini Rajagopalan  Add word to store old z position
C-                                             (still version 3) 
C-   Updated   3-OCT-1995   Srini Rajagopalan  Add Check on VERH size. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C
      INTEGER IER,ND
      INTEGER LVERH, LVERT, GZVERH
      INTEGER LNEW, LTMP, IXVERH
      INTEGER VERSION, MBOOKT(5)
C
      REAL    BEAM_POS(3), BEAM_ERR(2), BEAM_SLOP(2), BMSL_ERR(2),
     &        IP_SIGMA(2),RCP_BEAM_ERR(3)
      REAL    XVER, YVER, ZVER, DXVER, DYVER
C
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('BEAM_POS',BEAM_POS,IER)
        IF (IER.EQ.0) CALL EZGET('BEAM_SLOP',BEAM_SLOP,IER)
        IF (IER.EQ.0) CALL EZGET('BMSL_ERR',BMSL_ERR,IER)
        IF (IER.EQ.0) CALL EZGET('IP_SIGMA',IP_SIGMA,IER)
        IF (IER.EQ.0) CALL EZGET('BEAM_ERR',RCP_BEAM_ERR,IER)
        IF (IER.NE.0) CALL ERRMSG('Bad RCP','FIX_VERH',
     &                            'RCP variables not found','F')
        CALL EZRSET
C
        CALL MZFORM('VERH','3I 8F 1I',IXVERH)
        CALL UCTOH('VERH',MBOOKT(1),4,4)
        MBOOKT(2) = 3
        MBOOKT(3) = 2
        MBOOKT(4) = 12
        MBOOKT(5) = IXVERH
      ENDIF
C
C  The bug is in Version - 2 and below.  If version.eq.3 do nothing
C
      LVERH = GZVERH()
      IF (LVERH.LE.0) GO TO 999
      VERSION = IQ(LVERH+1)
      ND = IQ(LVERH-1)
      IF (VERSION.GE.3 .AND. ND.EQ.12) GO TO 999
C
C  Book a temporary VERH bank correctly and replace old VERH bank
C
      CALL MZBOOK(IXMAIN,LTMP,LTMP,1,'VTMP',2,2,1,0,-1)
C
      LQ(LTMP-1) = LVERH                             ! Old VERH bank
C
      CALL MZLIFT(IXMAIN,LNEW,LTMP,-2,MBOOKT,0)      ! New VERH bank
C
C copy from old to new
C
      CALL UCOPY(LQ(LVERH-3),LQ(LNEW-3),3)           ! ref links
      IQ(LNEW+1) = 3                                 ! version #
      CALL UCOPY(IQ(LVERH+2),IQ(LNEW+2),2)           ! data
C
      CALL MZREPL(IXMAIN,LTMP,' ')                   ! replace
C
C  Now recover the beam_slope positions.
C
      LVERH = GZVERH()
      LVERT = LQ(LVERH-IZVERT)
      IF (LVERT.LE.0) GO TO 999       ! tough luck
C
      XVER = Q(LVERT+3)
      YVER = Q(LVERT+4)
      ZVER = Q(LVERT+5)
      DXVER = Q(LVERT+6)
      DYVER = Q(LVERT+7)
C
      BEAM_POS(1) = XVER - BEAM_SLOP(1)*ZVER
      BEAM_POS(2) = YVER - BEAM_SLOP(2)*ZVER
C
      BEAM_ERR(1) = DXVER**2 - IP_SIGMA(1)**2 -
     &              ((ZVER - BEAM_POS(3)) * BMSL_ERR(1))**2
      IF (BEAM_ERR(1).GT.0.) THEN
        BEAM_ERR(1) = SQRT(BEAM_ERR(1))
      ELSE
        BEAM_ERR(1) = RCP_BEAM_ERR(1)
      ENDIF
C
      BEAM_ERR(2) = DYVER**2 - IP_SIGMA(2)**2 -
     &              ( (ZVER-BEAM_POS(3)) * BMSL_ERR(2) )**2
      IF (BEAM_ERR(2).GT.0.) THEN
        BEAM_ERR(2) = SQRT(BEAM_ERR(2))
      ELSE
        BEAM_ERR(2) = RCP_BEAM_ERR(2)
      ENDIF
C
      CALL UCOPY(BEAM_POS(1),Q(LVERH+4),3)
      CALL UCOPY(BEAM_SLOP(1),Q(LVERH+7),2)
      CALL UCOPY(BEAM_ERR(1),Q(LVERH+9),2)
C
  999 RETURN
      END
