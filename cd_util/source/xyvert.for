      SUBROUTINE XYVERT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find x and y of event vertex as a function of z
C-                         X = Xb + dX/dZb * (Z - Zb); similar for Y.
C-     If a BMXY bank exists in ZEBSTP (on-line determined beam position),
C-     this routine will use the information contained therein to fill
C-     the VERT banks, setting bit 23 of word 2 to indicate that the XY
C-     information does not come from the RCP file.  It will then check
C-     to see whether a BMXY bank hangs from VTXH.  If not, it will
C-     MZCOPY the existing BMXY bank to link IZBMXY of VTXH, booking
C-     VTXH if necessary.
C-
C-   Inputs  : Z of vertex, Xb, Yb, Zb - beam position,
C-             dX/dZb, dY/dZb - beam slopes
C-   Outputs : X, dX, Y, dY of vertex (in bank VERT)
C-
C-   Created  18-SEP-1992   Alexandre Zinchenko
C-   Updated  21-OCT-1992   Qizhong Li-Demarteau  fix overwritting on FIRST
C-                                               and added a call to EZRSET
C-   Rewritten  18-APR-1994  Justin R. Bendich   Use XY information from
C-                                               online processes
C-   Modified   29-NOV-1994  Justin R. Bendich
C-      Use constant (not fitted) beam slope, and increase NomSlopErr
C-   Modified   30-JAN-1995  Justin R. Bendich
C-      Don't add in error from uncertainty on Z of vertex
C-      Put dXdZ and dYdZ into words 4 and 5 of the VERH bank
C-      Put X0, Y0, dX0, dY0 in words 6-9 of VERH bank  (e.o. 6-Feb-95)
C-
C-   Updated  16-MAY-1995   Norman A. Graf   Added updates to VFIT and
C-                                           check on MCDATA earlier
C-   Updated   6-JUL-1995   Liang-ping Chen  dX0 is word 9 in VERH, not word 8
C-   Updated   5-SEP-1995   Srini Rajagopalan Fix fetch of RCP_BEAM_POS/ERR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZBMXY.LINK'
      INCLUDE 'D0$PARAMS:BMXY.PARAMS'
      CHARACTER*39 Message
      INTEGER IER, LVERT, I, GZVERT, NVERT, Run, RUNNO, LastRun
      INTEGER LVERH, GZVERH, LVTXH, GZVTXH, LVFIT, GZVFIT
      REAL BEAM_POS(3), BEAM_ERR(2), BEAM_SLOP(2), BMSL_ERR(2),
     &     IP_SIGMA(2)
      REAL XVER, DXVER, YVER, DYVER, ZVER, DZVER
      LOGICAL FIRST, MCDATA
C
      REAL RCP_BEAM_POS(3), RCP_BEAM_ERR(3)
      COMMON/USYKGI/ RCP_BEAM_POS, RCP_BEAM_ERR
C
      DATA FIRST /.TRUE./, LastRun/-1/
   10 FORMAT('Error copying into ZEB',A3,'; IQUEST =',I4)
C-
      IF (FIRST) THEN
        FIRST = .FALSE.
        MCDATA = IQ(LHEAD + 1) .GT. 1000
        IF (MCDATA) GO TO 999
        CALL EZLOC('VERTEX_RCP', I)
        IF(I .LE. 0) THEN
          CALL INRCP('VERTEX_RCP', IER)
          IF(IER .NE. 0) CALL ERRMSG('INRCP','XYVERT','VERTEX_RCP','F')
        ENDIF
        CALL EZPICK('VERTEX_RCP')
        CALL EZGETA('BEAM_POS',1,3,1,RCP_BEAM_POS,IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYVERT',
     &    'BEAM_POS not found', 'F')
        CALL EZGETA('BEAM_ERR',1,2,1,RCP_BEAM_ERR,IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYVERT',
     &    'BEAM_ERR not found', 'F')
        CALL EZGETA('BEAM_SLOP',1,2,1,BEAM_SLOP,IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYVERT',
     &    'BEAM_SLOP not found', 'F')
        CALL EZGETA('BMSL_ERR',1,2,1,BMSL_ERR,IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYVERT',
     &    'BMSL_ERR not found', 'F')
        CALL EZGETA('IP_SIGMA', 1, 2, 1, IP_SIGMA, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYVERT',
     &    'IP_SIGMA not found', 'F')
        CALL EZRSET
      END IF
C
C ****  Modify X and Y of vertex only for real data
C
      IF (MCDATA) GO TO 999
      Run = RUNNO()
      IF(Run .NE. LastRun) THEN
        LastRun = Run
        IF(LBMXY .GT. 0) THEN
C
C ****  Get the information for the store. Ignore the run-by-run determination.
C ****  Maybe this should be studied?
C
          BEAM_POS(1) = C(LBMXY + 13)
          BEAM_POS(2) = C(LBMXY + 14)
          BEAM_POS(3) = C(LBMXY + 22)
          BEAM_ERR(1) = C(LBMXY + 17)
          BEAM_ERR(2) = C(LBMXY + 18)
        ELSE
C
C ****  Too bad, we have to use the RCP parameters (XYRCP reads these in)
C
          CALL UCOPY(RCP_BEAM_POS(1),BEAM_POS(1),3)
          CALL UCOPY(RCP_BEAM_ERR(1),BEAM_ERR(1),2)
        ENDIF
      ENDIF                                    !  Run .NE. LastRun
      IF(LBMXY .GT. 0) THEN
        LVTXH = GZVTXH(0)
        IF(LVTXH .LE. 0) THEN
          CALL BKVTXH
          LVTXH = GZVTXH(0)
        ENDIF
        IF((LQ(LVTXH - IZBMXY) .LE. 0) .AND.
     &     (IC(LBMXY + 1) .GT. 0)) THEN
          CALL MZCOPY(IDVSTP, LBMXY, IXMAIN, LVTXH, -IZBMXY, ' ')
          IF(IQUEST(1) .NE. 0) THEN
            WRITE(Message,10) 'COM', IQUEST(1)
            CALL ERRMSG('MZCOPY', 'XYVERT', Message, 'W')
          ENDIF
        ENDIF
      ENDIF
      LVERH = GZVERH()
      IF(LVERH .LE. 0) CALL BKVERH(LVERH)
      IF (IQ(LVERH+1) .LE. 2) IQ(LVERH+1) = 2
      CALL UCOPY(BEAM_POS(1),Q(LVERH+4),3)
      CALL UCOPY(BEAM_SLOP(1),Q(LVERH+7),2)
      CALL UCOPY(BEAM_ERR(1),Q(LVERH+9),2)
C
C ****  First VERT banks...
C
      NVERT = IQ(LVERH+2)
      DO 100 I = 1,NVERT
        LVERT = GZVERT(I)
        ZVER = Q(LVERT+5)
        DZVER = Q(LVERT+8)
        XVER = BEAM_POS(1) + BEAM_SLOP(1)*ZVER
        YVER = BEAM_POS(2) + BEAM_SLOP(2)*ZVER
        DXVER = SQRT(BEAM_ERR(1)**2 + ((ZVER - BEAM_POS(3)) *
     &               BMSL_ERR(1))**2 + IP_SIGMA(1)**2)
        DYVER = SQRT(BEAM_ERR(2)**2 + ((ZVER - BEAM_POS(3)) *
     &               BMSL_ERR(2))**2 + IP_SIGMA(2)**2)
        Q(LVERT+3) = XVER ! x_beam
        Q(LVERT+4) = YVER ! y_beam
        Q(LVERT+6) = DXVER
        Q(LVERT+7) = DYVER
        Q(LVERT+9) =  1. ! correlation matrix elem. cxx
        Q(LVERT+10) = 0. ! cxy
        Q(LVERT+11) = 0. ! cxz
        Q(LVERT+12) = 1. ! cyy
        Q(LVERT+13) = 0. ! cyz
        Q(LVERT+14) = 1. ! czz
        IF(LBMXY .GT. 0) THEN
          IQ(LVERT + 2) = IBSET(IQ(LVERT + 2), DBBit)
        ELSE
          IQ(LVERT + 2) = IBCLR(IQ(LVERT + 2), DBBit)
        ENDIF
  100 CONTINUE
C
C ****  Now VFIT banks...
C
      LVFIT = GZVFIT(1)
      DO WHILE (LVFIT.GT.0)
        ZVER = Q(LVFIT+5)
        DZVER = Q(LVFIT+8)
        XVER = BEAM_POS(1) + BEAM_SLOP(1)*ZVER
        YVER = BEAM_POS(2) + BEAM_SLOP(2)*ZVER
        DXVER = SQRT(BEAM_ERR(1)**2 + ((ZVER - BEAM_POS(3)) *
     &               BMSL_ERR(1))**2 + IP_SIGMA(1)**2)
        DYVER = SQRT(BEAM_ERR(2)**2 + ((ZVER - BEAM_POS(3)) *
     &               BMSL_ERR(2))**2 + IP_SIGMA(2)**2)
        Q(LVFIT+3) = XVER ! x_beam
        Q(LVFIT+4) = YVER ! y_beam
        Q(LVFIT+6) = DXVER
        Q(LVFIT+7) = DYVER
        Q(LVFIT+9) =  1. ! correlation matrix elem. cxx
        Q(LVFIT+10) = 0. ! cxy
        Q(LVFIT+11) = 0. ! cxz
        Q(LVFIT+12) = 1. ! cyy
        Q(LVFIT+13) = 0. ! cyz
        Q(LVFIT+14) = 1. ! czz
        IF(LBMXY .GT. 0) THEN
          IQ(LVFIT + 2) = IBSET(IQ(LVFIT + 2), DBBit)
        ELSE
          IQ(LVFIT + 2) = IBCLR(IQ(LVFIT + 2), DBBit)
        ENDIF
        LVFIT = LQ(LVFIT)
      ENDDO
C
  999 RETURN
      END
