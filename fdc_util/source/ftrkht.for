      SUBROUTINE FTRKHT(LKFDCT, NHIT, HITX, HITY, HITZ, WR, WZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns FDC hits along one FDC track
C-                         preserving the accuracy along the drift
C-                         direction in XY of each hit and using the track
C-                         fit results in the perpendicular XY direction.
C-                         Z-direction is the wire position.
C-
C-   Inputs  : LKFDCT   = Link to FTRAKS track
C-   Outputs : NHIT     = Number of hits on track
C-             HITX(I),HITY(I),HITZ(I) = X,Y,Z coordinates of the Ith
C-                                       FDC hits on track
C-             WR(I)    = Weight of HITX(I) and HITY(I)
C-             WZ(I)    = Weight of HITZ(I)
C-
C-   Created  29-JUN-1990   Jeffrey Bantly
C-   Updated  24-JAN-1991   Jeffrey Bantly  add hit data check and cleanup 
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                           with FDRIFTDIR. Also cleanup. 
C-   Updated   2-AUG-1991   Susan K. Blessing  Delay line information was
C-    being included incorrectly.  If a delay line was present in the FDCT
C-    track, it was included in the HITx arrays twice and the wire 0 hit
C-    was left out.  The X, Y and Z information for a delay line hit 
C-    is identical to the wire 0 information since the actual delay line 
C-    position was not being used. The weight, however, was quite different.
C-    The weight of a d.l. point is about .1, the wire 0 weight is about 1000. 
C-    (Both were being used as .1.)  
C-    Since the FDCT tracks are from three dimensional fits, information 
C-    from the sense wires is far more valuable than that from the delay 
C-    lines.  Leave the delay lines out of the ZTRK fit now.  This means 
C-    the maximum value of NHIT will be 32 and the HITx and Wx arrays have 
C-    been adjusted.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to
C-     26 (two errors and two spares).
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove EXTERNAL FSTAGER.
C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LKFDCT,NHIT
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,TRKNUM,IHALF
      INTEGER IHSEC,IHTRK(0:2),ICALL,IER,NHITS
      INTEGER IADD,IHIT,LR,LADDER(0:2)
      INTEGER IQTRAK(26),NEL,NWORDS,ITOT
      INTEGER PREV_IADD
C
      REAL HITX(32),HITY(32),HITZ(32),WR(32),WZ(32)
      REAL QTRAK(26),CONT(26),QHSEC(3,34),DRIFTD
      REAL QHIT(18),STAGGER
      REAL SDRIFT,CDRIFT,XC,YC,ZC,X0,Y0,Z0(2)
      REAL DXDZ,DYDZ,ZTRK,XTRK,YTRK,PHI,THETA,RADIUS
      REAL R,RPHI,WPHI,RPERP,RWIR
      EQUIVALENCE(QTRAK,IQTRAK)
      REAL FSTAGR
C
      SAVE ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (LKFDCT.LE.0) GOTO 999
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
      ENDIF
      CALL VZERO(CONT,26)
      CALL VZERO(QHSEC,102)
C
C  Get track number from link and fetch track info
C
      TRKNUM = IQ(LKFDCT-5)
      CALL GTFDCT(TRKNUM,CONT,QHSEC,LADDER)
      CALL UCOPY(CONT,QTRAK,26)
      IF (IQTRAK(2).EQ.0) GOTO 999      ! If no hits on track then return
      CALL UCOPY(QHSEC(1,1),IADD,1)
      IF (IADD.LE.0) GOTO 999         ! No hit info with track
C
C  Get information of hits on the track
C
      NHITS = IQTRAK(2)
      DXDZ = QTRAK(7)
      DYDZ = QTRAK(8)
      ITOT = 0
      XTRK = QTRAK(4)
      YTRK = QTRAK(5)
      ZC = Z0(HALF+1)
      RADIUS = SQRT(XTRK**2. + YTRK**2.)
      PHI = 0.
      THETA = 0.
      IF (ZC.NE.0.000) THETA = ATAN2(RADIUS,ZC)
      IF (XTRK.NE. 0.000) PHI = ATAN2(YTRK,XTRK)
      PREV_IADD = -1
C
      DO 10 IHSEC = 1,NHITS
        CALL UCOPY(QHSEC(1,IHSEC),IADD,1)
        CALL UCOPY(QHSEC(2,IHSEC),IHIT,1)
C
C Do not include delay line information in arrays for ZTRK fit.
C IADD (logical address of wire) is identical for a theta SW0 hit and its
C delay line hit.  IADD should never be the same for other wires on a track.
        IF (IADD.EQ.PREV_IADD) THEN
          GO TO 10
        ELSE
          PREV_IADD = IADD
        END IF
C
        ITOT = ITOT + 1
C
        CALL FCODER((IADD/2),HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IF (UNIT.LE.0) THEN
          CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT,NEL,
     X           NWORDS,QHIT)
        ELSE
          CALL GTFPSC(HALF,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
        END IF
C
        LR = IADD - 2*(IADD/2)
        STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
        DRIFTD = QHIT(2+LR)-STAGGER
C
        CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
        CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
        IF (ZC.EQ. 0.0 .OR.ABS(ZC).GT. 150.) GOTO 10
        XTRK = QTRAK(4)+DXDZ*(ZC-Z0(HALF+1))
        YTRK = QTRAK(5)+DYDZ*(ZC-Z0(HALF+1))
        R = SQRT(XTRK**2 + YTRK**2)
        IF (XTRK.EQ.0.0) THEN
          XTRK = 0.00000001
          IF (YTRK.GE. 0.0) RPHI = PI/2.
          IF (YTRK.LT. 0.0) RPHI = 3.*PI/2.
        ELSE
          RPHI = ATAN2(YTRK,XTRK)
        ENDIF
        IF (XC.EQ. 0.0) THEN
          XC = 0.00000001
          IF (YC.GE. 0.0) WPHI = PI/2.
          IF (YC.LT. 0.0) WPHI = 3.*PI/2.
        ELSE
          WPHI = ATAN2(YC,XC)
        ENDIF
C
        IF (UNIT.EQ.0) THEN
C Theta chamber
          RPERP = R*SIN(RPHI-WPHI)
          IF (SECTOR.EQ.1) THEN
            HITX(ITOT) = XC+DRIFTD*CDRIFT-
     &             (RPERP*(-SDRIFT))
            HITY(ITOT) = YC+DRIFTD*SDRIFT-
     &             (RPERP*CDRIFT)
            HITZ(ITOT) = ZC
          ELSE
            HITX(ITOT) = XC+DRIFTD*CDRIFT+
     &             (RPERP*(-SDRIFT))
            HITY(ITOT) = YC+DRIFTD*SDRIFT+
     &             (RPERP*CDRIFT)
            HITZ(ITOT) = ZC
          ENDIF
C
          IF (QHIT(5).GT. 0.000001) THEN
            WR(ITOT) = 1/(QHIT(5)**2.)
          ELSE
            WR(ITOT) = 1/(0.5**2.)                ! In case of QHIT(5) = 0.
          END IF
          WZ(ITOT) = 1/(.02**2.)       ! Wires located within 20 microns,EST,
C                                      ! but used 200 microns.
        ELSE                          
C Phi Chamber
          RWIR = R*COS(RPHI-WPHI)
          HITX(ITOT) = DRIFTD*CDRIFT+
     &             RWIR*(SDRIFT)
          HITY(ITOT) = DRIFTD*SDRIFT+
     &             RWIR*(-CDRIFT)
          HITZ(ITOT) = ZC
C
          IF (QHIT(5).GT.0.0) THEN
            WR(ITOT) = 1/(QHIT(5)**2.)
          ELSE
            WR(ITOT) = 1/(0.5**2.)                ! In case of QHIT(5) = 0.
          END IF
          WZ(ITOT) = 1/(.02**2.)       ! Wires located within 20 microns,EST,
C                                      ! but used 200 microns.
        ENDIF
C
   10 CONTINUE
C
      NHIT = ITOT
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
