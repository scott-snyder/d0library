      SUBROUTINE FTVTXT(LADDER)
C------------------------------------------------------------------------
C
C  Fit a VTX track candidate in x-y and r-z.
C  In x,y plane fit the data to a straight line:
C
C          (y-YG)cos(phi)=(x-XG)*sin(phi),
C
C  in r,z plane fit:
C
C           s-SG=(z-ZG)*tg(theta)
C
C  where s=sqrt((y-YG)**2+(x-XG)**2), assuming that the error of s is
C  negligible compared to the error of z.
C  Store the track in Zebra bank 'VTXT' and the associated hits in 'VTTH.'
C
C  INPUT:
C         LADDER(0:2) = segments in layers (0:2) on track candidate
C
C  Daria Zieminska May 1987
C                  Oct.1988: take geometry from static parameter file
C-   Updated 24-AUG-1991  P. Grudberg Add compressed hits option
C-   Updated   4-NOV-1991   P. Grudberg Fix PATH, change input to VTTH
C-   Updated  19-MAR-1992   M.Pang, Added dE/dX part
C-   Updated   8-JUN-1992   Peter M. Grudberg  Change theta to sin(theta) in
C-                            call to VXDEDX; set status bit if sintheta
C-                            correction done in dedx.
C-   Updated  11-AUG-1992   M. Pang, Added Area Correction Based on
C-                              Drift Distance
C_   updated  31-aug-1992   L. Chen use the wire dX/dZ and dY/dZ in VALS.ZEB
C-   Updated   6-NOV-1992   Peter M. Grudberg New VTXT format, remove strips,
C-                            make dx/dz,dy/dz use optional
C-   Updated  11-FEB-1993   Ed Oltman   Use z-info from road definition (if it
C-                          exists) and utilize dx/dz,d2x/dz2 etc. of wires
C-   Updated  22-MAR-1993   Ed Oltman  skip hit loop if MINHIT not satisfied
C-   Updated  23-MAR-1993   Ed Oltman   replace vtx SIN(THETA) with road info
C-   Updated  04-APR-1993   Ed Oltman   add a new method to select R-Z hits,   
C-              use all R-Z hits in a wide THETA road if RZMETHOD=1
C-   Updated  19-APR-1993   Liang-ping Chen Do not use saturated pulse for Z
C-   Updated  20-APR-1993   Al Clark, Liang-ping Chen Move TAN() SQRT() out 
C-                          of DO LOOP. Correct DZ and QHSEC(4,IH)  
C-   Updated  22-APR-1993   Al Clark, correct SXY, same is done in VFITSG
C-                          Remove unused or redundant variables.
C_                          correct the dimension of CONT from 27 to 31
C-   Updated  29-APR-1993   Al Clark Fix phi bug, per VFITSG fix of 11/12/92
C_   Updated  03-MAY-1993   L.Chen, add map to VSGs in VTTH 
C-   Updated   7-OCT-1993   Liang-Ping Chen Use PULL weight for RZMETHOD=0
C-   Updated  11-OCT-1993   Ed Oltman   remove ladder tag
C-   Updated  10-DEC-1993   Liang-Ping Chen apply NZTOT-NZBAD test within
C-                          each RZMETHOD. Add MINZHIT cut, the minimum 
C-                          number of Z hits required before a VTXT bank is
C-                          booked
C-   Updated  13-DEC-1993   Ed Oltman Save SIN(THETA) in word 21; compute
C-                          z-related stuff even for fewer then NZGOOD zhits
C-   Updated  14-JAN-1994   Ed Oltman Save dz/dr and ZVTX in words 14 and 15,
C-                          instead of "total number of degrees of freedom"
C-                          and "number of z-strip hits related to track".
C-   Updated  15-FEB-1994   Al Clark   Delete tagging of hits in VHIT; remove
C-                          all ref to VHIT; clean up unused variables.
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      REAL QHIT(18),QTRAK(21),QHSEC(4,24),ZBITS
      INTEGER IQTRAK(21),IQHSEC(4,24),MINHIT 
      EQUIVALENCE (IQTRAK,QTRAK),(IQHSEC,QHSEC)
      INTEGER IQHIT(18),NEL,NWORDS,IPAL,LVALS,IZBITS,NZGOOD
      INTEGER MINZHIT 
      EQUIVALENCE(IZBITS,ZBITS)
      EQUIVALENCE (IQHIT,QHIT)
      REAL ARRAY(16)
      INTEGER IARRAY(16),MXHIT,ISBITS
      EQUIVALENCE (IARRAY,ARRAY)
      INTEGER NLAYER,LAYER,SECTOR,NWIRES,HITADD,WIRENB(24)
      PARAMETER (NLAYER=2)
      PARAMETER (NWIRES=7)
      PARAMETER (MXHIT=24)
      INTEGER LADDER(0:NLAYER),NHSEC,NZTOT,NZBAD,IZ
      INTEGER IZTOT,NHITS,IPNT
      INTEGER ID,WIRE,LR,LRWIR,IADD
      INTEGER GZVALS, STATUS
      REAL CONT(31),XHIT(24),YHIT(24),WT(24),DRIFT
      REAL RHIT(24),SXY(24),ZHIT(24),ZERR(24)
      REAL WTZ(24)
      REAL CHIMAX,CHIDF,CHIDFZ,CHIMXZ,VZGTHETA,FACTOR
      REAL AL,XG,YG,ALZ,THETA,SINTHE,SG,ZG,VG,VAL,VZG,VALZ
      REAL TGALZ, SINAL, COSAL, PHIG, PHIDIFF
      REAL FIADD,FNHITS,FLRWIR,FIPNT
      REAL AREAINF(5,MXHIT),MIP,MIPERR
      EQUIVALENCE (IADD,FIADD),(NHITS,FNHITS)
      EQUIVALENCE (LRWIR,FLRWIR),(IPNT,FIPNT)
      INTEGER ICALL,IBAD,IH,POINT
      REAL PI,PI2,DZ,ABSDZ,DZMAX
      REAL DZTOL0,DZTOL1
      INTEGER RZMETHOD
      LOGICAL VCHEKL
      LOGICAL SIN_CORRECTION, SLOPE_CORRECT
      INTEGER SIN_COR_BIT
      PARAMETER ( SIN_COR_BIT = 0 )
      INTEGER IER
      INTEGER HITNUM, LAY, NLAY
      REAL  PHIR,DZDR,ZVTX,ZPRED
      REAL PULLQ(24), COSALZ
      DATA PI,PI2/3.141593,6.283185/
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('MINHIT',MINHIT,IER)
        CALL EZGET('CHIMAX_TRACK',CHIMAX,IER)
        CALL EZGET('CHIMXZ_TRACK',CHIMXZ,IER)
        CALL EZGET('RZMETHOD',RZMETHOD,IER)
        CALL EZGET('DZTOL0',DZTOL0,IER)          ! r-z tolerance in sigmans
        CALL EZGET('DZTOL1',DZTOL1,IER)          ! r-z tolerance in cm
        CALL EZGET('NZGOOD',NZGOOD,IER)
        CALL EZGET('SIN_CORRECTION',SIN_CORRECTION,IER)
        CALL EZGET('SLOPE_CORRECT',SLOPE_CORRECT,IER)
        CALL EZGET('MINZHIT',MINZHIT,IER)
        CALL EZRSET
        ICALL=1
      END IF
      IF (VCHEKL(LADDER)) GO TO 1000
      NLAY = 0
      DO LAY = 0,2
        IF (LADDER(LAY) .NE. 0) NLAY = NLAY + 1
      ENDDO
      IF (8*NLAY .LT. MINHIT) GO TO 1000
      NHSEC=0
      NZTOT=0
      NZBAD=0
      CALL VZERO(YHIT,24)
      CALL VZERO(ZHIT,24)
      CALL VZERO(AREAINF,5*MXHIT)
      IQTRAK(3)=0
      IQTRAK(4)=0
      CALL VRDGET(ZVTX,PHIR,DZDR)
C
C
C  Loop through segments on ladder and get hits assigned to them.
C
      DO 100 LAYER=0,NLAYER
        IF (LADDER(LAYER).EQ.0) GO TO 100
C
C  Get contents of segment in LAYER
C
        CALL GTSEGM(LAYER,LADDER(LAYER),CONT)
        FIADD=CONT(2)
        FNHITS=CONT(3)                    ! number of hits in segment
        SECTOR=IADD-LAYER*32
        LVALS = GZVALS(LAYER,SECTOR)
C
C ****  Get pointer info from VSEC
C
        CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,ARRAY)
C
C  Loop through wire hits on segment and get their coordinates
C
        IADD=IADD*16
        DO 200 ID=1,NHITS
          NHSEC=NHSEC+1
          FIPNT=CONT(11+ID)
          FLRWIR=CONT(3+ID)
          WIRE = LRWIR / 2
          POINT = IARRAY(NEL+WIRE+1)
          HITNUM = (IPNT - POINT)/NWORDS + 1
          IQHSEC(2,NHSEC) = HITNUM
          HITADD=IADD+LRWIR
          IQHSEC(1,NHSEC)=HITADD
          WIRENB(NHSEC)=WIRE+LAYER*8
          IQTRAK(3)=IBSET(IQTRAK(3),WIRENB(NHSEC))
          IQTRAK(4)=IBSET(IQTRAK(4),WIRENB(NHSEC))
          LR=LRWIR-WIRE*2
          CALL GTVSEC(LAYER,SECTOR,'HIT',IPNT,NEL,NWORDS,QHIT)
          DRIFT=QHIT(2+LR)-(C(LC(LVGEH-3)+31+WIRE))*(-1.)**SECTOR
          CALL V_CORRECT_AREA(LAYER,WIRE,DRIFT,QHIT(7))
          IPAL=LVALS+6+IC(LVALS+6)*WIRE
          XHIT(NHSEC) = C(IPAL+1)+DRIFT*C(LVALS+3) ! x coordinate in D0 frame
          YHIT(NHSEC) = C(IPAL+2)+DRIFT*C(LVALS+4) ! y coordinate in D0 frame
C
C ****  If requested, make correction for slope of wire in D0 frame (dx,dz).
C ****  Demand that the hit has hits on both ends.
C
C
C ****  BUT FIRST, IF ROAD INFO EXISTS, USE THAT
C
          IF (ABS(ZVTX) .LT. 999.) THEN
            ZPRED = ZVTX + SQRT(XHIT(NHSEC)**2 + YHIT(NHSEC)**2)*DZDR
            XHIT(NHSEC) = XHIT(NHSEC) 
     &        + C(IPAL+4)*ZPRED + C(IPAL+6)*ZPRED**2
            YHIT(NHSEC) = YHIT(NHSEC) 
     &        + C(IPAL+5)*ZPRED + C(IPAL+7)*ZPRED**2
          ELSE
            STATUS = IBITS(IQHIT(10),0,2)
            IF ( STATUS .EQ. 3 .AND. SLOPE_CORRECT ) THEN
              XHIT(NHSEC) = XHIT(NHSEC) + C(IPAL+4)*QHIT(4)
              YHIT(NHSEC) = YHIT(NHSEC) + C(IPAL+5)*QHIT(4)
            ENDIF
          ENDIF
          WT(NHSEC)=1./QHIT(5)**2            ! QHIT(5) is error in x-y plane
          RHIT(NHSEC)=SQRT(XHIT(NHSEC)**2+YHIT(NHSEC)**2)
          ZHIT(NHSEC)=QHIT(4)                      ! z coordinate in D0 frame
          WTZ(NHSEC)=1./QHIT(6)**2           ! QHIT(6) is error on z
          ZERR(NHSEC) = QHIT(6)
          ZBITS=QHIT(10)
          ISBITS = IZBITS
          IZBITS=IBITS(IZBITS,0,2)
          AREAINF(1,NHSEC) = QHIT(7)
          AREAINF(2,NHSEC) = DRIFT
          AREAINF(3,NHSEC) = FLOAT(IZBITS)
          IF ( IBITS(ISBITS,8,1).EQ.1 .OR.
     &             IBITS(ISBITS,12,1).EQ.1 ) THEN
            AREAINF(5,NHSEC) = 1.
          END IF
          IF (IZBITS.NE.3 .OR.
     &         IBITS(ISBITS,8,1).EQ.1 .OR.
     &         IBITS(ISBITS,12,1).EQ.1 ) THEN
            NZBAD=NZBAD+1
            WTZ(NHSEC)=0.
            IQTRAK(4) = IBCLR(IQTRAK(4),WIRENB(NHSEC))
          END IF
          NZTOT=NZTOT+1
  200   CONTINUE
  100 CONTINUE
      IF (NHSEC.LT.MINHIT) GO TO 1000
C
C  Fit in x-y projection
C
      CALL FITLIN(XHIT,YHIT,WT,NHSEC,AL,XG,YG,VG,VAL,CHIDF)
      IF (CHIDF.GT.CHIMAX) GO TO 1000
      IF (NHSEC.LT.8.AND.CHIDF.GT.3.) GO TO 1000
C
C ****  Make sure the phi angle is not off by PI - it should be very close to
C ****  the phi of the center of gravity.  Then force it into the range from 0
C ****  to twopi.
C
      PHIG = ATAN2(YG,XG)
      PHIDIFF = PHIG - AL
      PHIDIFF = PHIDIFF - NINT(PHIDIFF/PI)*PI
      AL = PHIG - PHIDIFF
      IF (AL.LT.0.) AL=AL+PI2
      IF (AL.GT.PI2) AL=AL-PI2
      SINAL = SIN(AL)
      COSAL = COS(AL)
      DO 900 IZTOT=1,NZTOT
        SXY(IZTOT)=(XHIT(IZTOT)-XG)*COSAL+(YHIT(IZTOT)-YG)*SINAL
  900 CONTINUE
C                  
C  Fit in s-z projection
C
      IF (RZMETHOD.EQ.0 .OR. ABS(ZVTX) .GT. 999.) THEN
        IF (NZTOT-NZBAD.LT.NZGOOD) GO TO 700
C
C ****  No external r-z information used here: throw away bad r-z hits on basis
C ****  of PULL weight (RZMETHOD=0 should NOT be used for road tracking!!)
C
  600   CONTINUE
        CALL FITLOC(SXY,ZHIT,WTZ,NZTOT,ALZ,SG,ZG,VZG,VALZ,CHIDFZ)
        THETA=1.570796-ALZ
C
C  Since the pattern recognition was done in r-phi plane, there may be
C  hits with wrong z.
C  Check residuals in r-z. If the largest residual exceeds DZTOL
C  remove the hit and refit the track.
C
        DZMAX=0.
        TGALZ = TAN(ALZ)
        COSALZ = COS(ALZ)
        DO 800 IZ=1,NZTOT
          PULLQ(IZ) = 0.
          IF (WTZ(IZ).GT.0.) THEN
            DZ= (ZHIT(IZ)-ZG)-(SXY(IZ)-SG)*TGALZ  ! residual
            PULLQ(IZ) = DZ*DZ/
     &          (1./WTZ(IZ)-(VZG+((SXY(IZ)-SG)/COSALZ**2)**2*VALZ) )
            ABSDZ=ABS(PULLQ(IZ))
            IF (ABSDZ.GT.DZMAX) THEN
              DZMAX=ABSDZ
              IBAD=IZ
            END IF
          END IF
  800   CONTINUE
        IF (DZMAX.GT.DZTOL0) THEN
          WTZ(IBAD)=0.
          IF (IBAD.LE.NHSEC) THEN
            IQTRAK(4)=IBCLR(IQTRAK(4),WIRENB(IBAD))
          END IF
          NZBAD=NZBAD+1
          AREAINF(4,IBAD) = 1.
          IF (NZTOT-NZBAD.LT.NZGOOD) GO TO 700
          GO TO 600                         ! refit without bad hit
        ENDIF
      ELSE 
C
C ****  External r-z information IS available -- eliminate bad r-z hits on basis
C ****  of both distance from road center and standard devaitions from road
C ****  center.
C
        DO ID = 1,NHSEC
          IF (WTZ(ID) .GT. 0.) THEN
            ZPRED = ZVTX + RHIT(ID)*DZDR
            IF ( (ABS(ZHIT(ID)-ZPRED) .GT. DZTOL1) .OR.
     &           (ABS(ZHIT(ID)-ZPRED) .GT. DZTOL0*ZERR(ID)) ) THEN
              WTZ(ID) = 0.
              IQTRAK(4) = IBCLR(IQTRAK(4),WIRENB(ID))
              AREAINF(4,ID) = 1.
              NZBAD = NZBAD + 1
            ENDIF
          ENDIF
        ENDDO
        IF (NZTOT-NZBAD.LE.2) GO TO 700
        CALL FITLOC(SXY,ZHIT,WTZ,NZTOT,ALZ,SG,ZG,VZG,VALZ,CHIDFZ)
        TGALZ=TAN(ALZ)
        THETA=1.570796-ALZ
      ENDIF
  700 IF (NZTOT-NZBAD.LT.MINZHIT) GOTO 1000 
C
C  Fill QTRAK,QHSEC
C
      IQTRAK(1)=0
      IF ( SIN_CORRECTION ) IQTRAK(1) = IBSET(IQTRAK(1),SIN_COR_BIT)
      IQTRAK(2)=NHSEC                                   
      IQTRAK(5)=NZTOT-NZBAD
      QTRAK(6)=AL
      QTRAK(7)=XG
      QTRAK(8)=YG
      QTRAK(12)=CHIDF*FLOAT(NHSEC-2)
      IF ( ( (RZMETHOD.EQ.0 .OR. ABS(ZVTX) .GT. 999.) .AND. 
     &        NZTOT-NZBAD .LT. NZGOOD                    ).OR.
     &        NZTOT-NZBAD.LE.2                                 ) THEN
        SINTHE = 1.
        QTRAK(9)=0.
        QTRAK(10)=0.
        QTRAK(11)=0.
        QTRAK(13)=0.
        QTRAK(18)=9.999
        QTRAK(19)=99.
      ELSE
        SINTHE = SIN(THETA)
C
C ****  Convert Z fit to D0 coords:  store Z @ (XG,YG) and calculate the
C ****  resultant change in the ZG variance as well as the new covariance term.
C
        ZG = ZG - SG*TGALZ
        FACTOR = SG/(SINTHE**2)
        VZG = VZG + VALZ*(FACTOR**2)
        VZGTHETA = VALZ*FACTOR
        QTRAK(9)=THETA
        QTRAK(10)=VZGTHETA
        QTRAK(11)=ZG
        QTRAK(13)=CHIDFZ*FLOAT(NZTOT-NZBAD-2)
        QTRAK(18)=SQRT(VALZ)
        QTRAK(19)=SQRT(VZG)
      END IF
C
C     The dE/dX and R_Z residual calculation will be based on the SINTHE
C     with the road center if the new R_Z strategy is used   
C
      IF (ABS(ZVTX) .LT. 999. ) THEN
        SINTHE = 1./SQRT(1.+ DZDR**2)
      ENDIF
      QTRAK(17)=SQRT(VG)
      QTRAK(16)=SQRT(VAL)
      CALL VXDEDX(AREAINF,SINTHE,NHSEC,MIP,MIPERR)
      QTRAK(20)=MIP                     ! Ionization
      QTRAK(21)=SINTHE
      QTRAK(14)= DZDR
      QTRAK(15)= ZVTX
C
C  Calculate fit residuals.
C  First for sense wire hits
C
      DO 400 IH=1,NHSEC
        QHSEC(3,IH)=(YHIT(IH)-YG)*COSAL-(XHIT(IH)-XG)*SINAL
        IF (WTZ(IH).LE.0. .OR. QTRAK(13).LE.0.) THEN
          QHSEC(4,IH)=0.
          GOTO 400                              
        ELSE
          QHSEC(4,IH)=ZHIT(IH)-ZG-SXY(IH)*TGALZ
        END IF
  400 CONTINUE
      CALL LDVTXT(QTRAK,QHSEC,LADDER)      ! Store track
c      CALL VUSDSG(LADDER)           ! tag the segments on track

 1000 RETURN
      END
