      SUBROUTINE FIT_TRK_T0(HALF,LADDER,
     &  QTRAK,IQTRAK,QHSEC,IQHSEC,CHINORM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : refit FDC track with one extra degree of freedom,
C-   an overall t0.
C-
C-   Inputs  :  HALF,LADDER
C-
C    Note: Structure of track bank is expanded, in order to 
C    allow space for t0. 
C    The words stored in FDCT for these tracks are:
C       +1 to +25  Same as standard FDCT (zee FDCT.ZEB)
C       +26        t0 from fit (ns)
C       +27        error on par(5), t0
C       +28 TO 32  Covariance of t0 w. parameters 1 to 5
C                  (i.e. C(1,5) ... C(5,5) )
C
C-   Created   1-NOV-1990   Robert E. Avery
C-   Updated   1-NOV-1993   Robert E. Avery   Change FDCT structure.
C-                              (more consistant with standard).
C-   Updated  18-NOV-1993   Robert E. Avery  Fix incorrectly dimensioned
C-                              (I)QTRAK arrays. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER MAX_PARAM
      PARAMETER( MAX_PARAM = 5 )
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 34 )
C INPUT
      INTEGER HALF
      INTEGER LADDER(0:2)
C OUTPUT
      INTEGER IQTRAK(*),IQHSEC(3,34)
      REAL QTRAK(*),QHSEC(3,34)
      REAL CHINORM
C LOCAL
      INTEGER FIT_POINT, IPOINT
      INTEGER IPAR1,IPAR2
      INTEGER IFAIL
      INTEGER INDEX(MAX_PARAM)
      INTEGER NEL,NWORDS,LWIRE
      INTEGER IER
      INTEGER LSECT,LSECTM1,LSECTP1
      INTEGER HITADD,WIRENB(32)
      INTEGER UNIT,QUAD,SECTOR,UB,MODULE,LAYER
      INTEGER ITOT,NTOT,I,IHIT,IHITD,SECTD
      INTEGER ID,WIRE,LR,LRWIR,NWLAY,LVRFT
      INTEGER ICALL,IWIRE,IH
      INTEGER IBSET,WIREBTS ,WL
      REAL Z0(2)
      REAL CONT(54)
      REAL FSTAGR,STAGGER
      REAL XWIRE(MAX_POINTS),YWIRE(MAX_POINTS),ZWIRE(MAX_POINTS)
      REAL SINE(MAX_POINTS),COSINE(MAX_POINTS),WT(MAX_POINTS)
      REAL XC,YC,ZC,XCD,YCD,ZCD
      REAL SDRIFT,CDRIFT,PHI,THETA,DRDZ,WNORM
      REAL AA(MAX_POINTS,MAX_PARAM),BB(MAX_POINTS),DIST(MAX_POINTS)
      REAL RESID
      REAL PARAM(MAX_PARAM),PARAM_ERR(MAX_PARAM)
      REAL ETZERO, ATZERO, VELOP, VELOM
      REAL CHISQ
      REAL DTDX,DTDY,ERR_THETA
      REAL DPDX,DPDY,ERR_PHI
      REAL DENOM 
C
      REAL*8 COV(MAX_PARAM,MAX_PARAM),DET,SUM(MAX_PARAM),PAR
C
      REAL QHIT(18)
      INTEGER IQHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
C
      REAL FSECTD,FIADD,FNHITS
      INTEGER ISECTD,IADD,NHITS
      EQUIVALENCE (ISECTD,FSECTD),(IADD,FIADD),(NHITS,FNHITS)
C
      SAVE ICALL,Z0
      DATA ICALL/0/
C----------------------------------------------------------------------

      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Loop through segments on ladder and get hits assigned to them.
C
      IPOINT=0
      DO LAYER=0,2
C
C  Need three layer track.
        IF (LADDER(LAYER).EQ.0) GO TO 999
C
C  Get contents of segment in LAYER
C
        MODULE=LAYER+3*HALF
        CALL GTFSEG(MODULE,LADDER(LAYER),CONT)
        FSECTD=CONT(1)
        FIADD=CONT(2)
        FNHITS=CONT(3)                    ! number of hits in segment
        CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IHITD=ISECTD/1000
        IF(ABS(IHITD).NE.0) THEN
          SECTD=SECTOR+(ISECTD/ABS(ISECTD))
        ENDIF
C
        CALL FGTLTM( HALF,UNIT,QUAD,SECTOR,WIRE,
     &               ETZERO,ATZERO,VELOP,VELOM)
C
C  Loop through hits on segment and get their coordinates
C
        CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
        DO ID=1,NHITS
          LRWIR=CONT(3+ID)
          IF( (ID .GE. ABS(IHITD)) .AND.
     &        (IHITD .NE. 0) ) THEN
            SECTOR = SECTD
            CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,0,0,2)
            CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
          ENDIF
          IF (UNIT.LE.0) THEN
            IHIT=CONT(11+ID)
            CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          ELSE
            IHIT=CONT(19+ID)
            CALL GTFPSC(HALF,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          END IF
          HITADD=IADD*2+LRWIR
          WIRE=LRWIR/2
          LR=LRWIR-WIRE*2
          IPOINT=IPOINT+1
          SINE(IPOINT)=SDRIFT
          COSINE(IPOINT)=CDRIFT
          STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
          DIST(IPOINT)=QHIT(2+LR)-STAGGER
          IF ((QHIT(5) .GT. 0.0000001) .AND. (QHIT(5) .LE. 9999.)) THEN
            WT(IPOINT)=1./QHIT(5)**2
          ELSE
            WT(IPOINT)=0.05                 ! default in case of QHIT(5)=0
          ENDIF
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
          IF(ZC.EQ. 0.0) GOTO 999
          XWIRE(IPOINT) = XC
          YWIRE(IPOINT) = YC 
          ZWIRE(IPOINT) = ZC-Z0(HALF+1)
          IQHSEC(1,IPOINT) = HITADD
          IQHSEC(2,IPOINT) = IHIT
C
          AA(IPOINT,1) = COSINE(IPOINT)
          AA(IPOINT,2) = SINE(IPOINT)
          AA(IPOINT,3) = ZWIRE(IPOINT)*COSINE(IPOINT)
          AA(IPOINT,4) = ZWIRE(IPOINT)*SINE(IPOINT)
          AA(IPOINT,5)=  0.5 * (-1)**LR * VELOP / 10000.
C
C          IF ( (UNIT.EQ.0) .AND. (SECTOR.EQ.1) ) THEN
C            AA(IPOINT,5)= -0.5 * (-1)**LR * VELOP / 10000.
C          ELSE
C          ENDIF
C
          BB(IPOINT)   = -XWIRE(IPOINT)*COSINE(IPOINT)
     &                   -YWIRE(IPOINT)*SINE(IPOINT)
          IF ( IPOINT .EQ. 1 ) THEN
            WNORM = WT(IPOINT )
          ENDIF
          WT(IPOINT) = WT(IPOINT)/WNORM ! to avoid big numbers
          IF ( LAYER.EQ.0 ) THEN
            WL = 7 - WIRE
          ELSEIF( LAYER.EQ.1 ) THEN
            WL = 24 + WIRE
          ELSE
            WL = 8 + WIRE
          ENDIF
          WIREBTS = IBSET(WIREBTS,WL)
C
c (don't include delay line)
        ENDDO
      ENDDO
      NTOT=IPOINT
C
      CALL VZERO(SUM,2*MAX_PARAM)
      CALL VZERO(COV,2*MAX_PARAM*MAX_PARAM)
      DO IPOINT=1,NTOT
        DO IPAR1=1,MAX_PARAM
          SUM(IPAR1)=SUM(IPAR1)
     &         + AA(IPOINT,IPAR1)
     &         *(DIST(IPOINT)-BB(IPOINT) )*WT(IPOINT)
        ENDDO
        DO IPAR1=1,MAX_PARAM
          DO IPAR2=IPAR1,MAX_PARAM
            COV(IPAR1,IPAR2)=COV(IPAR1,IPAR2)
     &            + AA(IPOINT,IPAR1)*AA(IPOINT,IPAR2)*WT(IPOINT)
            COV(IPAR2,IPAR1)=COV(IPAR1,IPAR2)
          ENDDO
        ENDDO
      ENDDO
C
      CALL DINV(MAX_PARAM,COV,MAX_PARAM,INDEX,IFAIL)  
C
      DO IPAR1=1,MAX_PARAM
        PAR = 0
        DO IPAR2=1,MAX_PARAM
          PAR = PAR + COV(IPAR1,IPAR2)*SUM(IPAR2)
          COV(IPAR1,IPAR2) = COV(IPAR1,IPAR2)/WNORM
        ENDDO
        PARAM(IPAR1) = PAR
        PARAM_ERR(IPAR1) = SQRT(ABS(COV(IPAR1,IPAR1)))
      ENDDO
C
      CHISQ=0
      DO IPOINT = 1,NTOT
        RESID = DIST(IPOINT)-BB(IPOINT) 
        DO IPAR1=1,MAX_PARAM
          RESID = RESID
     &      - AA(IPOINT,IPAR1)*PARAM(IPAR1)
        ENDDO
        QHSEC(3,IPOINT) = RESID
        CHISQ=CHISQ + WT(IPOINT) * (RESID**2.)
      ENDDO
      CHISQ = CHISQ * WNORM
      CHINORM = SQRT(2*CHISQ) - SQRT(2*(NTOT-MAX_PARAM)-1.)
C
C  Phi
C
      PHI = ATAN2(PARAM(4),PARAM(3))-PI*FLOAT(HALF-1)
      IF (PHI.LT.0.) PHI = PHI+TWOPI
      IF (PHI.GT.TWOPI) PHI = PHI-TWOPI
C
C Error on phi
C
      DENOM = 1.+(PARAM(4)/PARAM(3))**2
      DPDX = (-PARAM(4)/PARAM(3)**2) / DENOM 
      DPDY = (1./PARAM(3)) / DENOM 
      ERR_PHI = DPDX**2 * COV(3,3) +
     &          DPDY**2 * COV(4,4) +
     &          2.*DPDX*DPDY * COV(3,4)
      IF (ERR_PHI.LT.0.) ERR_PHI = 0.
      ERR_PHI = SQRT(ERR_PHI)
C
C Theta
C
      DRDZ = SQRT(PARAM(3)**2 + PARAM(4)**2)
      THETA = ATAN(DRDZ)
      IF (HALF.EQ.0) THETA = PI-THETA
      IF (THETA.GT.PI) THETA = TWOPI-THETA
      IF (THETA.LT.0.0) THETA = ABS(THETA)
C
C Error on theta
C
      DTDX = PARAM(3)/(DRDZ*(1.+DRDZ**2))
      DTDY = PARAM(4)/(DRDZ*(1.+DRDZ**2))
      ERR_THETA = DTDX**2 * COV(3,3) +
     &            DTDY**2 * COV(4,4) +
     &            2.*DTDX*DTDY * COV(3,4)
      IF (ERR_THETA.LT.0.) ERR_THETA = 0.
      ERR_THETA = SQRT(ERR_THETA)
C
      IQTRAK(1) = HALF
      IQTRAK(2) = NTOT                    ! number of hits on track
      IQTRAK(3) = WIREBTS 
      QTRAK(4)  = PARAM(1)                   ! x0
      QTRAK(5)  = PARAM(2)                   ! y0
      QTRAK(6)  = PHI                        ! phi angle
      QTRAK(7)  = PARAM(3)                   ! dx/dz
      QTRAK(8)  = PARAM(4)                   ! dy/dz
      QTRAK(9)  = COV(1,1)
      QTRAK(10) = COV(1,2)
      QTRAK(11) = COV(1,3)
      QTRAK(12) = COV(1,4)
      QTRAK(13) = COV(2,2)
      QTRAK(14) = COV(2,3)
      QTRAK(15) = COV(2,4)
      QTRAK(16) = COV(3,3)
      QTRAK(17) = COV(3,4)
      QTRAK(18) = COV(4,4)
      QTRAK(19) = CHISQ
      QTRAK(20) = 0.                      ! Calculated later in FDEDX
      QTRAK(21) = 0.                      ! Calculated later in FDEDX
      QTRAK(22) = THETA                   ! theta angle
      QTRAK(23) = ERR_PHI
      QTRAK(24) = ERR_THETA
      IQTRAK(25)= NTOT                    ! number of hits on track
      QTRAK(26) = 0.5 * PARAM(5)          ! t0 shift for this track
      QTRAK(27) = 0.5 * PARAM_ERR(5)      ! error in the above
      QTRAK(28) = COV(1,5)                ! rest of covariance matrix
      QTRAK(29) = COV(2,5)
      QTRAK(30) = COV(3,5)
      QTRAK(31) = COV(4,5)
      QTRAK(32) = COV(5,5)
C
  999 RETURN
      END
