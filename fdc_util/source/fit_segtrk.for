      SUBROUTINE FIT_SEGTRK(HALF,LADDER,QTRAK,IQTRAK,CHINORM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute fit parameters and Chisq for
C-   FDC track composed of LADDER. Fit is based on FDC segment fit results,
C-   as opposed to FDC hits. Up to 8 measurements make up the fit,
C-   the segment position in the drift direction at the first and last 
C-   wire of each sector, and the delay line position for each theta
C-   sector (if the delay line measurement exists).
C-
C    Input  : HALF        = FDC Half of track
C             LADDER(0:2) = segments in layers (0:2) on track candidate
C                          layers are: 0 - inner Theta chamber
C                                      1 - outer Theta chamber
C                                      2 - Phi chamber
C    Output : QTRAK(*),IQTRAK(*) = Track info from fit 
C                               (only 2,4,5,7,8,19 filled)
C             CHINORM     = Normalized Chi-square from track fit
C-
C-   Created  29-AUG-1991   Robert E. Avery, based on FTFDCT.
C-   Updated  27-NOV-1991   Robert E. Avery  For tracks with X-Sector PHI
C-                              segments, treat two parts of segment as 
C-                              two segments (only for two layer tracks).
C-   Updated   2-JUN-1992   Susan K. Blessing  Remove a useless line.
C-    Move section of code outside a loop to speed up.
C-   Updated  21-JUL-1992   Susan K. Blessing  QTRAK(23) and (24) must be
C-    filled in for FDC_MISS_SEG_CHK.
C-   Updated  24-AUG-1994   Susan K. Blessing  Call GTFALH in ICALL=1 loop
C-    and store information on wire center locations rather than calling
C-    it within the loops.
C-    Replace the double precision calculation with a single precision one.
C-    Redefine WT_NORM to be 1/WT_NORM.
C-   Updated  25-AUG-1994   Srini Rajagopalan  Remove calls to GTFSEG,GTFPSC
C-                          and GTFTSC, Replace with local zebra lookup.
C-   Updated  28-SEP-1994   Susan K. Blessing   Check that PAR(3) is not
C-    equal to zero before doing error calculation.  Probably never came
C-    up before because calculation used to be double precision.
C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C
C  Input:
      INTEGER HALF,LADDER(0:2)
C
C  Output:
      INTEGER IQTRAK(*)
      REAL QTRAK(*)
      REAL CHINORM
C
C  Local:
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 10 )
      INTEGER I,J
      INTEGER ICALL
      INTEGER INDEX(4)
      INTEGER IPAR1,IPAR2
      INTEGER UNIT,QUAD,SECTOR,UB,MODULE,LAYER
      INTEGER ITOT,NTOT
      INTEGER ID,WIRE
      INTEGER IFAIL,IER
      INTEGER IADD,NHIT,SECTD
      INTEGER MAX_WIRE
      INTEGER XSECT_HIT, SECTOR_NEW 
      INTEGER H,U,QU,S
      INTEGER MQUAD,MSEC
      INTEGER LSEGM,LOC
      INTEGER GZFSEG,LZFIND
C
      REAL Z0(2)
      REAL XWIRE(MAX_POINTS),YWIRE(MAX_POINTS),ZWIRE(MAX_POINTS)
      REAL SINE(MAX_POINTS),COSINE(MAX_POINTS),WT(MAX_POINTS)
      REAL RESID(MAX_POINTS)
      REAL XC,YC,ZC,ZC_WMAX
      REAL SDRIFT,CDRIFT,PAR(4)
      REAL AA(MAX_POINTS,4),BB(MAX_POINTS),DIST(MAX_POINTS)
      REAL CHIMAX,CHISQ
      REAL WTDIST,WTAA,SUMAA
      REAL X_SLOPE, X_DRIFT, X_INTERCEPT
      REAL STAGGER, FSTAGR
      REAL RADIUS
      REAL Y_DL
      REAL COV(4,4),SUM(4)
C
      REAL WT_NORM, WEIGHT 
      REAL WT_PHI, WT_THETA, WT_DL
      REAL SWT_WGHT, SWP_WGHT, DL_WGHT
      REAL FDC_ERROR_SLOPE
C
      REAL ERR_HIT,ERR_DL
      REAL ERR_PHI,DPDX,DPDY
      REAL ERR_THETA,DTDX,DTDY,DRDZ
C
      REAL XCA(0:MXHALF,0:MXUNIT,0:MXQUAD,0:MXSECP,0:1)
      REAL YCA(0:MXHALF,0:MXUNIT,0:MXQUAD,0:MXSECP,0:1)
      REAL ZCA(0:MXHALF,0:MXUNIT,0:MXQUAD,0:MXSECP,0:1)
C
C
      LOGICAL FIT_NO_DL
      LOGICAL FIT_NO_DL_DUMMY
      LOGICAL FIT_3L_NO_DL
      LOGICAL THREE_LAYER
      LOGICAL DL_FOUND,OK
      LOGICAL PHI_XSECT  
C
      SAVE ICALL,Z0,FIT_NO_DL, FIT_3L_NO_DL
      SAVE WT_PHI, WT_THETA, WT_DL, WT_NORM
C
      DATA ICALL/0/
      DATA FIT_NO_DL/.FALSE./
      DATA FIT_3L_NO_DL/.TRUE./
      DATA WT_THETA /2.0/
      DATA WT_PHI /4.0/
      DATA WT_DL /0.5/
C
C------------------------------------------------------------------------
C
      IF (ICALL.EQ.0) THEN
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_rarr('Z0',Z0,IER)
        CALL EZGET_l('FIT_NO_DL',FIT_NO_DL,IER)
        CALL EZGET_l('FIT_3L_NO_DL',FIT_3L_NO_DL,IER)
        CALL EZGET('WT_THETA_SEGTRK', WT_THETA,IER)
        CALL EZGET('WT_PHI_SEGTRK', WT_PHI,IER)
        CALL EZGET('WT_DL_SEGTRK', WT_DL,IER)
        CALL EZRSET
C
C  Weights are normalized so that values are not Extreme.
C
        WT_NORM = FDC_ERROR_SLOPE(0.0,0)**2   
C                                                 
        WT_THETA = WT_THETA * WT_NORM
        WT_PHI = WT_PHI * WT_NORM
        WT_DL = WT_DL * WT_NORM
C
C Fill an array of wire center positions for first and second to last wires
        DO H = 0, MXHALF
          DO U = 0, MXUNIT
            IF (U.EQ.0) THEN
              MQUAD = MXQUAD
              MSEC = MXSECT
              MAX_WIRE = 6
            ELSE
              MQUAD = 0
              MSEC = MXSECP
              MAX_WIRE = 14
            END IF
            DO QU = 0, MQUAD
              DO S = 0, MSEC
C
C Wire 0                
                CALL GTFALH(H,U,QU,S,0,XC,YC,ZC)
                XCA(H,U,QU,S,0) = XC
                YCA(H,U,QU,S,0) = YC
                ZCA(H,U,QU,S,0) = ZC
C
C Second to last wire
                CALL GTFALH(H,U,QU,S,MAX_WIRE,XC,YC,ZC)
                XCA(H,U,QU,S,1) = XC
                YCA(H,U,QU,S,1) = YC
                ZCA(H,U,QU,S,1) = ZC
              END DO
            END DO
          END DO
        END DO
C
        ICALL = 1
      END IF
C
      ITOT = 0
      CHISQ = 0.
C
      DO I = 1, 4
        PAR(I) = 0.
        SUM(I) = 0.
        COV(I,I) = 0.
        DO J = I+1, 4
          COV(I,J) = 0.
          COV(J,I) = 0.
        END DO
      END DO
C
C  Loop through segments on ladder and get hits assigned to them.
C
      THREE_LAYER = (LADDER(0)*LADDER(1)*LADDER(2)) .GT. 0
      PHI_XSECT = .FALSE.
      DO LAYER = 0,2
        IF (LADDER(LAYER).NE.0) THEN
C
C  Get contents of segment in LAYER
C
          MODULE = LAYER+3*HALF
          LSEGM = GZFSEG(HALF,LAYER)
          LOC = LZFIND(IXCOM,LSEGM,LADDER(LAYER),-5)
          SECTD = IQ(LOC+1)
          IADD = IQ(LOC+2)
          NHIT = IQ(LOC+3)
C
          CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
          XC = XCA(HALF,UNIT,QUAD,SECTOR,0)
          YC = YCA(HALF,UNIT,QUAD,SECTOR,0)
          ZC = ZCA(HALF,UNIT,QUAD,SECTOR,0)
C
          IF ( UNIT .EQ. 0 ) THEN
            CALL FDRIFTDIR(HALF,UNIT,QUAD,0,WIRE,SDRIFT,CDRIFT)
            X_SLOPE = Q(LOC+30)
            X_INTERCEPT = Q(LOC+31)
            ERR_HIT = Q(LOC+34)
            RADIUS = ((XC)**2. + (YC)**2.)**.5
            X_DRIFT = (X_INTERCEPT-RADIUS) + X_SLOPE  * ZC
            WEIGHT = WT_THETA / ERR_HIT**2. 
            MAX_WIRE = 6
C
          ELSE
            CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
            X_SLOPE = Q(LOC+55)
            X_INTERCEPT = Q(LOC+56)
            ERR_HIT = Q(LOC+59)
            STAGGER = FSTAGR(HALF,1,0,0,0)
            X_DRIFT = (X_INTERCEPT-STAGGER) + X_SLOPE * ZC
            WEIGHT = WT_PHI / ERR_HIT**2. 
            MAX_WIRE = 14
            IF ( .NOT.THREE_LAYER ) THEN
              XSECT_HIT = SECTD/1000
              IF ( ABS(XSECT_HIT).GT. 0) THEN
                WEIGHT = WEIGHT / 2.
                PHI_XSECT = .TRUE.
              ENDIF
            ENDIF
          ENDIF
C
C At wire 0:
C
          ITOT = ITOT+1
          SINE(ITOT) = SDRIFT
          COSINE(ITOT) = CDRIFT
          DIST(ITOT) = X_DRIFT
          WT(ITOT) = WEIGHT
          XWIRE(ITOT) = XC
          YWIRE(ITOT) = YC
          ZWIRE(ITOT) = ZC-Z0(HALF+1)
C
C At wire MAX_WIRE:
C
          XC = XCA(HALF,UNIT,QUAD,SECTOR,1)
          YC = YCA(HALF,UNIT,QUAD,SECTOR,1)
          ZC_WMAX = ZCA(HALF,UNIT,QUAD,SECTOR,1)
C
          ITOT = ITOT+1
          SINE(ITOT) = SDRIFT
          COSINE(ITOT) = CDRIFT
          DIST(ITOT) = X_DRIFT + X_SLOPE*(ZC_WMAX-ZC)
          WT(ITOT) = WEIGHT
          XWIRE(ITOT) = XC
          YWIRE(ITOT) = YC
          ZWIRE(ITOT) = ZC_WMAX-Z0(HALF+1)

C
C Delay line (if wanted)
C
          IF ( ( .NOT. FIT_NO_DL )
     &      .AND. (.NOT. (FIT_3L_NO_DL.AND.THREE_LAYER) ) ) THEN
            IF (UNIT.EQ.0) THEN
              ERR_DL = Q(LOC+36)
              IF ( ERR_DL .LT. 9999. ) THEN   ! Good DL hit
                ITOT = ITOT+1
                SINE(ITOT) = CDRIFT
                COSINE(ITOT) = -SDRIFT
                DIST(ITOT) = Q(LOC+35)
                WT(ITOT) = WT_DL / ERR_DL**2
                XWIRE(ITOT) = XC
                YWIRE(ITOT) = YC
                ZWIRE(ITOT) = ZC-Z0(HALF+1)
              ENDIF
            ENDIF
          ENDIF
C
C Phi X-SECT segment, treat as second segment.
C
          IF ( PHI_XSECT ) THEN
c
            chinorm = -9999.
            if (chinorm.eq.-9999.) go to 999
c            
            SECTOR_NEW = SECTOR + SIGN(1,XSECT_HIT)
            CALL FDC_SEG_XSECT(MODULE,LADDER(LAYER),
     &        X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
            IF ( OK ) THEN
              XC = XCA(HALF,UNIT,QUAD,SECTOR_NEW,0)
              YC = YCA(HALF,UNIT,QUAD,SECTOR_NEW,0)
              ZC = ZCA(HALF,UNIT,QUAD,SECTOR_NEW,0)
              CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR_NEW,WIRE,
     &                       SDRIFT,CDRIFT)
C
C At wire 0:
C
              ITOT = ITOT+1
              SINE(ITOT) = SDRIFT
              COSINE(ITOT) = CDRIFT
              DIST(ITOT) = X_DRIFT
              WT(ITOT) = WEIGHT
              XWIRE(ITOT) = XC
              YWIRE(ITOT) = YC
              ZWIRE(ITOT) = ZC-Z0(HALF+1)
C
C At wire MAX_WIRE:
C
              XC = XCA(HALF,UNIT,QUAD,SECTOR_NEW,1)
              YC = YCA(HALF,UNIT,QUAD,SECTOR_NEW,1)
              ZC_WMAX = ZCA(HALF,UNIT,QUAD,SECTOR_NEW,1)
C
              ITOT = ITOT+1
              SINE(ITOT) = SDRIFT
              COSINE(ITOT) = CDRIFT
              DIST(ITOT) = X_DRIFT + X_SLOPE*(ZC_WMAX-ZC)
              WT(ITOT) = WEIGHT
              XWIRE(ITOT) = XC
              YWIRE(ITOT) = YC
              ZWIRE(ITOT) = ZC_WMAX-Z0(HALF+1)
            ENDIF
          ENDIF
C
        ENDIF
      ENDDO
      NTOT = ITOT
C
      DO ITOT = 1,NTOT
        AA(ITOT,1) = COSINE(ITOT)
        AA(ITOT,2) = SINE(ITOT)
C Divide by 100 to encourage numbers to be closer together.
        AA(ITOT,3) = ZWIRE(ITOT)*COSINE(ITOT)/100.
        AA(ITOT,4) = ZWIRE(ITOT)*SINE(ITOT)/100.
        BB(ITOT)  = -XWIRE(ITOT)*COSINE(ITOT)-YWIRE(ITOT)*SINE(ITOT)
        WTDIST = (DIST(ITOT)-BB(ITOT))*WT(ITOT)
        DO IPAR1 = 1,4
          SUM(IPAR1) = SUM(IPAR1)+ AA(ITOT,IPAR1)*WTDIST
        ENDDO
C
C  Calculate inverse of covariance matrix
C
        DO IPAR1 = 1,4
          WTAA = AA(ITOT,IPAR1)*WT(ITOT)
          DO IPAR2 = IPAR1,4
            COV(IPAR1,IPAR2) = COV(IPAR1,IPAR2) + AA(ITOT,IPAR2)*WTAA
          ENDDO
        ENDDO
      ENDDO
C
      DO IPAR1 = 1,4
        DO IPAR2 = IPAR1+1,4
          COV(IPAR2,IPAR1) = COV(IPAR1,IPAR2)
        ENDDO
      ENDDO
C
C  Invert covariance matrix:
C
      CALL RINV(4,COV,4,INDEX,IFAIL)
      DO IPAR1 = 1,4
        DO IPAR2 = 1,4
          PAR(IPAR1) = PAR(IPAR1)+COV(IPAR1,IPAR2)*SUM(IPAR2)
          COV(IPAR1,IPAR2) = COV(IPAR1,IPAR2) * WT_NORM
        ENDDO
      ENDDO
C
C  Calculate fit residuals.
C
      DO ITOT = 1,NTOT
        RESID(ITOT) = DIST(ITOT)-BB(ITOT)
        SUMAA = 0.0
        DO IPAR1 = 1,4
          SUMAA = SUMAA + AA(ITOT,IPAR1)*PAR(IPAR1)
        ENDDO
        RESID(ITOT) = RESID(ITOT)- SUMAA
        CHISQ = CHISQ+RESID(ITOT)**2*WT(ITOT)
      ENDDO
C
C Restore normalization on parameters involving ZWIRE
      PAR(3) = PAR(3)/100.
      PAR(4) = PAR(4)/100.
C
C Make sure PAR(3) is not zero.
      IF (PAR(3).EQ.0.) PAR(3) = .0001
C
C Error on phi
      DPDX = 1./(1.+(PAR(4)/PAR(3))**2) * (-1.*PAR(4)/PAR(3)**2)
      DPDY = 1./(1.+(PAR(4)/PAR(3))**2) * (1./PAR(3))
      ERR_PHI = DPDX**2 * COV(3,3) +
     &          DPDY**2 * COV(4,4) +
     &          2.*DPDX*DPDY * COV(3,4)
      IF (ERR_PHI.LT.0.) ERR_PHI = 0.
      ERR_PHI = SQRT(ERR_PHI)
C
      DRDZ = SQRT(PAR(3)**2 + PAR(4)**2)
C
C Error on theta
C
      DTDX = PAR(3)/(DRDZ*(1.+DRDZ**2))
      DTDY = PAR(4)/(DRDZ*(1.+DRDZ**2))
      ERR_THETA = DTDX**2 * COV(3,3) +
     &            DTDY**2 * COV(4,4) +
     &            2.*DTDX*DTDY * COV(3,4)
      IF (ERR_THETA.LT.0.) ERR_THETA = 0.
      ERR_THETA = SQRT(ERR_THETA)
C
C  Fill QTRAK (only what is needed)
C
      IQTRAK(2) = NTOT
      QTRAK(4) = PAR(1)                   ! x0
      QTRAK(5) = PAR(2)                   ! y0
      QTRAK(7) = PAR(3)                   ! dx/dz
      QTRAK(8) = PAR(4)                   ! dy/dz
      QTRAK(19) = CHISQ/WT_NORM
      QTRAK(23) = ERR_PHI
      QTRAK(24) = ERR_THETA
      IF ( NTOT .GT. 4 ) THEN
        CHINORM = SQRT(2*QTRAK(19)) - SQRT(2*(FLOAT(NTOT)-4.)-1.)
      ELSE
        CHINORM = -10.
      ENDIF
C
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      ENTRY FIT_SEGTRK_NO_DL(FIT_NO_DL_DUMMY)
      FIT_NO_DL = FIT_NO_DL_DUMMY
      RETURN
      END
