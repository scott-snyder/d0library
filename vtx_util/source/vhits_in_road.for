      SUBROUTINE VHITS_IN_ROAD(ZVTX,PH1,PH2,TH1,TH2,
     &                         NW,NH1,NHW1,NH2,NHW2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Count up VTX hits in road defined in calling
C-           arguments.  Also count up total number of wires that intersect
C-           road.
C-           
C-           This routine WILL NOT WORK ON DST -- need STA or RAW data.
C-
C-   Inputs  : ZVTX    -- z-pos'n of primary vertex position used road def'n
C-             PH1,PH2 -- phi limits of road
C-             TH1,TH2 -- theta limits of road
C-   Outputs : NW  -- Total number of wire-layers intersected by road (max=24)
C-             NH1 -- Number of r-phi hits in the road
C-             NHW1-- Number of wires with r-phi hits
C-             NH2 -- Number of r-z hits in road (using standard VTX def'n)
C-             NHW2-- Number of wires with r-phi hits
C-   Controls: 
C-
C-   Created  18-FEB-1994   Ed Oltman
C-   Updated  22-MAR-1994   Ed Oltman  BUG FIX: CHECK IF VSEC EXISTS
C-   Updated  25-JAN-1995   Ed Oltman  based on version in D0$PROD area (e.g.
C-                            generation 4 in CMS area) . Strip out calls to
C-                            (optional) booking and filling of VRHT user bank
C-                            o restore VALS,VRFT after hitfinding (16-jun-94)
C-                            o fix phi road width problem (10-aug-1994)
C-                           This version will be generation 7 in CMS and will
C-                           (hopefully) go into the feb 95 pass release of
C-                           FULL_D0RECO
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$INC:VTXLNK.INC'
c I/O:
      REAL    ZVTX,PH1,PH2,TH1,TH2
      INTEGER NW,NH1,NHW1,NH2,NHW2
c Locals:
      LOGICAL FIRST,GOODZ,DONE,UNPACK(0:2),SOMETHING
      REAL    DZDR1,DZDR2,DZDR,R,X,Y,SSIGN,DELPHI,DZTOL0,DZTOL1
      REAL    PHI,ZPRED,PH11,THETA,CELL_SIZE(0:2),EPS1,EPS2,DIST1,DIST2
      REAL    PHIV(2),DEL(2),DELPH,DRFT,DZ,PI2,X0,Y0,DX0,DY0
      REAL    COSPH1,COSPH2,SINPH1,SINPH2
      INTEGER LAY,SEC,WIR,NSEC(0:2),LVALS,IPAL,NHV,NPV,NWV,ERR,ID
      INTEGER IH,PT,LR,LVCHT,ISTAT,LCDH1,LVRFT,RUN,EVT,HITS(0:23,3)
      INTEGER RUN_LAST,EVT_LAST
c Externals:
      INTEGER GZVSEC,GZVALS,GZVCHT,GZVTXH,GZCDH1,GZVRFT
      LOGICAL VTX_BAD_SECTOR
c Data:
      DATA      NSEC/15,31,31/
      DATA      FIRST/.TRUE./
      DATA      RUN_LAST/-1/,EVT_LAST/-1/
C----------------------------------------------------------------------
      CALL EVNTID(RUN,EVT)
      IF (RUN .NE. RUN_LAST .OR. EVT .NE. EVT_LAST) THEN
        CALL VTX_DYNADJ
        RUN_LAST = RUN
        EVT_LAST = EVT
      ENDIF
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('DZTOL0',DZTOL0,ERR)
        CALL EZGET('DZTOL1',DZTOL1,ERR)
        CALL EZRSET
        PI2 = TWOPI
        DO LAY = 0,2
          R = C(LC(LVGEH-3)+7+7*LAY) + C(LC(LVGEH-3)+23)
          PHI = C(LC(LVGEH-3)+6+7*LAY)*RADIAN
          CELL_SIZE(LAY) = R*TAN(PHI)
        ENDDO
      ENDIF
      DZDR1 = COS(TH1)/SIN(TH1)
      DZDR2 = COS(TH2)/SIN(TH2)
      THETA = 0.5*(TH1+TH2)
      DZDR = COS(THETA)/SIN(THETA)
      NW  = 0
      NH1 = 0
      NHW1= 0
      NH2 = 0
      NHW2= 0
C
C ****  Figure out what wires are hit based on ZVTX, theta limits
C
      SOMETHING = .FALSE.
      LVRFT = GZVRFT()
      DO LAY = 0,2
        DO WIR = 0,7
          R = C(LVRFT+7+7*LAY) + C(LVRFT+23+WIR)
          UNPACK(LAY) =  
     &      ( ABS(ZVTX+DZDR2*R) .LT. C(LVGEH+17+2*LAY) ) .OR.
     &      ( ABS(ZVTX+DZDR1*R) .LT. C(LVGEH+17+2*LAY) )
        ENDDO
        SOMETHING = SOMETHING .OR. UNPACK(LAY)
      ENDDO
      IF (.NOT. SOMETHING) GO TO 999
C
C ****  Compute PHI center and PHI width (Assume PHI width < pi!)
C
      IF (ABS(PH1-PH2) .LT. PI) THEN
        PH11 = PH1
      ELSE
        PH11 = PH1 - SIGN(PI2,PH1-PH2)
      ENDIF
      PHI = 0.5*(PH11+PH2)
      IF (PHI .LT. 0.) PHI = PHI + TWOPI
      DELPHI = 0.5*ABS(PH11-PH2)
C
C ****  Loop over all hit banks  which intersect the road
C
C
      COSPH1 = COS(PH1)
      SINPH1 = SIN(PH1)
      COSPH2 = COS(PH2)
      SINPH2 = SIN(PH2)
      CALL VXY_BEAM1(ZVTX,X0,DX0,Y0,DY0,ISTAT)
      CALL VZERO(HITS(0,1),3*24)
      DO LAY = 0,2
        DO SEC = 0,NSEC(LAY)
          IF (.NOT. UNPACK(LAY)) GO TO 50
          IF ( VTX_BAD_SECTOR(LAY,SEC)) GO TO 50
C
C ****  See if PHI road intersects WIRE 0 of the sector -- EPS is the COS of 
C ****  the angle between the road edge and the sector's drift plane -- if |EPS|
C ****  is greater then .5 then the track is more then 30 degrees from being
C ****  radial (maximum cell half width = 11.25 deg)  DIST is the drift distance
C ****  that that corresponds to the road edge -- if its larger then the cell,
C ****  the road edge is outside the cell.  As long as the beam center is less 
C ****  then 7.4 mm from the D0 coordinate center, this test is sufficient.
C
          LVALS = GZVALS(LAY,SEC)
          IF (C(LVALS+7)*COSPH1+C(LVALS+8)*SINPH1 .LT. 0.) GO TO 50
          EPS1 = C(LVALS+3)*COSPH1 + C(LVALS+4)*SINPH1
          EPS2 = C(LVALS+3)*COSPH2 + C(LVALS+4)*SINPH2
          IF (ABS(EPS2) .GT. .5 .AND. ABS(EPS2) .GT. .5) GO TO 50
          X = C(LVALS+7) - X0
          Y = C(LVALS+8) - Y0
          SSIGN = (-1.)**SEC
          DIST1 = (EPS1*COSPH1-C(LVALS+3))*X+(EPS1*SINPH1-C(LVALS+4))*Y
          DIST1 = DIST1/(1.-EPS1**2) + C(LC(LVGEH-3)+31)*SSIGN
          DIST2 = (EPS2*COSPH2-C(LVALS+3))*X+(EPS2*SINPH2-C(LVALS+4))*Y
          DIST2 = DIST2/(1.-EPS2**2) + C(LC(LVGEH-3)+31)*SSIGN
          IF ( (ABS(DIST1) .GT. CELL_SIZE(LAY)) .AND. 
     &         (ABS(DIST2) .GT. CELL_SIZE(LAY))  ) GO TO 50
C
C ****  The road intersects this sector:  If VSEC does not exist, must build 
C ****  it from CDD1 or CDH1 or VCHT
C
          LVSEC(SEC,LAY) = GZVSEC(LAY,SEC)
          IF (LVSEC(SEC,LAY) .EQ. 0) THEN
            LVTXH = GZVTXH()
            LVCHT = GZVCHT()
            IF (LVCHT .GT. 0) THEN  
              CALL VCHT_UNPACK(LAY,SEC,ISTAT)
              IF (ISTAT .EQ. 0) CALL VHTCHK(LAY,SEC,1,DONE)
            ELSE
              IF (LVTXH .EQ. 0) CALL BKVTXH
              LCDD1 = LQ(LHEAD-IZCDD1)
              LCDH1 = GZCDH1()
              IF (LCDD1 .GT. 0 .OR. LCDH1 .GT. 0) THEN
                CALL VSECHT(LAY,SEC)
              ELSE
                CALL ERRMSG('No VTX hit banks','VHITS_IN_ROAD',
     &            'VSEC does not exits; cannot be built','W')
                GOTO 999
              ENDIF
            ENDIF
            LVRFT = GZVRFT()
            LVALS = GZVALS(LAY,SEC)
          ENDIF
C
C ****  Loop over all wires and hits on each wire
C
          DO WIR = 0,7
            R = C(LVRFT+7+7*LAY) + C(LVRFT+23+WIR)
            IF ( ( ABS(ZVTX+DZDR2*R) .LT. C(LVGEH+17+2*LAY) ) .OR.
     &           ( ABS(ZVTX+DZDR1*R) .LT. C(LVGEH+17+2*LAY) )) THEN
              ID = 8*LAY + WIR
              HITS(ID,3) = HITS(ID,3) + 1
              IF (LVSEC(SEC,LAY) .EQ. 0) GO TO 40
              IPAL = LVALS+6+IC(LVALS+6)*WIR
              NHV = IQ(LVSEC(SEC,LAY)+4+WIR)
              NPV = IQ(LVSEC(SEC,LAY)+12+WIR)
              NWV = IQ(LVSEC(SEC,LAY)+3)
              DO IH = 1,NHV
                GOODZ = .FALSE.
                PT = LVSEC(SEC,LAY) + NPV + NWV*(IH-1)
                DO LR = 1,2
                  DRFT = Q(PT+LR) - C( LC(LVGEH-3)+31+WIR )*SSIGN
                  X = C(IPAL+1) + DRFT*C(LVALS+3)
                  Y = C(IPAL+2) + DRFT*C(LVALS+4)
                  R = SQRT( X**2 + Y**2 )
                  ZPRED = ZVTX + R*DZDR
                  X = X + C(IPAL+4)*ZPRED + C(IPAL+6)*ZPRED**2 - X0
                  Y = Y + C(IPAL+5)*ZPRED + C(IPAL+7)*ZPRED**2 - Y0
                  PHIV(LR) = ATAN2(Y,X)
                  IF (PHIV(LR) .LT. 0.) PHIV(LR) = PHIV(LR) + TWOPI
                ENDDO
                IF (IBITS( IQ(PT+9) , 0, 2) .EQ. 3) THEN
                  DZ = ABS(ZPRED - Q(PT+3))
                  GOODZ = (DZ .LT. DZTOL1).and.(DZ .LT. DZTOL0*Q(PT+5))
                ENDIF
                DO LR = 1,2
                  DEL(LR) = ABS(PHIV(LR) - PHI)
                  IF (DEL(LR) .GT. PI) DEL(LR) = ABS(DEL(LR)-TWOPI)
                ENDDO
                DELPH = AMIN1(DEL(1),DEL(2))
                IF (  DELPH .LT. DELPHI ) THEN
                  HITS(ID,1) = HITS(ID,1) + 1
                  IF (GOODZ) HITS(ID,2) = HITS(ID,2) + 1
                ENDIF
              ENDDO           ! LOOP OVER HITS ON WIRE
   40         CONTINUE        ! SECTOR HAS HITS
            ENDIF             ! ROAD INTERSECTS WIRE-LAYER
          ENDDO               ! LOOP OVER WIRE
   50     CONTINUE            ! ROAD INTESECTS SECTOR
        ENDDO                 ! LOOP OVER SECTOR
      ENDDO                   ! LOOP OVER LAYER
C
C ****  COMPUTE HIT SUMS
C
  100 CONTINUE
      DO IH = 0,23
        NH1 = NH1 + HITS(IH,1)
        NH2 = NH2 + HITS(IH,2)
        IF (HITS(IH,1) .GT. 0) NHW1 = NHW1 + 1
        IF (HITS(IH,2) .GT. 0) NHW2 = NHW2 + 1
        IF (HITS(IH,3) .GT. 0) NW = NW + 1
      ENDDO
  999 RETURN
      END
