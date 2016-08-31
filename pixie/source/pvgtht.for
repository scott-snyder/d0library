      SUBROUTINE PVGTHT(LAYER,SECTOR,NTOT,YCEN,YERR,ZCEN,ZERR,POS)
C------------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Find coordinates y,z of hits in current cell (LAYER,SECTOR) in VTX.
C-
C-   Inputs  : LAYER, SECTOR : location in VTX
C-   Outputs : NTOT = number of y-z hits in sector (includes lr ambiguity)
C-             YCEN, YERR = arrays containing y position and errors of hits
C-             ZCEN, ZERR = arrays containing z position and errors of hits
C-             POS = array containing information on type of hit:
C-                 POS = -1 : normal hit
C-                 POS = 0  : unmatched hit (data from one wire end only)
C-                 POS = 1  : hit used in vertex finding
C-
C-
C-   Created  10-MAY-1989 S. Hagopian Based on VPOINT by  D.Zieminska
C-   Modified 17-DEC-1989 P. Grudberg - handle unmatched hits
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INTEGER LAYER, SECTOR
      INTEGER NTOT,POS(*)
      REAL YCEN(*),YERR(*),ZCEN(*),ZERR(*)
      INTEGER NWIRES,NBSENS,NLAYER,NEL,NWORDS
      PARAMETER (NWIRES=7)      ! maximum wire# (counting from 0)
      PARAMETER (NBSENS=8)      ! number of sense wires
      PARAMETER (NLAYER=2)      ! maximum layer# (counting from 0)
      REAL CONT(18)
      INTEGER STAT, MASK
      PARAMETER (MASK=3)
      INTEGER NHITS(0:NWIRES), IPTR(0:NWIRES), MXHSEC
      PARAMETER (MXHSEC=200)
      INTEGER  I1, LR
      REAL Y0MAX(0:2), XWIRE(0:NLAYER,0:NWIRES)
      INTEGER  FIRST, LAST, STEP
      INTEGER  WIRE, IHIT
      INTEGER  LVALS, GZVALS, IPAL, WIR1, WIR2
      REAL     DRIFT, XHIT, YHIT
      INTEGER I0, IPAL0, I7, IPAL7
      REAL IMPACT, TOL, XHIT0, YHIT0, XHIT7, YHIT7, Y0, SLOPE
C
      REAL QHIT(18*MXHSEC)
      INTEGER IQHIT(18*MXHSEC)
      EQUIVALENCE ( QHIT, IQHIT )
      DATA TOL/0.1/
      CHARACTER*4 PATH
C-------------------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      CALL GTVSEC(LAYER, SECTOR, 'WIR', 0, NEL, NWORDS, CONT)
      CALL UCOPY(CONT, NHITS, NEL)
      CALL UCOPY(CONT(NEL+1), IPTR, NEL)
      CALL GTVSEC(LAYER, SECTOR, 'ALL',  IPTR(0), NEL, NWORDS, QHIT)
      NTOT = 0
      WIR1 = 0
      WIR2 = NWIRES
C      IF ( NHITS(WIR1) .EQ. 0 .OR. NHITS(WIR2) .EQ. 0 ) GO TO 1000
C      IF ( NHITS(WIR1) .NE. NHITS(WIR2) ) GO TO 1000
      DO 200 WIRE = 0, NWIRES
        IF( NHITS(WIRE) .EQ. 0) GO TO 200
        DO 300 LR = 1, 0, -1
          IF ( LR .EQ. 0) THEN
            FIRST = 1
            LAST = NHITS(WIRE)
            STEP = 1
          ELSE
            FIRST = NHITS(WIRE)
            LAST = 1
            STEP = -1
          END IF
          DO 400 IHIT= FIRST, LAST, STEP
            I1= IPTR(WIRE) - 2*NEL - 4 + NWORDS*(IHIT-1)
            LVALS = GZVALS(LAYER, SECTOR)
            IPAL = LVALS + 6 + IC(LVALS+6)*WIRE
            DRIFT = QHIT(I1+2+LR)-(C(LC(LVGEH-3)+31+WIRE))*(-1.)**SECTOR
            XHIT = C(IPAL+1) + DRIFT*C(LVALS+3) ! hit coordinates in D0 frame
            YHIT = C(IPAL+2) + DRIFT*C(LVALS+4)
            NTOT = NTOT + 1
            POS(NTOT) = -1
            YCEN(NTOT)= YHIT
            YERR(NTOT)= QHIT(I1+5)! DY
            ZCEN(NTOT)= QHIT(I1+4)            ! z
            ZERR(NTOT)= QHIT(I1+6) ! DZ
            STAT = iand(IQHIT(I1+10), MASK)
            IF ( STAT .NE. 3 .AND. PATH .EQ. 'RECO' ) THEN ! unmatched hit
              POS(NTOT) = 0
              GO TO 400
            ENDIF
            IF ( NHITS(WIR1) .EQ.  NHITS(WIR2)  ) THEN
              IF (WIRE .EQ. 0 .OR. WIRE .EQ. NWIRES) THEN
                IF ( WIRE .EQ. NWIRES ) THEN
                  I0 = IPTR(0) - 2*NEL - 4 + NWORDS*(IHIT-1)
                  IPAL0 = LVALS + 6
                  DRIFT =QHIT(I0+2+LR)-(C(LC(LVGEH-3)+31))*(-1.)**SECTOR
                  XHIT0 = C(IPAL0+1) + DRIFT*C(LVALS+3)
                  YHIT0 = C(IPAL0+2) + DRIFT*C(LVALS+4)
                  SLOPE = (YHIT - YHIT0)/(XHIT - XHIT0)
                  Y0 = (YHIT + YHIT0 - (XHIT + XHIT0)*SLOPE)/2.
                ELSE IF ( WIRE .EQ. 0 ) THEN
                  I7 = IPTR(7) - 2*NEL - 4 + NWORDS*(IHIT-1)
                  IPAL7 = LVALS + 6 + IC(LVALS+6)*7
                  DRIFT =QHIT(I7+2+LR)-(C(LC(LVGEH-3)+38))*(-1.)**SECTOR
                  XHIT7 = C(IPAL7+1) + DRIFT*C(LVALS+3)
                  YHIT7 = C(IPAL7+2) + DRIFT*C(LVALS+4)
                  SLOPE = (YHIT7 - YHIT)/(XHIT7 - XHIT)
                  Y0 = (YHIT7 + YHIT - (XHIT7 + XHIT)*SLOPE)/2.
                END IF
                IMPACT= ABS(Y0)/SQRT(1. + SLOPE**2)
                IF ( IMPACT .LT. TOL) THEN
                  POS(NTOT) = 1
                END IF
              ENDIF
            ENDIF
  400     CONTINUE
  300   CONTINUE
  200 CONTINUE
 1000 RETURN
      END
