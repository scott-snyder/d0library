      SUBROUTINE FDC_SEG_XSECT(MODULE,ISEG,
     &  X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find slope and position of seg. in plane perp.
C-                      to wires, from a found segment. If segment is 
C-                      a cross sector segment, this will be in the 
C-                      local coordinates of the LAST sector 
C-                      (unlike FDC_SEGMENT).
C-
C-   Inputs  : MODULE,ISEG = Module and segment number for that module
C-   Outputs : X_SLOPE = slope of seg. in plane perp. to wires.
C-             X_DRIFT = Drift distance of segment at wire 0
C-             DL_found    = TRUE if delay line found
C-             OK    = TRUE if segment ok
C-
C-   Created  11-JUN-1991   Robert E. Avery   
C-              FROM: FDC_SEGMENT
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  21-JUL-1991   Robert E. Avery  Get info directly from
C-                      segment bank. (Much simplified).
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  27-NOV-1991   Robert E. Avery  Add checks for bad Phi X-sect
C-                                              segments.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C Input:
      INTEGER MODULE,ISEG
C Output:
      REAL    X_SLOPE
      REAL    X_DRIFT
      REAL    Y_DL
      LOGICAL DL_FOUND
      LOGICAL OK
C Local:
      INTEGER IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER SECTOR_NEW
      INTEGER LFXSC 
      INTEGER IPTR_SC 
      INTEGER STATUS 
      INTEGER XSECT_HIT
      INTEGER NHIT 
      INTEGER LR
      INTEGER FIRST_HIT,HIT,THIS_HIT
      REAL    XC,YC,ZC
      REAL    X_INTERCEPT 
      REAL    RADIUS
      REAL    Z(2),STAGGER(2),DRIFTD(2)
      REAL    FSTAGR
      REAL    RESIDUAL
C
      INTEGER ICONT(62)
      REAL CONT(62)
      EQUIVALENCE(ICONT,CONT)
C
C Functions:
      INTEGER GZFXSC
C----------------------------------------------------------------------
      X_SLOPE = 0.0
      X_DRIFT = 0.0
      Y_DL = 0.0
      OK=.FALSE.
      DL_FOUND = .FALSE.
C
      CALL GTFSEG(MODULE,ISEG,CONT)
      XSECT_HIT = ICONT(1)/1000
      IADD = ICONT(2)
      NHIT = ICONT(3)
      IF(NHIT.LE.0) GOTO 999
C
      CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      IF ( ABS(XSECT_HIT) .GT. 0 ) THEN
        SECTOR_NEW = SECTOR + SIGN(1,XSECT_HIT)
        FIRST_HIT = ABS(XSECT_HIT)
      ELSE
        SECTOR_NEW = SECTOR 
      ENDIF
      CALL GTFALH(HALF,UNIT,QUAD,SECTOR_NEW,0,XC,YC,ZC)
C
      IF ( UNIT .EQ. 0 ) THEN
        X_SLOPE = CONT(30)
        X_INTERCEPT = CONT(31)
        RADIUS=((XC)**2. + (YC)**2.)**.5
        X_DRIFT = (X_INTERCEPT-RADIUS) + X_SLOPE * ZC 
        IF ( CONT(36).LT. 9999. ) THEN  ! Get DL position
          Y_DL = CONT(35)
          DL_FOUND = .TRUE.
        ENDIF
      ELSE
C
C  For Phi x-sect have to do it the old way:
C
        IF ( ABS(XSECT_HIT) .GE. NHIT ) GOTO 999
        IF ( ABS(XSECT_HIT) .GT. 0 ) THEN
          LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR_NEW)
          DO HIT = 1,2
            THIS_HIT = FIRST_HIT+HIT-1
            WIRE=CONT(3+THIS_HIT)/2.
            LR=CONT(3+THIS_HIT)-2*WIRE
            IPTR_SC = LFXSC + CONT(19+THIS_HIT)        ! POINTER in FXSC
            RESIDUAL=CONT(38+THIS_HIT)
            STAGGER(HIT)=FSTAGR(HALF,UNIT,QUAD,SECTOR_NEW,WIRE)
            DRIFTD(HIT)=Q(IPTR_SC+2+LR)-STAGGER(HIT)-RESIDUAL 
            CALL GTFALH(HALF,UNIT,QUAD,SECTOR_NEW,WIRE,XC,YC,ZC)
            Z(HIT)=ZC
          ENDDO
          IF ( Z(1).EQ.Z(2) ) GOTO 999
          X_SLOPE = ( (DRIFTD(2)+STAGGER(2)) - (DRIFTD(1)+STAGGER(1)) ) 
     &                          / (Z(2) - Z(1))
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR_NEW,0,XC,YC,ZC)
          X_DRIFT = DRIFTD(1) + X_SLOPE * (ZC - Z(1) )
        ELSE
          X_SLOPE = CONT(55)
          X_INTERCEPT = CONT(56)
          STAGGER(1) = FSTAGR(HALF,1,0,0,0)
          X_DRIFT = (X_INTERCEPT-STAGGER(1)) + X_SLOPE * ZC 
        ENDIF
      ENDIF
      OK = .TRUE.
C
C----------------------------------------------------------------------
  999 RETURN
      END
