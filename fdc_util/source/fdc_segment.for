      SUBROUTINE FDC_SEGMENT(MODULE,ISEG,
     &  X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find slope and position of seg. in plane perp.
C-                      to wires, from a found segment. 
C-
C-   Inputs  : MODULE,ISEG = Module and segment number for that module
C-   Outputs : X_SLOPE = slope of seg. in plane perp. to wires.
C-             X_DRIFT = Drift distance of segment at wire 0, from wire.
C-             DL_found    = TRUE if delay line found
C-             OK    = TRUE if segment ok
C-
C-   Created   27-JUN-1990   Robert E. Avery   
C-   FROM: FSGXYZ.FOR BY 8-JUN-1990   Jeffrey Bantly
C-   Updated   7-DEC-1990   Robert E. Avery  Fix for x-sect segs (& clean up).  
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  21-JUL-1991   Robert E. Avery  Get info directly from
C-                      segment bank. (Much simplified).
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C Input:
      INTEGER MODULE,ISEG
C Output:
      REAL    X_SLOPE, X_DRIFT
      REAL    Y_DL
      LOGICAL DL_FOUND, OK
C Local:
      INTEGER IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER NHIT 
      REAL    XC,YC,ZC
      REAL    X_INTERCEPT 
      REAL    RADIUS
      REAL    STAGGER 
      REAL    FSTAGR
C
      INTEGER ICONT(62)
      REAL CONT(62)
      EQUIVALENCE(ICONT,CONT)
C
C----------------------------------------------------------------------
      X_SLOPE = 0.0
      X_DRIFT = 0.0
      Y_DL = 0.0
      OK=.FALSE.
      DL_FOUND = .FALSE.
C
      CALL GTFSEG(MODULE,ISEG,CONT)
      IADD = ICONT(2)
      NHIT = ICONT(3)
      IF(NHIT.LE.0) GOTO 999
C
      CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      CALL GTFALH(HALF,UNIT,QUAD,SECTOR,0,XC,YC,ZC)
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
        X_SLOPE = CONT(55)
        X_INTERCEPT = CONT(56)
        STAGGER = FSTAGR(HALF,1,0,0,0)
        X_DRIFT = (X_INTERCEPT-STAGGER) + X_SLOPE * ZC 
      ENDIF
      OK = .TRUE.
C
  999 RETURN
      END
