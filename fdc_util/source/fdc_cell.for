      SUBROUTINE FDC_CELL(
     &              HALF,UNIT,QDRT,SCTR,WIRE,
     &              X_TRK,Y_TRK ,DX_TRK ,DY_TRK,
     &              X_ROT,Y_ROT ,DX_ROT ,DY_ROT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a track in global FDC coordinates, return
C-   coordinates relative to center of cell in local cell coordinates.
C-      X_ROT,DX_ROT are in drift direction, 
C-      Y_ROT,DY_ROT are in orthogonal direction,
C-              (Delay line for Thetas, Radial for Phis)
C-
C-   Inputs  : HALF,UNIT,QDRT,SCTR,WIRE         ! which cell
C-             X_TRK,Y_TRK ,DX_TRK ,DY_TRK      ! track (FDC coords.)
C-   Outputs : X_ROT,Y_ROT ,DX_ROT ,DY_ROT      ! track (Local cell coords.)
C-   Controls: 
C-
C-   Created  23-AUG-1990   Robert E. Avery
C-   Updated  28-MAR-1991   Robert E. Avery, Track assumed to be defined by Z0
C-                                      (old version was Z_OFFSET = 120.) 
C-   Updated  27-JUN-1991   Robert E. Avery  For phi, Y_ROT is now distance 
C-                                 from FDC center in wire plane direction 
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-   Updated   7-MAY-1993   Robert E. Avery  Add call to FDC_ALIGNCHK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C Inputs  : 
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      REAL    X_TRK,Y_TRK ,DX_TRK ,DY_TRK
C Outputs : 
      REAL    X_ROT ,Y_ROT,DX_ROT ,DY_ROT 
C local: 
      INTEGER IADD
      INTEGER LFTRH, GZFTRH
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    X_TRK_C,Y_TRK_C  
      REAL    SDRIFT,CDRIFT
      REAL    Z0(0:1)
      REAL    XWIRE1,YWIRE1
      REAL    XWIRE2,YWIRE2
      REAL    RWIRE
      LOGICAL FALH_CHANGED 
      LOGICAL FDC_ALIGNCHK
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      FALH_CHANGED = FDC_ALIGNCHK()
      IF ( FIRST .OR. FALH_CHANGED ) THEN
        LFTRH = GZFTRH()
        IF ( LFTRH .GT. 0 ) THEN
          Z0(0)= Q(LFTRH+3)
          Z0(1)= Q(LFTRH+4)
          FIRST = .FALSE.
        ENDIF
        CALL GTFALH(
     &              HALF,1,0,0,0,
     &              XWIRE1,YWIRE1,ZWIRE)
        CALL GTFALH(
     &              HALF,1,0,18,0,
     &              XWIRE2,YWIRE2,ZWIRE)
        XWIRE=(XWIRE1-XWIRE2)/2.
        YWIRE=(YWIRE1-YWIRE2)/2.
        RWIRE= SQRT(XWIRE**2. + YWIRE**2.)
      END IF
C Position of wire:
      CALL GTFALH(
     &              HALF,UNIT,QDRT,SCTR,WIRE,
     &              XWIRE,YWIRE,ZWIRE)
      ZWIRE =  ZWIRE - Z0(HALF)
C
C position of track relative to center of cell (wire):
      X_TRK_C = (X_TRK  + DX_TRK * ZWIRE) - XWIRE 
      Y_TRK_C = (Y_TRK  + DY_TRK * ZWIRE) - YWIRE 
      IF ( UNIT.EQ.0 ) THEN
        CALL FDRIFTDIR(HALF,UNIT,QDRT,0,WIRE,SDRIFT,CDRIFT)
      ELSE
        CALL FDRIFTDIR(HALF,UNIT,QDRT,SCTR,WIRE,SDRIFT,CDRIFT)
      ENDIF
C
C In rotated cell coordinates (x=drift direction, y = othogonal)
      X_ROT =  X_TRK_C*CDRIFT + Y_TRK_C*SDRIFT
      Y_ROT = -X_TRK_C*SDRIFT + Y_TRK_C*CDRIFT
      DX_ROT =  DX_TRK*CDRIFT + DY_TRK*SDRIFT
      DY_ROT = -DX_TRK*SDRIFT + DY_TRK*CDRIFT
      IF ( UNIT.EQ.1 ) THEN             
        Y_ROT = -Y_ROT + RWIRE      !  define y_rot large as large radius
        DY_ROT = -DY_ROT 
      ENDIF
C
  999 CONTINUE
C-------------------------------------------------------------------------
      RETURN
      END
