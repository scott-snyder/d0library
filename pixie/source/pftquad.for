      SUBROUTINE PFTQUAD(LFDTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws a Outline of Theta quadrant 
C-                         and its DL hits (if parameter set).
C-
C-   Inputs  : LFDTX  - STP bank location for geometry constants
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   6-AUG-1991   Robert E. Avery
C-   Updated   7-OCT-1991   Robert E. Avery  Use new FDDELP 
C-   Updated  30-MAR-1992   Robert E. Avery  Change  color name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDDELP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C  INPUT:
      INTEGER LFDTX
C  LOCAL:
      INTEGER HALF, QUAD, SECTOR
      INTEGER POINT
      INTEGER DRDELH
      INTEGER IER
      INTEGER XOFFSET, YOFFSET 
      INTEGER FDC_QUADTYPE
      INTEGER ILAB
C
      REAL    XC, YC, DX, DY, ANG
C
      INTEGER NPOINT 
      PARAMETER( NPOINT = 14 )
      REAL    UR(NPOINT),UL(NPOINT),V(NPOINT)
      REAL    UU(NPOINT),VV(NPOINT)
C
      CHARACTER*4 FDCCLR, TITCLR
      LOGICAL EZERROR
      LOGICAL FDC_ONLY
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFQUAD','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'FDC HALF', HALF)
      CALL PUGETV( 'FDC QUAD', QUAD)
      CALL PUGETV( 'FDC DRAW DELAY', DRDELH)
      CALL PUGETV( 'FDC DRAW LABEL', ILAB)
      CALL PUGETV( 'FDC ONLY', FDC_ONLY)
      CALL PUGETA( 'FDC COLR BOUNDARY',FDCCLR )
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      IF(QUAD.LE.3) THEN
        ANG = -PI/4. + QUAD*HALFPI
      ELSE
        ANG = -HALFPI + (QUAD-4)*HALFPI
      ENDIF
      CALL PXCOLR(FDCCLR )
      IF(FDC_QUADTYPE(QUAD,HALF) .EQ. 1) THEN
        XOFFSET = 2
        YOFFSET = 3
      ELSE
        XOFFSET = 3
        YOFFSET = 2
      ENDIF
C
C Accumulate Poly-line of quadrant outline:
C
      YC = C(LFDTX+9+YOFFSET+3)
      DY = C(LFDTX+9+YOFFSET)
      POINT = 1
      UR(POINT) = 0
      UL(POINT) = 0
      V(POINT) = YC-DY
C
      DO SECTOR =  0, MXSECT
        XC = C(LFDTX+9+9*SECTOR+XOFFSET+3)                  
        DX = C(LFDTX+9+9*SECTOR+XOFFSET)                    
        YC = C(LFDTX+9+9*SECTOR+YOFFSET+3)                  
        DY = C(LFDTX+9+9*SECTOR+YOFFSET)                    
C
        POINT = POINT + 1
        UR(POINT)=XC+DX
        UL(POINT)=-UR(POINT)
        V(POINT)=V(POINT-1)
C
        POINT = POINT + 1
        UR(POINT)=UR(POINT-1)
        UL(POINT)=-UR(POINT)
        V(POINT)=YC+DY
C
      ENDDO
C
      POINT = NPOINT
      UR(POINT)=XC
      UL(POINT)=UR(POINT)
      V(POINT)=V(POINT-1)
C
C  rotate the coordinate system by ANG
C
      DO POINT=1,NPOINT
        UU(POINT)=UR(POINT)*COS(ANG)-V(POINT)*SIN(ANG)
        VV(POINT)=UR(POINT)*SIN(ANG)+V(POINT)*COS(ANG)
      ENDDO
      CALL JMOVE(UU(1),VV(1))
      CALL JPOLY(UU,VV,NPOINT)
C
      DO POINT=1,NPOINT
        UU(POINT)=UL(POINT)*COS(ANG)-V(POINT)*SIN(ANG)
        VV(POINT)=UL(POINT)*SIN(ANG)+V(POINT)*COS(ANG)
      ENDDO
      CALL JMOVE(UU(1),VV(1))
      CALL JPOLY(UU,VV,NPOINT)
C
C  Draw delay line hits
C
      IF(DRDELH.GT.0) THEN      
        DO SECTOR =  0, MXSECT
          CALL PUSETV( 'FDC SECT', SECTOR)
          IF( N_DL_HITS(HALF,QUAD,SECTOR) .GE. 1 ) THEN
            CALL PFDELH
          ENDIF
        ENDDO
      ENDIF
      CALL EZRSET
C
C  Draw Quadrant Labels:
C
      IF ( FDC_ONLY ) THEN
        IF(ILAB.GT.0)THEN         ! Labeling sectors
          CALL PXCOLR(TITCLR)
          CALL PFQUADL(UU(NPOINT),VV(NPOINT),QUAD)
        ENDIF
      ENDIF
C
  999 RETURN
      END
