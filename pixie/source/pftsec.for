      SUBROUTINE PFTSEC(LFDTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws a sector of the Theta display
C-                         and its tracks if parameter set
C-
C-   Inputs  : LFDTX  - STP bank location for geometry constants
C-   Outputs :
C-
C-   Created  21-FEB-1989   Lupe Rosas & Jeffrey Bantly
C-   Updated  10-JAN-1990   Lupe Howell Implementing Color Table
C-   Updated   7-FEB-1990   Jeffrey Bantly  cleanup temp fixes
C-   Updated   6-AUG-1991   Robert E. Avery  Correct theta type used,
C-                              resulting simplifications.
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
C
      INTEGER HALF, QUAD, SECTOR
      INTEGER ILAB, DRDELH
      INTEGER FDC_QUADTYPE
      INTEGER IER
      INTEGER XOFFSET, YOFFSET 
      REAL    XC, YC, DX, DY
      REAL ANG ! Angle in degrees of theta unit wrt X-axis
      CHARACTER*4 SECCLR
      LOGICAL EZERROR
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTSEC','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'FDC HALF', HALF)
      CALL PUGETV( 'FDC QUAD', QUAD)
      CALL PUGETV( 'FDC SECT', SECTOR)
      CALL PUGETV( 'FDC DRAW LABEL', ILAB)
      CALL PUGETV( 'FDC DRAW DELAY', DRDELH)
      CALL PUGETA( 'FDC COLR SECTOR',SECCLR )
      IF(QUAD.LE.3) THEN
        ANG = -45. + QUAD*90.
      ELSE
        ANG = -90. + (QUAD-4)*90. 
      ENDIF
      CALL PXCOLR(SECCLR )
      IF (FDC_QUADTYPE(QUAD,HALF) .EQ. 1) THEN
        XOFFSET = 2
        YOFFSET = 3
      ELSE
        XOFFSET = 3
        YOFFSET = 2
      ENDIF
C
      XC = C(LFDTX+9+9*SECTOR+XOFFSET+3)                  
      DX = C(LFDTX+9+9*SECTOR+XOFFSET)                    
      YC = C(LFDTX+9+9*SECTOR+YOFFSET+3)                  
      DY = C(LFDTX+9+9*SECTOR+YOFFSET)                    
C
      CALL JPINTR(0)
      CALL PFRECT( SECCLR, XC, YC, 0., DX, DY ,ANG )
      IF(ILAB.GT.0)THEN         ! Labeling sectors
        CALL PFSECL(XC,YC,ANG)
      ENDIF
      IF( N_DL_HITS(HALF,QUAD,SECTOR) .GE. 1 ) THEN
        IF(DRDELH.GT.0) THEN      ! Draw delay line hit
          CALL PFDELH
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  990 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
