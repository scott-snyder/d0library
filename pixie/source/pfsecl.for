      SUBROUTINE PFSECL(XC,YC,ANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw label in the sector of a Theta display
C-
C-   Inputs  : XC     - center x value of current box
C-             YC     - center y value of current box
C-             ANG    - Angle Theta chamber is rotated, rel. to phi=0.
C-   Outputs : draws labels
C-   Controls:
C-
C-   Created  21-FEB-1989   Lupe Rosas
C-   Updated   2-MAY-1989   Jeffrey Bantly  eliminate passed variables
C-   Updated   6-AUG-1991   Robert E. Avery  Correct theta type used,
C-                              resulting simplifications.
C-   Updated  23-MAR-2004   compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      include 'd0$inc:pi.def'
      REAL ANG
      real angr
      REAL FXSIZE, XC, YC, U, V
      REAL PRCENT,YXRATI
      REAL XBASE, YBASE 
      INTEGER QUAD, SECTOR
      CHARACTER*1 LAB(0:7)
      CHARACTER*3 TEXT
      INTEGER IER
      LOGICAL EZERROR
      DATA LAB /'0','1','2','3','4','5','6','7'/
      DATA PRCENT,YXRATI,FXSIZE  /0.75,1.5,1.0/
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFSECL','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'FDC QUAD', QUAD)
      CALL PUGETV( 'FDC SECT', SECTOR)
      CALL EZRSET
C
      CALL JJUST(2,2)
      angr = ang*pi/180
      U= XC*COS(ANGr)-YC*SIN(ANGr)
      V= XC*SIN(ANGr)+YC*COS(ANGr)
C
      IF ( QUAD .LE. 3 ) THEN
        XBASE = 1.0
        YBASE = - SIGN(1.0,U*V)
      ELSE
        XBASE = 1.0
        YBASE = 0.0
      ENDIF
      CALL JBASE(XBASE,YBASE,0.)
      CALL JPLANE(0.,1.,0.)
C
      TEXT = LAB(QUAD)//' '//LAB(SECTOR)
      CALL PUVSTR(U,V,PRCENT,YXRATI,TEXT)
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
