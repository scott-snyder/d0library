      SUBROUTINE PDXYTK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display X-Y view of the  CDC tracks
C-                       If draw  CDC only (no other detectors), the track
C-                       is extrapolated to the center of the detector
C-                       The inner and outer radii of the  CDC detector are drawn
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  24-JAN-1991 S. HAGOPIAN
C-            based on PDXYVW by Qizhong Li-Demarteau
C-   Updated   6-MAR-1991   Lupe Howell  Implementing PIXIE using COMAPACK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL IFDCDC
      REAL    EXTRAP
      REAL     CDCEXT, STDEXT
      PARAMETER(  CDCEXT = 55.0 )
      PARAMETER( STDEXT = 0.0 )
      INTEGER  CDCSEC,IER
      REAL RMIN, RMAX
      LOGICAL EZERROR
C
      DATA RMIN,RMAX/51.8,71.9/
C----------------------------------------------------------------------
      CALL PUGETV('CDC DRAW SECTORS', CDCSEC)
      CALL PUSETV('CDC DRAW SECTORS',0)
      CALL PUGETV('CDC ONLY', IFDCDC )
C
      IF ( IFDCDC ) THEN
        EXTRAP =  CDCEXT
      ELSE
        EXTRAP = STDEXT
      ENDIF
C
      CALL PDTRCK(0,31,EXTRAP)
C
C   draw the center of the detector
C
      CALL PUOPEN
      CALL JMOVE(-2.5,0.)
      CALL JDRAW(2.5,0.)
      CALL JMOVE(0.,-2.5)
      CALL JDRAW(0.,2.5)
      CALL PXCOLR('GRE')
      CALL JCIRCL(0.,0.,0.,RMIN,0)
      CALL JCIRCL(0.,0.,0.,RMAX,0)
      CALL PUCLOSE
      CALL PUSETV('CDC DRAW SECTORS', CDCSEC)
  999 RETURN
      END
