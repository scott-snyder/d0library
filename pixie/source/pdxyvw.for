      SUBROUTINE PDXYVW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display X-Y view of the CDC
C-                       If draw CDC only (no other detectors), the track
C-                       is extrapolated to the center of the detector
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  27-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL IFDCDC
      REAL    EXTRAP
      REAL    CDCEXT, STDEXT
      PARAMETER( CDCEXT = 55.0 )
      PARAMETER( STDEXT = 0.0 )
C----------------------------------------------------------------------
      CALL PUGETV('CDC ONLY', IFDCDC )
      IF ( IFDCDC ) THEN
        EXTRAP = CDCEXT
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
      CALL PUCLOSE
  999 RETURN
      END
