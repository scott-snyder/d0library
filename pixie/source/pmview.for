      SUBROUTINE PMVIEW (IVIEW)
C======================================================================
C
C  Description:
C  ============
C  Draws the Muon Cosmic Ray Detector and current event
C
C  Input Arguements:
C  ==================
C  IVIEW - Keeps track of the present view..
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - October 27,1986
C
C=====================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
      INTEGER IVIEW,IVIEWY              ! IVIEW - the present view
      INTEGER FLAG
      REAL WXMIN,WXMAX
      REAL WYMIN,WYMAX
      DATA WXMIN,WXMAX,WYMIN,WYMAX/-1175.,1175.,-1175.,1175./      
      REAL VXMIN,VXMAX
      REAL VYMIN,VYMAX
      DATA VXMIN,VXMAX,VYMIN,VYMAX/-1.,.3,-.5,1./
C
C  Executable Code:
C  ================
C
      FLAG = 0
C
      IF (IVIEW .EQ. 1) THEN
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         CALL JVPORT(VXMIN,VXMAX,VYMIN,VYMAX)
         CALL PMAXES(IVIEW)
         CALL PMDDET(IVIEW)
         CALL PMEVNT(IVIEW,3)
      ENDIF
C
      IF (IVIEW .EQ. 2) THEN
         CALL JVPORT(VXMIN,VXMAX,VYMIN,VYMAX)
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         CALL PMAXES(IVIEW)
         CALL PMDDET(IVIEW)
         CALL PMEVNT(IVIEW,3)
      ENDIF
C
      IF (IVIEW.EQ.3) THEN
         CALL JVPORT(VXMIN,VXMAX,VYMIN,VYMAX)
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         CALL PMAXES(IVIEW)
         CALL PMDDET(IVIEW)
         CALL PMEVNT(IVIEW,3)
      ENDIF
C
      IF (IVIEW .EQ. 4) THEN
         FLAG = 1
         CALL JVPORT(-1.,0.,0.,1.)
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         IVIEWY = 1
         CALL PMAXES(IVIEWY)
         CALL PMDDET(IVIEWY)
         CALL PMEVNT(IVIEWY,3)
         CALL JVPORT(-1.,0.,-1.,0.)
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         IVIEWY = 2
         CALL PMAXES(IVIEWY)
         CALL PMDDET(IVIEWY)
         CALL PMEVNT(IVIEWY,3)
         CALL JVPORT(0.,1.,0.,1.)
         CALL JWINDO(WXMIN,WXMAX,WYMIN,WYMAX)
         IVIEWY = 3
         CALL PMAXES(IVIEWY)
         CALL PMDDET(IVIEWY)
         CALL PMEVNT(IVIEWY,3)
      ENDIF
C
      RETURN
      END
