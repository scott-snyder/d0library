      SUBROUTINE MNWLST(IPDT,NCELL,SCINT_POS,SCINT_CEN,TOF_WLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get TOF on WLS fiber on muon scintillator
C-
C-   Inputs  : IPDT             ! Module number
C-             NCELL            ! cell id
C-             SCINT_POS(3)     ! X,Y,Z cor. of track on scint.
C-                                in global system
C-             SCINT_CEN(3)     ! X,Y,Z cor. of center of scint.
C-                                in global system
C-   Outputs : TOF_WLS          ! Time of flight on WLS fiber
C-   Controls: None
C-
C-   Created  26-OCT-1994   shashi
C-   Modified 11-NOV-1994   R. Markeloff.  Rename from WLSTOF to MNWLST
C-   Modified 21-DEC-1994   R. Markeloff.  TOF_WLS is now always positive
C-   Modified 17-JUL-1995   Tim McMahon    Added cell # to arguments and
C-                                         call to MSCT_CORR
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INTEGER IPDT,N_OCT,NCELL
      REAL    SCINT_POS(3), SCINT_CEN(3), TOF_WLS, DIST, RI
      REAL    TSHIFT
      DATA RI / 2.16 /            ! Refractive index of fiber (nominally 1.6)
C
      N_OCT = (IPDT-200) - ((IPDT-200)/10)*10
      IF (N_OCT .EQ. 0 .OR. N_OCT .EQ. 3 .OR. N_OCT .EQ. 4 .OR. N_OCT
     &  .EQ. 7) THEN
        DIST = ABS(SCINT_CEN(2)-SCINT_POS(2))
      ELSE
        DIST = ABS(SCINT_CEN(1)-SCINT_POS(1))
      ENDIF
      TOF_WLS = (RI*DIST)/29.979              ! Nanoseconds
C
C ___ correction for time variations across counter
C
      CALL MSCT_CORR(IPDT,NCELL,SCINT_POS,SCINT_CEN,TSHIFT)
      TOF_WLS = TOF_WLS - TSHIFT
  999 RETURN
      END
