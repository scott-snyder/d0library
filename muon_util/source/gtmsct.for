      SUBROUTINE GTMSCT( NMSCT, IADD, IFLAG, IMUOT, IHIT, TOF, TXYZ,
     &  XYZ, DXYZ, WLS_TIME )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : NMSCT  : scintillator hit number
C-   Outputs : IADD   : scintillator address ( Chamber_id*256 + cell_id)
C-             IFLAG  : quality flag
C-                           bit 0 : 1st P.M. latch missing
C-                           bit 1 : 2nd P.M. latch missing
C-                           bit 2 : 1st P.M. pedestal
C-                           bit 3 : 2nd P.M. pedestal
C-                           bit 4 : 1st P.M. non-physical
C-                           bit 5 : 2nd P.M. non-physical
C-             IMOUT  : matched track number at MUOT
C-             IHIT   : MUHP pointer
C-             TOF    : time of flight (nsec)
C-             TXYZ   : track position at scintillator
C-             XYZ    : scintillator center postion (global)
C-             DXYZ   : half width of scintillator  (global)
C-             WLS_TIME: wavelength shifter delay
C-   Controls: None
C-
C-   Created   8-FEB-1994   Atsushi Taketani
C-   Modified  23-MAY-1992  Vipin Bhatnagar! Take care of crashing
C-    IP.GT. changed to IP.GE.
C-   Modified  19-DEC-1994  R. Markeloff. Added IHIT argument
C-   Modified   1-Mar-1995  R. Markeloff. Now returns WLS delay
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- Arguments
      INTEGER NMSCT, IADD, IFLAG, IMUOT, IHIT
      REAL   TOF, TXYZ(3), XYZ(3), DXYZ(3), WLS_TIME
C-- includes
      INCLUDE 'D0$INC:ZEBCOM.INC'
C_- local
      INTEGER LMSCT, GZMSCT, IP, I
C----------------------------------------------------------------------
      LMSCT = GZMSCT(0)                   ! exist MSCT?
      IF ( LMSCT.EQ.0 ) GOTO 998
C
      IP = ( NMSCT - 1) * 20              ! size MSCT?
      IF ( IP.GE.IQ(LMSCT-1) ) GOTO 998
C
      LMSCT = LMSCT + IP
      IADD  = IQ(LMSCT+1)                 ! scintillator hit address
      IFLAG = IQ(LMSCT+2)                 ! quality flag
      IMUOT = IQ(LMSCT+3)                 ! MUOT track number
      IHIT  = IQ(LMSCT+4)                 ! MUHP hit number
      TOF   = Q(LMSCT+9)                  ! Time of flight
      DO I=1,3
        TXYZ(I) = Q(LMSCT+ 9+I)           ! track position
        XYZ(I)  = Q(LMSCT+14+I)           ! scintillator center
        DXYZ(I) = Q(LMSCT+17+I)           ! half width
      END DO
      WLS_TIME = Q(LMSCT+13)
C
  999 RETURN
C
  998 IADD = 0
      RETURN
C
      END
