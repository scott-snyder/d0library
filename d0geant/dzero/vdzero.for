      SUBROUTINE VDZERO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prepares 3 views of the detector
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:  (Data statements control viewing parameters)
C-
C-   Created  20-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
C----------------------------------------------------------------------
C     view number one, general view of the SPECTROMETER and it's
C     longitudinal X-Z VIEW
C
      CALL SLSRCP('SRCP_REST')    !Get the viewing parameters from this bank
      CALL SETVEW('VDZERO_VIEW1')
      CALL SETVEW('VDZERO_VIEW2')
      CALL SETVEW('VDZERO_VIEW3')
C
C
  999 RETURN
      END
