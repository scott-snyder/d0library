      SUBROUTINE ROADPH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set roads in phi and theta for test of the TRD 
C-                         routines
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-NOV-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:anglim.INC'
      INCLUDE 'D0$INC:tcntrl.INC'
      INTEGER TYPP
C----------------------------------------------------------------------
      TYPP=TYPPRO
      TYPPRO=0
      ntroad=0
      call tristr
      TYPPRO=TYPP
  999 RETURN
      END
