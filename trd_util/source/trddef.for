      SUBROUTINE TRDDEF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines the inputs to common GCCONST
C-                         Purpose: readability of the program
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-OCT-1989   J.Fr. Glicenstein
C-   Updated  29-JUN-1992   A. Zylberstejn   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
      INTEGER TRUNIT
        PI=ACOS(-1.)
        TWOPI=2.*PI
        PIBY2=PI/2.
        DEGRAD=PI/180.
        RADDEG=180./PI
        LOUT=TRUNIT()
        NFADC=120
  999 RETURN
      END
