      SUBROUTINE GTNJET(LJETS,NJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find out how many JETS banks exist
C-
C-   Inputs  : LJETS - LINK to first JETS bank
C-   Outputs : NJETS - Number of JETS banks
C-   Controls: 
C-
C-   Created   1-MAY-1989   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C---------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER LJETS,NJET,LNEXT
C---------------------------------------------------------------------
      NJET=0
      LNEXT=LJETS
   10 IF(LNEXT.LE.0)GO TO 999
      NJET=NJET+1
      LNEXT=LQ(LNEXT)
      IF(LNEXT.GT.0)GO TO 10      
  999 RETURN
      END
