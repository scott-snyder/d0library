      SUBROUTINE NOI_EVENT_CLEAN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :DROP unnecessary banks for pileup events
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  7-OCT-1991   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INTEGER LCAD1,LCAD2,LISAE,LPARH,LHSTR,LPROC
C----------------------------------------------------------------------
      LCAD1 = LQ(LHEAD-IZCAD1)
      IF(LCAD1.GT.0)CALL MZDROP(IXMAIN,LCAD1,' ')
      LCAD2 = LQ(LHEAD-IZCAD2)
      IF(LCAD2.GT.0)CALL MZDROP(IXMAIN,LCAD2,' ')
  999 RETURN
      END
