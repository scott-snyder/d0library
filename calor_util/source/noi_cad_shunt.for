C DEC/CMS REPLACEMENT HISTORY, Element NOI_CAD_SHUNT.FOR
C *1     2-JUN-1992 17:09:34 STEWART "update from Allen Mincer for new CAHITS"
C DEC/CMS REPLACEMENT HISTORY, Element NOI_CAD_SHUNT.FOR
      SUBROUTINE NOI_CAD_SHUNT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Switch order of CAD bank linear chain
C-                         ORIGINAL: CAD1_A -> CAD1_B
C-                                   CAD2_A -> CAD2_B
C-
C-                         FINAL:    CAD1_B -> CAD1_A
C-                                   CAD2_B -> CAD2_A
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-FEB-1992   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INTEGER LCAD1,LCAD2
C----------------------------------------------------------------------
      LCAD1=LQ(LHEAD-IZCAD1)
      LCAD1=LQ(LCAD1)
      CALL ZSHUNT(IXMAIN,LCAD1,LHEAD,-IZCAD1,0)
      LCAD2=LQ(LHEAD-IZCAD2)
      LCAD2=LQ(LCAD2)
      CALL ZSHUNT(IXMAIN,LCAD2,LHEAD,-IZCAD2,0)
  999 RETURN
      END
