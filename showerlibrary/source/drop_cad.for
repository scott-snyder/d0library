      SUBROUTINE DROP_CAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :DROP CAD1, CAD2 BANKS IF THEY EXIST 
C-                        and  MUD1 , CDD1, CDD2 , CDD2 , 
C-                        CDD4 , CAD1 , CAD2 , FLTR , RECO banks from the
C-                        input.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-JUN-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$LINKS:IZFLTR.LINK'
      INTEGER LCAD1,LCAD2,LMUD1,LCDD1,LCDD2,LCDD3,LCDD4,LRECO,LFLTR
C----------------------------------------------------------------------
      LCAD1 = LQ(LHEAD-IZCAD1)
      LCAD2 = LQ(LHEAD-IZCAD2)
      LMUD1 = LQ(LHEAD-IZMUD1)
      LCDD1 = LQ(LHEAD-IZCDD1)
      LCDD3 = LQ(LHEAD-IZCDD3)
      LCDD4 = LQ(LHEAD-IZCDD4)
      LRECO = LQ(LHEAD-IZRECO)
      LFLTR = LQ(LHEAD-IZFLTR)
      IF(LCAD1.GT.0)CALL MZDROP(IXMAIN,LCAD1,' ')
      IF(LCAD2.GT.0)CALL MZDROP(IXMAIN,LCAD2,' ')
      IF(LMUD1.GT.0)CALL MZDROP(IXMAIN,LMUD1,' ')
      IF(LCDD1.GT.0)CALL MZDROP(IXMAIN,LCDD1,' ')
      IF(LCDD3.GT.0)CALL MZDROP(IXMAIN,LCDD3,' ')
      IF(LCDD4.GT.0)CALL MZDROP(IXMAIN,LCDD4,' ')
      IF(LRECO.GT.0)CALL MZDROP(IXMAIN,LRECO,' ')
      IF(LFLTR.GT.0)CALL MZDROP(IXMAIN,LFLTR,' ')
  999 RETURN
      END
