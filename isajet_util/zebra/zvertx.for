C DEC/CMS REPLACEMENT HISTORY, Element ZVERTX.FOR
C *1    13-JUN-1989 22:25:09 RAJA "Rajendran Raja: Isajet Vertex getter"
C DEC/CMS REPLACEMENT HISTORY, Element ZVERTX.FOR
      SUBROUTINE ZVERTX(ZV,DZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine returns Z and DZ of vertx
C-
C-   Inputs  : None
C-   Outputs : ZV,DZ   Z position and Spread of Vertex
C-   Controls: None
C-
C-   Created  23-MAR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INTEGER LISAE,LISV1
      REAL ZV,DZ
C----------------------------------------------------------------------
      ZV = 0.0
      DZ = 0.0
C
      LISAE=LQ(LHEAD-IZISAE)
      IF(LISAE.NE.0) THEN
        LISV1=LQ(LISAE-IZISV1)
        IF(LISV1.NE.0) ZV=Q(LISV1+9)
      ENDIF
C
  999 RETURN
      END
