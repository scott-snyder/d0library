      INTEGER FUNCTION GZMOTR(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :     Find pointer to MOTR
C-
C-   Created  29-JAN-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMOTR.LINK'
      INTEGER I,LMTRG,GZMTRG
C
      GZMOTR=0
      LMTRG=GZMTRG(0)                   
      IF(LMTRG.NE.0) GZMOTR=LQ(LMTRG-IZMOTR)
C
      RETURN
      END
