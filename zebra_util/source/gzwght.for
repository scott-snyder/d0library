      FUNCTION GZWGHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to WGHT
C-
C-   Created  Apr-28-1993 Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZWGHT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZWGHT.LINK'
C----------------------------------------------------------------------
C
      GZWGHT=0
      IF(LHEAD.NE.0)THEN
        IF(IQ(LHEAD-3).GE.IZWGHT)GZWGHT=LQ(LHEAD-IZWGHT)
      ENDIF
      RETURN
      END
