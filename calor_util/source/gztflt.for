C DEC/CMS REPLACEMENT HISTORY, Element GZTFLT.FOR
C *1    18-MAY-1990 11:13:36 STEWART "TB90 L2 FILTER ZEBRA ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element GZTFLT.FOR
      INTEGER FUNCTION GZTFLT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to TFLT
C-
C-   Created  11-MAY-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTFLT.LINK'
      INTEGER GZTB90,LTB90,LTFLT
C----------------------------------------------------------------------
C
      LTB90=GZTB90()
      LTFLT=0
      IF(LTB90.NE.0)  LTFLT=LQ(LTB90-IZTFLT)
      GZTFLT=LTFLT
C
  999 RETURN
      END
