      SUBROUTINE PXLNSTYL( INDX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface routine that will call the color/line
C-                         table (PXCOLN) to set the line style requeted by the
C-                         user.  
C-
C-   Inputs  : INDX - Number that determine what line style from the lin style
C-                    table will be set.
C-
C-   Created   4-JAN-1990   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INDX, ITYP, KCOLOR, KINTEN, KFILL, KSTYL
      CHARACTER*3 IDET
      LOGICAL CALFLG
      DATA        CALFLG /.TRUE./
      DATA        ITYP /0/
      DATA        IDET /'CDC'/
C----------------------------------------------------------------------
      CALL PXCOLN(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,KSTYL)
  999 RETURN
      END
