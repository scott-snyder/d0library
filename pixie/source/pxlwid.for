      SUBROUTINE PXLWID( INDX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface routine that will call the color/line
C-                         table (PXCOLN) to set the line width style requeted 
C-                         by the user.  
C-
C-   Inputs  : INDX - Number that determine what line width style from the 
C-                    tyle table will be set.
C-
C-   Created   31-JAN-1990   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INDX, ITYP, KCOLOR, KINTEN, KFILL, KSTYL
      CHARACTER*3 IDET
      LOGICAL CALFLG
      DATA        CALFLG /.TRUE./
      DATA        ITYP /2/
      DATA        IDET /'CDC'/
C----------------------------------------------------------------------
      CALL PXCOLN(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,KSTYL)
  999 RETURN
      END
