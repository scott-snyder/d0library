      SUBROUTINE D0HINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C
C  Initializes D0HPLT histogram plotting package...also
C  called from PXENTR so that event display and histogram
C                plotting are compatible.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C
C  Author:
C  ==========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - September 15, 1988
C-   Updated  20-FEB-1991   Harrison B. Prosper, Sharon Hagopian
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL DI3_START(1)
      CALL HPLOPT('STA ',1)
      CALL HPLSET('GSIZ',.55)
      CALL HPLSET('TSIZ',.5)
      CALL HPLSET('YGTI',1.)
      RETURN
      END
