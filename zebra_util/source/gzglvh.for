
      INTEGER FUNCTION GZGLVH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find link to GLVH bank hanging from GHIT
C-
C-   Returned value  : GZGLVH link address
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   7-MAR-1989   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGLVH.LINK'
      INTEGER GZGHIT,LGHIT,LGLVH
C----------------------------------------------------------------------
C
      LGHIT=GZGHIT()
      LGLVH=0
      IF(LGHIT.NE.0)  LGLVH=LQ(LGHIT-IZGLVH)
      GZGLVH=LGLVH
C
      RETURN
      END
