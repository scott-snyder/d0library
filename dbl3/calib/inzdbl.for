      SUBROUTINE INZDBL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize ZEBDBL store for dbl3 handeling
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-DEC-1993   SHAHRIAR ABACHI, Similar to inzstp
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBDBL.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL MZSTOR (IXDBL,'/ZEBDBL/','C',FENDBL,JDBLH,ZDCONS,
     &    ZDCONS(10),ZDCONS(10000),ENDZD)
C
        IDVDBL = IXDBL+2
        CALL CONDBL
      ENDIF
  999 RETURN
      END
