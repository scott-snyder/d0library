      SUBROUTINE CANMEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cancel an interrupt menu.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: ASTFLG is set to .FALSE. as is QIOFLG
C-
C-   Documented and modified 9-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBRES
      IF(ASTFLG) THEN
        QIOFLG=.FALSE.
        CALL DISABL
        CANFLG=.TRUE.
        ASTFLG=.FALSE.
        CURLEV=OLDLEV
        PF=0                               ! Start fresh and old level
        IF(SAVTRM) TRMFLG=.FALSE.         ! Was originally reading from command file, restore that.
        IF(RDCOM) FULSCR=.FALSE.
      ENDIF
      RETURN
      END
