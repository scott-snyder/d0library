      SUBROUTINE PXBUILD_VERSION(VERSION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return version number of PXBUILD program.
C-
C-   Inputs  : None
C-   Outputs : VERSION  [C*]    Version number (format: Vx.yy)
C-   Controls: 
C-
C-   Created  15-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) VERSION
C----------------------------------------------------------------------
      CHARACTER*16 VV
      INTEGER IER,L
      LOGICAL EZERROR
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,VV
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('PXBUILD_RCP')
        IF ( .NOT. EZERROR(IER) ) THEN
          CALL EZGETS('VERSION',1,VV,L,IER)
          CALL EZRSET
          VV = VV(1:L)
        ELSE
          VV = ' '
        ENDIF
      ENDIF
      VERSION = VV
  999 RETURN
      END
