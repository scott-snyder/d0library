      FUNCTION C3PMET_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          Initialize C3PMET package (read C3PMET.RCP file)
C-   Returned value  : true if succesful
C-
C-   Created  12-SEP-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL C3PMET_INI
      INTEGER IER
      LOGICAL FIRST,OK
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      C3PMET_INI=.TRUE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('C3PMET_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        C3PMET_INI=OK
        IF(.NOT.OK) THEN
          CALL ERRMSG('C3PMET','C3PMET_INI',
     &  ' Could not read RCP file','F')
          GOTO 999
        ENDIF
      ENDIF
C
  999 RETURN
      END
