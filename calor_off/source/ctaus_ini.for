      FUNCTION CTAUS_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     initialize CTAUS package
C-   Returned value  : true if CTAUS.RCP read succesfully
C-
C-   Created  27-SEP-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CTAUS_INI
C
      INTEGER IER
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CTAUS_INI=.TRUE.
      IF ( FIRST ) THEN
        FIRST =.FALSE.
C
        CALL INRCP('CTAUS_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        CTAUS_INI = OK
        IF ( .NOT. OK ) GOTO 999        ! Failed
C
        CALL INRCPE('CTAUS_RCPE',IER)  ! Read overwrite file (RCPE)
        IF( IER .EQ. 0 )
     &  CALL ERRMSG('CTAUS','CTAUS_INI',
     &  ' Default CTAUS_RCP modified','W')
      ENDIF
C
  999 RETURN
      END
