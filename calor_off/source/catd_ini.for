      FUNCTION CATD_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for CATD package
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-DEC-1991   Nobuaki Oshima
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CATD_INI
C
      INTEGER IER
      LOGICAL FIRST,OK
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST =.FALSE.
C
        CALL INRCP('CATD_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        IF ( .NOT. OK ) GOTO 999        ! Failed
C
        CALL INRCPE('CATD_RCPE',IER)  ! Read overwrite file (RCPE)
        IF( IER .EQ. 0 )
     &  CALL ERRMSG('CATD','CATD_INI',
     &  ' Default CATD_RCP modified','W')
      ENDIF
C
  999 CONTINUE
      CATD_INI = OK
      RETURN
      END
