      FUNCTION TAUFIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization for TAUFIX package
C-
C-   Returned value  : true: initialization is ok
C-
C-   Created  25-MAY-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TAUFIX_INI
      LOGICAL FIRST, OK
      INTEGER IER, LRCP
C      
      SAVE FIRST, OK
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
      TAUFIX_INI = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL INRCP('CTAUS_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        TAUFIX_INI = OK
        IF ( .NOT. OK ) GOTO 999        ! Failed
C
        CALL INRCPE('CTAUS_RCPE',IER)  ! Read overwrite file (RCPE)
        IF( IER .EQ. 0 )
     &    CALL ERRMSG('TAUFIX','TAUFIX_INI',
     &    ' Default CTAUS_RCP modified','W')
C
        CALL INRCP('TAU_HMATRIX_RCP',IER)    ! read in RCP file for tau_matrix
        OK = IER .EQ. 0
        TAUFIX_INI = OK
        IF ( .NOT. OK ) GOTO 999             ! Failed
C
        CALL INRCPE('TAU_HMATRIX_RCPE',IER)  ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &    CALL ERRMSG('TAUFIX','TAUFIX_INI',
     &    ' Default TAU_HMATRIX_RCP modified','W')
C
        CALL EZLOC('ZTRAKS_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('ZTRAKS_RCP',IER)
          OK = IER .EQ. 0
          IF (.NOT. OK) CALL ERRMSG('TAU_FIX','TAUFIX_INI',
     &      'Reading ZTRAKS_RCP failed','W')
        ENDIF
      ENDIF
C
  999 RETURN
      END
