      FUNCTION VTXCOFF_SUMMARY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process histograms and get the final calibration
C-                      constants.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTXCOFF_SUMMARY
      LOGICAL FIRST,OK, IER , VTZERO_JSUM, GOOD
      LOGICAL AREA, T_ZERO, W_GAIN, E_GAIN, WRITE_DBL3
      INTEGER LUN,IUSER,IERR
      CHARACTER*50 MSG
C
      DATA FIRST / .TRUE. /
      DATA IUSER / 777 /
C----------------------------------------------------------------------
      VTXCOFF_SUMMARY = .TRUE.
C
      IF ( FIRST ) THEN
        CALL EZPICK( 'VTXCOFF_RCP' )
        CALL EZGET('T_ZERO',T_ZERO,IER)
        CALL EZGET('AREA',AREA,IER)
        CALL EZGET('E_GAIN',E_GAIN,IER)
        CALL EZGET('W_GAIN',W_GAIN,IER)
        CALL EZGET('WRITE_DBL3',WRITE_DBL3,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C  T_Zero Calibration
C
      IF ( T_ZERO ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_T',IER,' ')
        OK = VTZERO_JSUM()
        IF ( .NOT. OK ) THEN
          VTXCOFF_SUMMARY = .FALSE.
          CALL ERRMSG('Error finding tzeros','VTXCOFF_SUMMARY',
     &      'Tzeros not written to database','W')
        ELSEIF ( WRITE_DBL3 ) THEN
          CALL VTMCHK(GOOD)
          IF ( GOOD ) THEN
            CALL VSTP_INSERT('TIMES',IER)
            IF ( IER  ) THEN
              WRITE(MSG,'(A,I2)') 
     &          'Error writing tzeros to DBL3, IER = ', IER
              CALL ERRMSG('DBL3 error','VTXCOFF_SUMMARY',
     &          MSG,'W')
            ENDIF
          ENDIF
        ENDIF
      END IF
C
C  Gain Calibration
C
      IF ( E_GAIN ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_Q',IER,' ')
        CALL VTXC_RELATIVE_GAIN
      END IF
C
      IF ( AREA .OR. W_GAIN ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_G',IER,' ')
        CALL VTX_AREA_GAIN(0,0.,1)
        CALL VTXC_FILL_EMPTY
        CALL VTXC_WRITE_GAINS ! Fill VGNL; write to file if requested
        IF ( WRITE_DBL3 ) THEN
          CALL VGNCHK(GOOD)
          IF ( GOOD ) THEN
            CALL VSTP_INSERT('GAINS',IER)
            IF ( IER ) THEN
              WRITE(MSG,'(A,I2)') 
     &          'Error writing gains to DBL3, IER = ', IER
              CALL ERRMSG('DBL3 error','VTXCOFF_SUMMARY',
     &          MSG,'W')
            ENDIF
          ENDIF
        ENDIF
      END IF
C
  999 RETURN
      END
