      FUNCTION VTXCOFF_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Accumulate histograms from processed data
C-                  for offline calibration of VTX
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTXCOFF_EVT
      LOGICAL FIRST,OK, IER , VTZERO_EVT
      LOGICAL AREA, T_ZERO, W_GAIN, E_GAIN
C
      CHARACTER*40 MSG
      INTEGER NEV
      SAVE NEV
C
      DATA NEV / 0 /
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      VTXCOFF_EVT = .TRUE.
      NEV = NEV + 1
      IF ( MOD(NEV,100) .EQ. 0 ) THEN
        WRITE(MSG,'(A,I5,A)') ' VTXCOFF: ', NEV, ' EVENTS PROCESSED'
        CALL INTMSG(MSG)
      ENDIF
C
      IF ( FIRST ) THEN
        CALL EZPICK( 'VTXCOFF_RCP' )
        CALL EZGET('T_ZERO',T_ZERO,IER)
        CALL EZGET('AREA',AREA,IER)
        CALL EZGET('E_GAIN',E_GAIN,IER)
        CALL EZGET('W_GAIN',W_GAIN,IER)
        CALL EZRSET
        FIRST =.FALSE.
      END IF
C
C  T_Zero Calibration
C
      IF ( T_ZERO ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_T',IER,' ')
        OK = VTZERO_EVT()
        IF ( .NOT. OK ) VTXCOFF_EVT = .FALSE.
      END IF
C
C  Gain Calibration
C
      IF ( AREA .OR. E_GAIN .OR. W_GAIN ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_G',IER,' ')
        CALL VTXC_MATCH_ZTRAK
      END IF
C
  999 RETURN
      END
