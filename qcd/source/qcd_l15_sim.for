      FUNCTION QCD_L15_SIM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate L15 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-JUL-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PSL1.INC'
      INTEGER IER
      LOGICAL QCD_L15_SIM
      LOGICAL FIRST
      INTEGER I, ICOUNT_1, ICOUNT_2
      DATA FIRST /.TRUE./ 
C----------------------------------------------------------------------
      QCD_L15_SIM = .TRUE.
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL HCDIR('//PAWC',' ')
        CALL HMDIR('L15CAL','S')
        CALL HBOOK1( 901, '# of L1 TOT ET candidates', 20, -.5, 19.5,
     &    0.)
        CALL HBOOK2( 902, 'REAL L1 - MY L1 ET VS ET ', 40, .5, 40.5,
     &    100, -50., 50., 0. )
        CALL HBOOK2( 903, 'REAL L1 - MY L1 ET VS ETA ', 40, 0., 4.,
     &    100, -50., 50., 0. )
        CALL HBOOK1( 904, ' # passing threshold 1x1 ', 200, 0., 200., 0.
     &    )
        CALL HBOOK1( 905, ' # passing threshold 3x3 ', 200, 0., 200., 0.
     &    )
        CALL HBOOK1( 906, ' # passing threshold 5x5 ', 200, 0., 200., 0.
     &    )
        CALL HBOOK1( 907, ' # passing ', 200, 0., 200., 0.
     &    )

C
C: RCP variables
C
        CALL INRCP('QCD_L15_RCP', IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP ERROR','QCD_L15_SIM','Cant read RCP file',
     &      'W')
          GOTO 999
        ENDIF
        CALL EZPICK('QCD_L15_RCP',IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP ERROR','QCD_L15_SIM','Cant find RCP bank',
     &      'W')
          GOTO 999
        ENDIF
        CALL EZGET( 'THRESH_1X1', THRESH_1X1, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'THRESH_3X3', THRESH_3X3, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'THRESH_5X5', THRESH_5X5, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET_i( 'COUNT_1',COUNT_1, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ET_THRESH_1',ET_THRESH_1, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ETA_LOW_1',ETA_LOW_1, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ETA_HIGH_1',ETA_HIGH_1, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET_i( 'COUNT_2',COUNT_2, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ET_THRESH_2',ET_THRESH_2, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ETA_LOW_2',ETA_LOW_2, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZGET( 'ETA_HIGH_2',ETA_HIGH_2, IER )
        IF ( IER .NE. 0 ) GOTO 900
        CALL EZRSET
      ENDIF
C
C: Setup
C
      CALL HCDIR('//PAWC/L15CAL',' ')
C
C: Fill the psuedo L1 ET arrays
C
      CALL QCD_PSEUDO_L1_FILL

C
C: Simulate the Local DSP's
C
      CALL QCD_LDSP

C
C: Simulate the Global DSP's
C
      CALL QCD_GDSP
C
C: Decide whether we pass or not
C
      ICOUNT_1 = 0
      ICOUNT_2 = 0
      DO I = 1, NENTRY( 4 )           ! Loop over final L15 objects
        IF ( PSLDSP_ETA(I) .GE. ETA_LOW_1 .AND. PSLDSP_ETA(I) .LE.
     &    ETA_HIGH_1 .AND. PSLDSP_ET(I) .GE. ET_THRESH_1 ) ICOUNT_1 =
     &    ICOUNT_1 + 1
        IF ( PSLDSP_ETA(I) .GE. ETA_LOW_2 .AND. PSLDSP_ETA(I) .LE.
     &    ETA_HIGH_2 .AND. PSLDSP_ET(I) .GE. ET_THRESH_2 ) ICOUNT_2 =
     &    ICOUNT_2 + 1
        ENDDO

        QCD_L15_SIM = ( ICOUNT_1 .GE. COUNT_1 .AND. ICOUNT_2 .GE.
     &    COUNT_2 )

C 
C: Cleanup
C
      GOTO 999
C
C: Errors
C
  900 CALL ERRMSG('RCP ERROR','QCD_L15_SIM','Cant read RCP VALUE',
     &      'W')
      CALL EZRSET
      QCD_L15_SIM = .FALSE.
      GOTO 999

  999 CALL HCDIR('//PAWC',' ')
      RETURN
      END
