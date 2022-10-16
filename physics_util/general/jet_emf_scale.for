      FUNCTION JET_EMF_SCALE( ET, ETA_DET, EMF, ISYS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the relative response of a jet of
C-                         this emfraction when compared to the average.
C-
C-
C-   Returned value  : JET_EMF_SCALE
C-                            [R]   Relative response of this jet when
C-                                  compared to an average jet of the
C-                                  same eta and ET.
C-
C-   Inputs  :          ET    [R]   Transverse energy of this jet
C-                    ETA_DET [R]   Detector eta of this jet
C-                      EMF   [R]   Em fraction of this jet
C-                     ISYS   [I]   0 = nominal 1=low correction 2= high
C-
C-   Outputs :
C-   Controls: QCD_JET_CORRECTION.RCP
C-
C-   Created  22-OCT-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL JET_EMF_SCALE
      REAL ET, ETA_DET, EMF, EMF_USED
      REAL EMF_MAX, EMF_MIN
      PARAMETER( EMF_MIN = .05 )    ! Minimum EMF allowed
      PARAMETER( EMF_MAX = .95 )    ! Maximum EMF allowed
      INTEGER N_ETA_BINS
      PARAMETER( N_ETA_BINS = 3 )   ! # eta bins: CC, ICD and EC
      INTEGER N_ET_BINS
      PARAMETER( N_ET_BINS = 5  )   ! # ET bins: min,low,med,hi,max
      INTEGER N_DOF
      PARAMETER( N_DOF = 6 )        ! # of parameters: use 5th deg.
C                                   ! polynomial fit
      REAL PARAM_RESPONSE( N_DOF, N_ETA_BINS, N_ET_BINS )
      REAL AVE_RESPONSE( N_ETA_BINS, N_ET_BINS )
      REAL ET_BINS( N_ET_BINS )
      REAL ETA_BINS( N_ETA_BINS )
      SAVE PARAM_RESPONSE, AVE_RESPONSE, ET_BINS, ETA_BINS
      REAL REL1, REL2, DET
      INTEGER I_ETA_BIN, I_ET_BIN, I_ET_BIN2
      INTEGER I, J, IER, IERTOT, ISYS
      INTEGER EMF_REGIONS(N_ETA_BINS)
      SAVE EMF_REGIONS
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      JET_EMF_SCALE = 1.0           ! Set default
C
C: One time initialization which reads in needed parameters
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL INRCP('QCD_JET_CORRECTION_RCP', IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_EMF_SCALE',
     &      'Cant read in QCD_JET_CORRECTION RCP file','F')
          GOTO 900
        ENDIF
C
        CALL EZPICK('QCD_JET_CORRECTION_RCP')
        CALL EZERR( IER  )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_EMF_SCALE',
     &      'Cant find bank ','F')
          GOTO 900
        ENDIF
C
        IERTOT = 0
C--- Read in relative responses
        CALL EZGET_i('EMF_REGIONS',EMF_REGIONS, IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_CC_MIN',PARAM_RESPONSE(1,1,1), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_ICD_MIN',PARAM_RESPONSE(1,2,1), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_EC_MIN',PARAM_RESPONSE(1,3,1), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_CC_LOW',PARAM_RESPONSE(1,1,2), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_ICD_LOW',PARAM_RESPONSE(1,2,2), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_EC_LOW',PARAM_RESPONSE(1,3,2), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_CC_MED',PARAM_RESPONSE(1,1,3), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_ICD_MED',PARAM_RESPONSE(1,2,3), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_EC_MED',PARAM_RESPONSE(1,3,3), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_CC_HIGH',PARAM_RESPONSE(1,1,4), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_ICD_HIGH',PARAM_RESPONSE(1,2,4), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_EC_HIGH',PARAM_RESPONSE(1,3,4), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_CC_MAX',PARAM_RESPONSE(1,1,5), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_ICD_MAX',PARAM_RESPONSE(1,2,5), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('REMF_EC_MAX',PARAM_RESPONSE(1,3,5), IER )
        IERTOT = IERTOT + ABS(IER)
C---Read in average responses
        CALL EZGET('AVE_MIN_RESPONSES', AVE_RESPONSE(1, 1 ), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('AVE_LOW_RESPONSES', AVE_RESPONSE(1, 2 ), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('AVE_MED_RESPONSES', AVE_RESPONSE(1, 3 ), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('AVE_HIGH_RESPONSES', AVE_RESPONSE(1, 4 ), IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('AVE_MAX_RESPONSES', AVE_RESPONSE(1, 5 ), IER )
        IERTOT = IERTOT + ABS(IER)
C---Et bins
        CALL EZGET('ET_BINS', ET_BINS, IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('ETA_BINS', ETA_BINS, IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZRSET
        IF ( IERTOT .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_EMF_SCALE',
     &      'Read error:abort ','F')
          GOTO 900
        ENDIF
      ENDIF

C
C---Determine which ET and eta bin we should use
C
      I_ETA_BIN = N_ETA_BINS
      DO I = N_ETA_BINS, 1, -1
        IF ( ABS( ETA_DET ) .LE. ETA_BINS( I ) ) I_ETA_BIN = I
      ENDDO
C
C---See if the user wants to apply a correction in this region
C
      IF ( EMF_REGIONS( I_ETA_BIN) .NE. 1 ) GOTO 999
C
      I_ET_BIN  = N_ET_BINS
      DO I = N_ET_BINS, 1, -1
        IF (   ET  .LE. ET_BINS( I ) ) I_ET_BIN = I
      ENDDO
C
C: Find another bin to interpolate between
C
      IF ( ET .LE. ET_BINS(1) .OR. ET .GE. ET_BINS( N_ET_BINS ) .OR.
     &  I_ET_BIN .EQ. 1 ) THEN
        I_ET_BIN2 = I_ET_BIN
        DET = 1.
      ELSE
        I_ET_BIN2 = I_ET_BIN - 1
        DET = ET_BINS( I_ET_BIN ) - ET_BINS( I_ET_BIN2 )
      ENDIF
C
C: Check emf value we were given. Should be in the range 0. to 1.
C
      EMF_USED = MAX( EMF_MIN , MIN( EMF_MAX, EMF ) )
C
C: Find relative responses
C
      REL1 = 0.0
      REL2 = 0.0
      DO I = 1, N_DOF
        REL1 = REL1 + PARAM_RESPONSE(I,I_ETA_BIN, I_ET_BIN)
     &    *(EMF_USED**(I-1))
        REL2 = REL2 + PARAM_RESPONSE(I,I_ETA_BIN, I_ET_BIN2)
     &    *(EMF_USED**(I-1))
      ENDDO
C
C: Divide by average response to get true relative response
C
      REL1 = REL1/AVE_RESPONSE(I_ETA_BIN, I_ET_BIN )
      REL2 = REL2/AVE_RESPONSE(I_ETA_BIN, I_ET_BIN2 )
C
C: Now interpolate
C
      JET_EMF_SCALE = REL2 + (REL1-REL2)*(ET-ET_BINS(I_ET_BIN2))/DET
C
C: Exit without error
C
      GOTO 999

  900 CONTINUE  ! Error condition
  999 RETURN
      END
