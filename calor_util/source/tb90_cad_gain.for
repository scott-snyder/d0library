      FUNCTION TB90_CAD_GAIN(JETA,JPHI,JLYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO RETURN THE GAIN FROM ENERGY TO ADC COUNTS
C-               OF CHANNEL WITH JETA,JPHI,JLYR in TB90 DATA
C-
C-   Inputs  : JETA,JLYR,JPHI
C-   Outputs : NONE
C-   Controls:
C-   RETURNS:  CAL ADC GAIN - GEV TO ADC COUNTS
C-
C-   Created  1-AUG-1990   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL TB90_CAD_GAIN,GAIN
      INTEGER IETA,ILYR,IPHI,JETA,JLYR,JPHI,NLYR,IER,I
      REAL GAIN_10PF(4),SFRAC_CORR(17),ADC_TO_GEV
      LOGICAL FIRST,CALIB_GAINS,EM,IH,MH,EZERR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
C
C ****   get GAIN constants out of CAL_GAINS.PARAMS
C
        CALL EZPICK('TB90_CALOR_UNPACK_RCP')
        IF(EZERR(IER)) THEN
          ADC_TO_GEV = 257.
          DO I = 1, 4
            GAIN_10PF(I) = 1.0 
            SFRAC_CORR(I) = 1.0
            SFRAC_CORR(I+10) = 1.5
          END DO
          DO I = 5, 10
            SFRAC_CORR(I) = 0.0
          END DO
          SFRAC_CORR(15) = 6.0
          SFRAC_CORR(16) = 1.5
          SFRAC_CORR(17) = 6.0
          CALL ERRMSG('NO TB90_CALOR_UNPACK_RCP ','TB90_CAD_GAINS',
     &      'NO RCP FOR TB90_CAD_GAINS - USE DEFAULTS','W')
        ELSE
          CALL EZGET('ADC_TO_GEV',ADC_TO_GEV,IER)
          CALL EZGET('CAPACITANCE_GAIN_CORR',GAIN_10PF,IER)
          CALL EZGET('SAMPLING_FRACTION_CORR',SFRAC_CORR,IER)
          CALL EZGET('DO_GNSCOR',CALIB_GAINS,IER)        
        END IF
        CALL EZRSET
C
        FIRST=.FALSE.
      ENDIF
C
C ****  DETERMINE SAMPLING FRACTION LAYER
C
      EM = .FALSE.
      IH = .FALSE.
      MH = .FALSE.
      IETA = JETA
      IPHI = JPHI
      ILYR = JLYR
      IF (ILYR.LE.7) EM = .TRUE.
      IF(EM) THEN            ! EM layers
        IF (IETA.LE.13) GOTO 999 ! not TB Load 1 data
        NLYR = ILYR
        IF (ILYR.GE.3.AND.ILYR.LE.6) NLYR = 3
        IF (ILYR.EQ.7) NLYR = 4
        ILYR = NLYR
      ELSE      !  IH or MH layers
        IF (ILYR.GE.11.AND.IETA.GE.21 ) THEN
          IH = .TRUE.
        ELSE IF (ILYR.GE.11.AND.IETA.LE.16) THEN
          MH = .TRUE.
        ELSE IF (ILYR+6.GT.IETA) THEN
          MH = .TRUE.
        ELSE
          IH = .TRUE.
        ENDIF
      ENDIF
      IF (MH) THEN
        NLYR = ILYR
        IF (ILYR.GE.11.AND.ILYR.LE.14) NLYR = 16
        IF (ILYR.EQ.15) NLYR = 17
        ILYR = NLYR
      END IF
      GAIN = 1.0
      IF (.NOT. CALIB_GAINS) THEN
        IF (ILYR.LE.4 .AND. IPHI.GE.29 .AND. IPHI.LE.34) THEN
          GAIN  = GAIN_10PF(ILYR)  ! Correct for 10pF in EM3 & EM4
        ENDIF
      END IF
      GAIN = GAIN * SFRAC_CORR(ILYR)  ! Sampling fraction correction
      TB90_CAD_GAIN = GAIN / ADC_TO_GEV
  999 RETURN
      END
