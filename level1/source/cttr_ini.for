      FUNCTION CTTR_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     initialize for trigger towers package generating CTTR bank
C-     This version is temporary until correct files are defined
C-
C-   Returned value  : true if succesful
C-
C-   Created  21-AUG-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CTTR_INI
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INTEGER I,J,K,L,IER
      LOGICAL FIRST,OK,NOISE,SMEAR,EZERR
      REAL    ECUT
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CTTR_INI=.TRUE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('CAL_TTOWERS_RCP',IER)    ! Read in RCP file
        OK = IER .EQ. 0
        CTTR_INI=OK
        IF(.NOT.OK) THEN
          CALL ERRMSG('CAL_TTOWERS','CTTR_INI',
     &  ' Could not read RCP file','F')
          GOTO 999
        ENDIF
C
        CALL EZPICK('CAL_TTOWERS_RCP')
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CAL_TTOWERS','CAL_TTOWERS',
     &      'CAL_TTOWERS RCP bank not found ','W')
        ELSE
C
C ****  read runtime switches and cutoff ****
C
          CALL EZGET('ADD_NOISE',NOISE,IER)
          CALL EZGET('DO_SMEAR',SMEAR,IER)
          CALL EZGET('E_CUTOFF',ECUT,IER)
          CALL CTTOWER_SMEAR(NOISE,SMEAR,ECUT)
        ENDIF
        CALL EZRSET
C
C
C              initialize DAC_BYTE array
        DO I=POS_ETA,NEG_ETA
          DO J=ETA_MIN,ETA_MAX
            DO K=PHI_MIN,PHI_MAX
              DO L=EM_TOWER,HD_TOWER
                DAC_BYTE(I,J,K,L)=1
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
