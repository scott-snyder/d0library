      FUNCTION CAL_TTOWERS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Interface to fill CTTR bank (calorimeter trigger towers)
C-     must use a file with CAEP banks
C-
C-   Returned value  : .TRUE.
C-
C-   ENTRY CTTOWER_SMEAR(ADD_NOISE,DO_SMEAR,ECUT)
C-   Inputs:
C-   ADD_NOISE=true if noise wanted
C-   DO_SMEAR =true if smearing is to be done
C-   ECUT     =energy cutoff in trigger towers
C-   
C-   Created  19-AUG-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CAL_TTOWERS,CTTOWER_SMEAR
      INTEGER IER
      LOGICAL ADD_NOISE,DO_SMEAR,NOISE,SMEAR,OK,DO_ANALYSIS
      LOGICAL FIRST,EZERR
      REAL CUTOFF,ECUT
      SAVE NOISE,SMEAR,CUTOFF,FIRST,DO_ANALYSIS
      DATA NOISE,SMEAR,CUTOFF/.TRUE.,.TRUE.,1.0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('CAL_TTOWERS_RCP')
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CAL_TTOWERS','CAL_TTOWERS',
     &      'CAL_TTOWERS RCP bank not found ','W')
        ELSE
          CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
          CALL EZRSET
        ENDIF
      ENDIF
      CAL_TTOWERS=.TRUE.
      CALL CTTRFL(NOISE,CUTOFF,OK)
      IF(SMEAR) CALL CTTR_SMEAR
      IF(DO_ANALYSIS) CALL CTTR_ANL        ! CTTR histograms
  999 RETURN
C
      ENTRY CTTOWER_SMEAR(ADD_NOISE,DO_SMEAR,ECUT)
      NOISE=ADD_NOISE
      SMEAR=DO_SMEAR
      CUTOFF=ECUT
      RETURN
C
      END
