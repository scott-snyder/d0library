      FUNCTION FTREVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main event analyzing control routine for FTRAKS
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-APR-1991   Jeffrey Bantly
C-   Updated  27-FEB-1992   Robert E. Avery  Add optional call to FDCISA 
C-   Updated  26-MAY-1992   Robert E. Avery  Add optional call to FDC_CALIB
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FTREVT
C
      INTEGER IER
C
      LOGICAL EZERROR,FIRST,DOHIST,DOFISA,DOCALIB 
C
      SAVE FIRST,DOHIST,DOFISA,DOCALIB 
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      FTREVT = .TRUE.
C
      IF(FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FTREVT','FTRAKS_RCP not found.','W')
          GOTO 999
        ELSE
          CALL EZGET('DOFISA',DOFISA,IER)
          CALL EZGET('DOHIST',DOHIST,IER)
          CALL EZGET('DOCALIB',DOCALIB,IER)
          CALL EZRSET
          IF ( DOCALIB ) THEN
            CALL FDC_CALIB_BOOK
          ENDIF
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
      CALL FTRAKS
      IF ( DOHIST ) THEN
        CALL FTRHIS
      ENDIF
      IF ( DOFISA ) THEN
        CALL FDCISA
      ENDIF
      IF ( DOCALIB ) THEN
        CALL FDC_CALIB
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
