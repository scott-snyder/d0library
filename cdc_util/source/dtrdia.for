      FUNCTION DTRDIA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      dialog for booking histograms for CDC test
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: entry DCHIST 
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  moved LOGICAL from routine
C-                                         name to Type Declaration Statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NRUN, ERR, IER
      LOGICAL DTRDIA
      LOGICAL DCHIST, BOKHST
C
      DATA BOKHST/.FALSE./
C----------------------------------------------------------------------
C
      DTRDIA=.TRUE.
C
      CALL INTMSG(' Book histograms for CDC test!
     & Please select the histograms you want.')
C
C         Create/Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('DTRAKS','DTRDIA',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      BOKHST = .TRUE.
C
      CALL DBKEFF    !  book efficiency histograms for SW and DL
      CALL DBKCHI    !  book Chi-square histograms for XY fit and RZ fit
      CALL DBKDEX    !  book DE/DX histogram
      CALL DBKPUL    !  book pulse area, wid and hei VS wire # histograms
      CALL DBKPED    !  book pedestal and T0 histograms
C      CALL DBKDSC    !  book DL charge VS SW charge histograms
      CALL DBKCHD    !  book <charge> VS drift distance histograms
C      CALL DBKCHZ    !  book <DL charge/SW_o charge> VS Z histograms
      CALL DBKSWR    !  book residuals and resolution histograms for SW 
      CALL DBKDLR    !  book residuals and resolution histograms for DL
C
      CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
C
  999 RETURN
C
      ENTRY DCHIST
      DCHIST = BOKHST
      END
