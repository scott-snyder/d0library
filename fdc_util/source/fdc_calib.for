      FUNCTION FDC_CALIB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call various routines for calibrating FDC 
C-      from real data.
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : histograms
C-
C-   Created  25-MAY-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_CALIB, FDC_CALIB_BOOK
C
      INTEGER LRCP,IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      FDC_CALIB = .TRUE.
      CALL FDC_XSW_HISTS                ! Global t0 from X-SW segments
      CALL FDC_XSECT_HISTS              ! Drift Veloc from X-Sect segments
      RETURN
C
C----------------------------------------------------------------------
C
      ENTRY FDC_CALIB_BOOK()
      FDC_CALIB_BOOK = .TRUE.
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        CALL EZLOC('FDC_RCP',LRCP)                        
        IF(LRCP.LE.0) THEN                                   
          CALL INRCP ('FDC_RCP',IER)  
        ENDIF
C
        CALL FDC_XSW_BOOK
        CALL FDC_XSECT_BOOK
      ENDIF
C
  999 RETURN
      END
