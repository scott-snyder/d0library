      SUBROUTINE CDC_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used by DSTFIX program to implement
C-                         available fixes at the DST level
C-
C-   Inputs  : None
C-   Outputs : "Fixed" DTRK banks
C-   Controls: RCP
C-
C-   Created  25-JUL-1995   Norman A. Graf
C-   Updated   2-NOV-1995   NORMAN A. GRAF  Added check on Monte Carlo 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST,DO_CDC_DEDX_CORR,DO_CDC_Z_CORR,MONTE_CARLO_DATA
      DATA FIRST /.TRUE./
      INTEGER IER
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CD_FIX_RCP')
        CALL EZGET('DO_CDC_DEDX_CORR',DO_CDC_DEDX_CORR,IER)
        CALL EZGET('DO_CDC_Z_CORR',DO_CDC_Z_CORR,IER)
        CALL EZRSET
        IF (IQ(LHEAD+1) .GT. 1000)  MONTE_CARLO_DATA = .TRUE.
      ENDIF
C
      IF(.NOT.MONTE_CARLO_DATA) THEN
        IF(DO_CDC_DEDX_CORR) CALL DTRK_FIX_DEDX
        IF(DO_CDC_Z_CORR) CALL DTRK_FIXZ
      ENDIF
C
  999 RETURN
      END
