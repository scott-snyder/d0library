      FUNCTION DILBOSON_PAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JAN-1992   DARIA ZIEMINSKA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL DILBOSON_PAR,VEEPAR,OK,DO_KSHORT
C----------------------------------------------------------------------
      CALL EZPICK('DILBOSON_RCP')
      CALL EZGET('DO_KSHORT',DO_KSHORT,IER)
      CALL EZRSET
      IF (DO_KSHORT) OK=VEEPAR()
      DILBOSON_PAR=.TRUE.
  999 RETURN
      END
