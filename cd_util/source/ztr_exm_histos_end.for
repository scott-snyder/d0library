      FUNCTION ZTR_EXM_HISTOS_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-DEC-1993   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ZTR_EXM_HISTOS_END
C
      INTEGER IER
C
      LOGICAL FIRST,OK
      LOGICAL PRINT_HISTOS,ZTRAKS_EXM_HPRINT
      LOGICAL COMPARE_HISTOS,ZTR_EXM_HCOMPARE
C
      DATA FIRST/.TRUE./
      DATA PRINT_HISTOS/.TRUE./
      DATA COMPARE_HISTOS/.TRUE./
C
C----------------------------------------------------------------------
C
      ZTR_EXM_HISTOS_END = .TRUE.
C
      IF (FIRST) THEN
        CALL EZPICK('CD_EXM_HISTOS_RCP')
        CALL EZGET('PRINT_HISTOS',PRINT_HISTOS,IER)
        CALL EZGET('COMPARE_HISTOS',COMPARE_HISTOS,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      IF (PRINT_HISTOS) OK = ZTRAKS_EXM_HPRINT()
      IF (COMPARE_HISTOS) OK = ZTR_EXM_HCOMPARE()
C
  999 RETURN
      END
