      FUNCTION CDC_HST_DISPATCH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Executes "CDC histograms" to book histograms 
C-                         requested from main menu
C-
C-   Returned value  : true if ok
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  22-JAN-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CDC_HST_DISPATCH,DTRDIA
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      CDC_HST_DISPATCH = .TRUE.
      CALL EXAMINE_DISPATCH_COMMAND(COMMAND)
      IF (COMMAND .EQ. 'CDC HISTOGRAMS') THEN
        CDC_HST_DISPATCH = DTRDIA()
      END IF
C
  999 RETURN
      END
