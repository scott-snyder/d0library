      FUNCTION CTTR_DMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump CTTR bank
C-
C-   Returned value  : TRUE
C-
C-   ENTRY CTTR_DEFD  turn on/off CTTR dump
C-   Returned value  : TRUE 
C-
C-   Created   15-Aug-1990
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CTTR_DMP,CTTR_DEFD,YES,FIRST
      EXTERNAL PRCTTR
      SAVE YES
      DATA YES/.TRUE./
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL DMPBNK('CTTR',YES)
        FIRST=.FALSE.
      ENDIF
      CTTR_DMP=.TRUE.
C
      CALL DMPANY('CTTR',PRCTTR)
C
      RETURN
C
C
      ENTRY CTTR_DEFD()
C
      CTTR_DEFD=.TRUE.
      YES=.TRUE.
      CALL GETPAR(1,' Dump CTTR (cal trigger towers) bank?[Y]:',
     &  'L',YES)
      CALL DMPBNK('CTTR',YES)
  100 CONTINUE
  999 RETURN
  101 FORMAT(' DUMP FOR BANK ',A4,' NOT CODED ')
      END
