      SUBROUTINE D0DBL3_RCLIENT_END(NITEM,ITEM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by client process to signal end of request
C -                        activate some cleanup if necessary.
C-
C-   Inputs  : NITEM        Dimension of ITEM
C-             ITEM(1)      Database name
C-             ITEM(2-10)   Reserved
C-   Outputs :
C-
C-   Controls:
C-
C-   Created  10-JAN-1994   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NITEM,ITEM(NITEM),IER
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*80 MSG
C
      DOPT = 0
      LFORCE = .FALSE.
      LVSN = .FALSE.
      TOPN = 'D0STP'
C
      IF(CALL_DBEND) THEN
        CALL_DBEND = .FALSE.
      ELSE
        WRITE(MSG,10)
   10   FORMAT(' Attempt to Call DBEND the second time. Aborted ')
        CALL INTMSG(MSG)
      ENDIF
C
      IER = 0
C
C----------------------------------------------------------------------
  999 RETURN
      END
