      LOGICAL FUNCTION NP_WRIGHT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filtering for Right handed W events.  This
C-   function expects to see DST format data.  The requirements are:
C-
C-     One PELC with more than ELEC_PT_CUT
C-
C-
C-   Returned value:  .TRUE. if the event is to be kept;
C-                    .FALSE. otherwise
C-   Inputs  : none
C-   Outputs : none
C-   Controls: control parameters in NP_WRIGHT_RCP
C-
C-    
C-   Created  25-JUN-1993   Azriel Goldschmidt   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      LOGICAL  FIRST, OK
      LOGICAL  NP_WRIGHT_EOJ,NP_WRIGHT_STA,NP_WRIGHT_SUM
      DATA     FIRST /.TRUE./
      REAL RSUM(20),RSUMMARY(20),ELEC_PT_CUT
      INTEGER LPELC,GZPELC,IER,I
      CHARACTER*128 MSG
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL VZERO (RSUMMARY, 20)
        CALL INRCP ('NP_WRIGHT_RCP', IER)
C
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ('No NP_WRIGHT_RCP', 'NP_WRIGHT', 
     &      'Could not find NP_WRIGHT_RCP', 'F')
        ENDIF                  
        CALL EZPICK ('NP_WRIGHT_RCP')
        CALL EZGET ('ELEC_PT_CUT', ELEC_PT_CUT, IER)
        CALL EZRSET
      ENDIF  
C
C ****  Beginning of event processing
C
      NP_WRIGHT = .FALSE.                 ! default action -- reject event
C
      RSUMMARY(1)=RSUMMARY(1)+1
      LPELC=GZPELC()
      IF(LPELC.LE.0) GOTO 999
      CALL  ZSORT (IXCOM,LPELC,7)
      LPELC=GZPELC()
      CALL  ZTOPSY (IXCOM,LPELC)
      LPELC=GZPELC()
      RSUMMARY(2)=RSUMMARY(2)+1
      IF(Q(LPELC+7).LT.ELEC_PT_CUT)GOTO 999
      RSUMMARY(3)=RSUMMARY(3)+1
      NP_WRIGHT = .TRUE.
 999  RETURN
C
      ENTRY NP_WRIGHT_EOJ(RSUM)
C
      NP_WRIGHT_EOJ = .TRUE.
      DO I = 1, 20
        RSUM(I) = RSUMMARY(I)
      ENDDO
      RETURN
      ENTRY NP_WRIGHT_STA()
      MSG=' '
        WRITE(MSG,120)RSUMMARY(1),RSUMMARY(2),RSUMMARY(3),RSUMMARY(3)
     &    /RSUMMARY(1)*100.
        CALL INTMSG(MSG)
      RETURN
      ENTRY NP_WRIGHT_SUM()
      MSG=' '
        WRITE(MSG,120)RSUMMARY(1),RSUMMARY(2),RSUMMARY(3),RSUMMARY(3)
     &    /RSUMMARY(1)*100.
        CALL INTMSG(MSG)
 120  FORMAT(1X,'TOTAL EVTS=',F14.5,'TOTAL ELC=',F14.5,'TOTAL PASS=',
     &  F14.5,'PERCENTAGE PASS=',F14.5)
      RETURN
      END
