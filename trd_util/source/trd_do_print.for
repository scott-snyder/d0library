      LOGICAL FUNCTION TRD_DO_PRINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Check if we want the debug prints
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-DEC-1993   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TCNTRL.INC'
c      INCLUDE 'D0$INC:TRDBGU.INC'
      INTEGER         SWTDBG,TNEVDG
      INTEGER IER,LOUT,TRUNIT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,NEVT,EVTI,EVT_TO_DEBUG
      LOGICAL FIRST,doprint
      data first/.true./
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('SWTDBG',SWTDBG,IER)
        CALL EZGET('TNEVDG',TNEVDG,IER)
        EVT_TO_DEBUG=0
        CALL EZGET('PRINT_EVENT',I,IER)
        IF(IER.EQ.0)                EVT_TO_DEBUG=I
        TNEVDG=TNEVDG+1
        LOUT = TRUNIT()
        CALL EZRSET
        DOPRINT=SWTDBG.GE.1 .AND. LOUT.NE.0
        EVTI=-100
      END IF
      IF(DOPRINT .AND. EVTI.NE.IQ(LHEAD+9))THEN ! check if new event
        EVTI=IQ(LHEAD+9)
        TNEVDG=TNEVDG-1
      END IF
      TRD_DO_PRINT=(DOPRINT.AND.TNEVDG.GT.0) .OR.
     +  (IQ(LHEAD+9).EQ.EVT_TO_DEBUG .AND.LOUT.NE.0)
C----------------------------------------------------------------------
  999 RETURN
      END
