      FUNCTION EV_CHECK_LIST(RUN,EVENT_NO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       find whether a given event is on the EVENT_LIST file
C-
C-   Returned value  : true if event is on list
C-   Inputs  : 
C-     RUN     = run number
C-     EVENT_NO= event output number
C-
C-   Created   9-APR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EV_CHECK_LIST
      INTEGER RUN,EVENT_NO
      INTEGER RUN_IN,EVENT_NOS(1000),NEVTS,I
      SAVE RUN_IN,EVENT_NOS,NEVTS
      DATA RUN_IN/0/
C----------------------------------------------------------------------
C
      EV_CHECK_LIST=.FALSE.
      IF(RUN_IN.NE.RUN) THEN
        RUN_IN=RUN
        CALL EV_READ_FROM_LIST(RUN_IN,NEVTS,EVENT_NOS)
      ENDIF
      DO I=1,NEVTS
        IF(EVENT_NOS(I).EQ.EVENT_NO) GOTO 1
      ENDDO
      GOTO 999   ! not on list
    1 EV_CHECK_LIST=.TRUE.
  999 RETURN
      END
