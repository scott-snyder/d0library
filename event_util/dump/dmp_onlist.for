      FUNCTION DMP_ONLIST(DUMP_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     check if event on list of events to dump
C-   Returned value  : true if event is on list
C-   Input:
C-    DUMP_TYPE='RAWD' for raw dump, 'PROD' for processed dump
C-
C-   ENTRY DMPLST(NLIST,LIST)
C-     fill array EVLIST with list of events to dump
C-   Input:
C-   NLIST = number of events in list
C-   LIST  = list of event ids
C-
C-   Created  15-SEP-1989   Serban D. Protopopescu
C-   Modified 16-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      CHARACTER*4 DUMP_TYPE
      LOGICAL DMP_ONLIST,DMPLST
      INTEGER EVLIST(3,NELIST)
      INTEGER NLIST,EVENT_TO_DUMP(*),RUN,ID,I,IER,NDUMPED,N_TO_DUMP
      SAVE EVLIST,NLIST,NDUMPED,N_TO_DUMP
      DATA N_TO_DUMP,NDUMPED/-1,0/
C----------------------------------------------------------------------
C
      DMP_ONLIST=.FALSE.
      CALL EVNTID(RUN,ID)
      DO 8 I=1,NLIST
        IF(EVLIST(1,I).EQ.RUN.AND.EVLIST(2,I).EQ.ID) GOTO 10
    8 CONTINUE
C
      IF(NDUMPED.LT.N_TO_DUMP) THEN
        NDUMPED=NDUMPED+1
        DMP_ONLIST=.TRUE.
        IF(NDUMPED.EQ.N_TO_DUMP) THEN
          N_TO_DUMP=-1
          IF(DUMP_TYPE.EQ.'RAWD') NHDONE=NHDONE+1
          IF(DUMP_TYPE.EQ.'PROD') NFDONE=NFDONE+1
        ENDIF
      ENDIF
C
      GOTO 999
   10 DMP_ONLIST=.TRUE.
      N_TO_DUMP=EVLIST(3,I)
      NDUMPED=1
  999 RETURN
C
C
      ENTRY DMPLST(EVENT_TO_DUMP,IER)
C
      IER=0
      IF(NLIST.LT.NELIST) THEN
        NLIST=NLIST+1
        EVLIST(1,NLIST)=EVENT_TO_DUMP(1)  ! run number
        EVLIST(2,NLIST)=EVENT_TO_DUMP(2)  ! event id
        EVLIST(3,NLIST)=EVENT_TO_DUMP(3)  ! number of consecutive events
      ELSE
        IER=NELIST
      ENDIF
C
      RETURN
      END
