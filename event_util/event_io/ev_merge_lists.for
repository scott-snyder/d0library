      PROGRAM EV_MERGE_LISTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      merge lists of events into one file EVENT_LIST
C-      must supply file FILE_NAMES with list
C-      of files containing event lists
C-
C-   Created  23-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*80 LIST
      INTEGER K,L,TRULEN,IER,LUN1,LUN2
      INTEGER RUN_READ,NEVT_READ,EVENT_NOS(1000)
      LOGICAL OK
C----------------------------------------------------------------------
C
      CALL GTUNIT(112,LUN1,IER)
      CALL D0OPEN(LUN1,'FILE_NAMES','IF',OK)
C
      IF(.NOT.OK) THEN
        PRINT *,' CANNOT OPEN FILE_NAMES, JOB TERMINATED'
        PRINT *,' CANNOT OPEN FILE_NAMES, JOB TERMINATED'
        STOP
      ENDIF
C
   1  CONTINUE
      READ(LUN1,100,END=999) LIST
      L=TRULEN(LIST)
      CALL GTUNIT(112,LUN2,IER)
      CALL D0OPEN(LUN2,LIST(1:L),'IF',OK)
C
      IF(.NOT.OK) THEN
        PRINT *,' CANNOT OPEN ',LIST(1:L),' file skipped'
        CLOSE(LUN2)
        CALL RLUNIT(112,LUN2,IER)
        GOTO 1
      ENDIF
    2 READ(LUN2,*,END=11) RUN_READ,NEVT_READ
      READ(LUN2,*) (EVENT_NOS(K),K=1,NEVT_READ)
      DO  K=1,NEVT_READ
        CALL EV_ADD_TO_LIST(RUN_READ,EVENT_NOS(K))
      ENDDO
      GOTO 2
   11 CONTINUE
      CLOSE(LUN2)
      CALL RLUNIT(112,LUN2,IER)
      GOTO 1
 999  CALL EV_WRITE_LIST
      STOP
 100  FORMAT(A)
C
      END
