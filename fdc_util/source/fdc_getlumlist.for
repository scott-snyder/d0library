      SUBROUTINE FDC_GETLUMLIST(RUN,LUM_ADJ,ENTRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given run, select correct set of luminosity
C-      correction parameters.
C-
C-   Inputs  : RUN
C-   Outputs : LUM_ADJ
C-
C-   Created   9-APR-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Input:
      INTEGER RUN
C  Output:
      REAL    LUM_ADJ(0:1,0:1)
C  Local:
      INTEGER WDS_ENTRY
      PARAMETER( WDS_ENTRY = 6 )
      INTEGER MAX_ENTRIES
      PARAMETER( MAX_ENTRIES = 5 )
C
      INTEGER I,ERR,N_VAL
      INTEGER ENTRY,ENTRY_LAST,N_ENTRIES
      INTEGER UNIT, HALF 
      INTEGER RUN_MIN(MAX_ENTRIES),RUN_MAX(MAX_ENTRIES)
      REAL    LUM_LIST(MAX_ENTRIES,0:1,0:1)
      LOGICAL FIRST
C
      SAVE FIRST,N_ENTRIES,LUM_LIST,ENTRY_LAST,RUN_MIN,RUN_MAX
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETA ('LUM_ADJ',0,0,0,N_VAL,ERR)
        CALL EZGETA ('LUM_ADJ',1,N_VAL,WDS_ENTRY,RUN_MIN,ERR)
        CALL EZGETA ('LUM_ADJ',2,N_VAL,WDS_ENTRY,RUN_MAX,ERR)
        CALL EZGETA ('LUM_ADJ',3,N_VAL,WDS_ENTRY,LUM_LIST(1,0,0),ERR)
        CALL EZGETA ('LUM_ADJ',4,N_VAL,WDS_ENTRY,LUM_LIST(1,1,0),ERR)
        CALL EZGETA ('LUM_ADJ',5,N_VAL,WDS_ENTRY,LUM_LIST(1,0,1),ERR)
        CALL EZGETA ('LUM_ADJ',6,N_VAL,WDS_ENTRY,LUM_LIST(1,1,1),ERR)
        N_ENTRIES  = N_VAL/WDS_ENTRY
        CALL EZRSET
        IF ( ERR.NE.0 ) THEN
          N_ENTRIES=0
        ENDIF
      ENDIF
C
      ENTRY=0
      DO I =  1, N_ENTRIES
        IF ((RUN.GE.RUN_MIN(I)).AND.(RUN.LE.RUN_MAX(I))) ENTRY = I
      ENDDO
C
      IF (ENTRY.NE.ENTRY_LAST ) THEN
        ENTRY_LAST = ENTRY
        DO UNIT =  0, 1
          DO HALF =  0, 1
            IF ( ENTRY.EQ.0 ) THEN
              LUM_ADJ(HALF,UNIT)  = 0.0
            ELSE
              LUM_ADJ(HALF,UNIT)  = LUM_LIST(ENTRY,HALF,UNIT) 
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
