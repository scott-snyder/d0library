      SUBROUTINE FDC_RCPADJ(RUN,GAIN_ADJ,NEW_RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given run, select correct set of gain
C-      correction constants.
C-
C-   Inputs  : RUN
C-   Outputs : GAIN_ADJ
C-
C-   Created   8-APR-1993   Robert E. Avery
C-   Updated  5-MAR-1994    Roy C. Thatcher  Have been getting spurious Doentry lines
C-    Changes local variable ENTRY to ENTRY1
C----------------------------------------------------------------------
      IMPLICIT NONE   
C
C  Input:
      INTEGER RUN
C  Output:
      REAL    GAIN_ADJ(0:1,0:1)
      LOGICAL NEW_RCP
C  Local:
      INTEGER WDS_ENTRY
      PARAMETER( WDS_ENTRY = 6 )
      INTEGER MAX_ENTRIES
      PARAMETER( MAX_ENTRIES = 100 )
C
      INTEGER I,IER,N_VAL
      INTEGER ENTRY1,ENTRY_LAST,N_ENTRIES
      INTEGER UNIT, HALF 
      INTEGER RUN_MIN(MAX_ENTRIES),RUN_MAX(MAX_ENTRIES)
      REAL    GAIN_LIST(MAX_ENTRIES,0:1,0:1)
      LOGICAL FIRST
C
      SAVE FIRST,N_ENTRIES,GAIN_LIST,ENTRY_LAST,RUN_MIN,RUN_MAX
      DATA FIRST /.TRUE./
      DATA ENTRY_LAST /-1/
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETA_i ('GAIN_ADJ',0,0,0,N_VAL,IER)
        CALL EZGETA_iarr ('GAIN_ADJ',1,N_VAL,WDS_ENTRY,RUN_MIN,IER)
        CALL EZGETA_iarr ('GAIN_ADJ',2,N_VAL,WDS_ENTRY,RUN_MAX,IER)
        CALL EZGETA ('GAIN_ADJ',3,N_VAL,WDS_ENTRY,GAIN_LIST(1,0,0),IER)
        CALL EZGETA ('GAIN_ADJ',4,N_VAL,WDS_ENTRY,GAIN_LIST(1,1,0),IER)
        CALL EZGETA ('GAIN_ADJ',5,N_VAL,WDS_ENTRY,GAIN_LIST(1,0,1),IER)
        CALL EZGETA ('GAIN_ADJ',6,N_VAL,WDS_ENTRY,GAIN_LIST(1,1,1),IER)
        N_ENTRIES  = N_VAL/WDS_ENTRY
        CALL EZRSET
        IF ( IER.NE.0 ) THEN
          N_ENTRIES=0
        ENDIF
      ENDIF
C
      ENTRY1 = 0
      DO I =  1, N_ENTRIES
        IF ((RUN.GE.RUN_MIN(I)).AND.(RUN.LE.RUN_MAX(I))) ENTRY1 = I
      ENDDO
C
      NEW_RCP = ENTRY1.NE.ENTRY_LAST
      IF ( NEW_RCP  ) THEN
        ENTRY_LAST = ENTRY1
        DO UNIT =  0, 1
          DO HALF =  0, 1
            IF ( ENTRY1.EQ.0 ) THEN
              GAIN_ADJ(HALF,UNIT)  = 1.0
            ELSE
              GAIN_ADJ(HALF,UNIT)  = GAIN_LIST(ENTRY1,HALF,UNIT) 
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
