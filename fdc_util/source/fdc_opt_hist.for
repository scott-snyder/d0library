      SUBROUTINE FDC_OPT_HIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book optional histograms
C-
C-   Inputs  : None
C-   Outputs : None
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EXM_MENU_FILES.INC/LIST'
C
      LOGICAL INTAST
      CHARACTER*20 PROC_TITLE
      DATA PROC_TITLE/' PROCESSING       '/
      CHARACTER*12 COMMAND
      CHARACTER*30 TITLE
      CHARACTER*80 MSG_STRING
      CHARACTER*1 ANS
C
      INTEGER ID
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER TRULEN
      LOGICAL FDBHST_PH,FDBHST_DT,FDBHST_PF
      LOGICAL HEXIST
      EXTERNAL PROC_DISPATCH
C----------------------------------------------------------------------
C
C
    1 CONTINUE
      TITLE = ' FDC OPTIONAL HISTOGRAMS '
      CALL MENUDO(TITLE,'FDC_HIST',COMMAND)
C
      IF (COMMAND.EQ.' ') GO TO 1
C
      IF (COMMAND(1:12).EQ.'PULSE HEIGHT') THEN
        CALL FGET_INFO(HALF,UNIT,QDRT,SCTR,WIRE)
        IF (HALF.GE.0) THEN
          IF (FDBHST_PH(HALF,UNIT,QDRT,SCTR,WIRE)) CONTINUE
        END IF
        GO TO 1
      ENDIF
C
      IF (COMMAND(1:10).EQ.'DRIFT TIME') THEN
        CALL FGET_INFO(HALF,UNIT,QDRT,SCTR,WIRE)
        IF (HALF.GE.0) THEN
          IF (FDBHST_DT(HALF,UNIT,QDRT,SCTR,WIRE)) CONTINUE
        END IF
        GO TO 1
      ENDIF
C
      IF (COMMAND(1:9).EQ.'PEAK FADC') THEN
        CALL FGET_INFO(HALF,UNIT,QDRT,SCTR,WIRE)
        IF (HALF.GE.0) THEN
          IF (FDBHST_PF(HALF,UNIT,QDRT,SCTR,WIRE)) CONTINUE
        END IF
        GO TO 1
      ENDIF
C
      IF (COMMAND(1:11).EQ.'DELETE HIST') THEN
        CALL GETPAR(1,' Enter id of histogram to delete > ','I',ID)
        IF (ID.GT.0) THEN
          IF (HEXIST(ID)) THEN
            CALL HDELET(ID)
          ELSE
            CALL INTMSG(' Histogram does not exist.')
          END IF
        END IF
        GO TO 1
      ENDIF
C
  101 IF (INTAST()) THEN
        CALL CANMEN
        CALL INTMEN(PROC_TITLE,MENUFILE(2) (1:TRULEN(MENUFILE(2))),
     &    PROC_DISPATCH)
      ENDIF
C
  999 RETURN
      END
