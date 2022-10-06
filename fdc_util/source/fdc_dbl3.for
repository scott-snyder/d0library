C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_DBL3.FOR
C *1     4-NOV-1993 10:52:36 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_DBL3.FOR
      SUBROUTINE FDC_DBL3(RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call FDC DBL3 routines.
C-
C-   Inputs  : RUN
C-   Outputs : none
C-
C-   Created   5-MAY-1993   Robert E. Avery
C-   Updated  18-OCT-1993   Robert E. Avery  Allow changes to constants
C-              for MC data also (global t0 shift, and/or testing of DBL3)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Input:
C
      INTEGER RUN
C
C  Local:
C
      INTEGER RUNTYPE,IER,PREV_RUN
      LOGICAL OK, MCDATA, FIRST, BYPASS_DBL3_ERROR
C
      SAVE FIRST, RUNTYPE, BYPASS_DBL3_ERROR, PREV_RUN 
C
      DATA FIRST/.TRUE./
      DATA PREV_RUN /-1/
C----------------------------------------------------------------------
C      MCDATA =  IQ(LHEAD+1) .GT. 1000
C      IF (MCDATA) GOTO 999
      IF (RUN.EQ.PREV_RUN) GOTO 999
      PREV_RUN = RUN
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
        CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
      ENDIF
C
      CALL FDBINI(RUN,OK)
      IF (.NOT.OK) THEN
        IF (BYPASS_DBL3_ERROR) THEN
          CALL INTMSG(' FDC_DBL3: Error in FDBINI.')
        ELSE
          CALL ERRMSG('FTRAKS','FDC_DBL3',
     &        'Error updating STP banks from DBL3','F')
          GO TO 999
        END IF
      END IF
C
      IF (RUNTYPE.LE.2) CALL FTIMEP() ! Find location of timing pulse
C
  999 RETURN
      END
