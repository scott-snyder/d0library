      SUBROUTINE STC_PLOT_END(ID)
C----------------------------------------------------------------------
C-   Purpose and Methods :  Remove plot ID from display but continue
C-     the accumulation of its traces in STC_FILL
C-   Created  11-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
C----------------------------------------------------------------------
      IF(ID.LT.0.OR.ID.GT.IDMAX) RETURN
      IF(ID.EQ.0) GO TO 100
      NID=INDEXC(ID)
      IF(NID.EQ.0) RETURN
      IF(.NOT.PLOTTD(NID)) RETURN
      CALL JPURGE(NID)
      PLOTTD(NID)=.FALSE.
      RETURN
  100 DO 10 NID=1,NCHARTS
        PLOTTD(NID)=.FALSE.
   10 CONTINUE
      CALL JCLEAR
  999 RETURN
      END
