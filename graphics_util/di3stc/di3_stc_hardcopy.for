      SUBROUTINE STC_HARDCOPY(ID)
C----------------------------------------------------------------------
C-   Purpose and Methods : Make a copy to device 2 of plot ID (0 = all)
C-   Created  13-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
C----------------------------------------------------------------------
      IDEV=2
      DO 10 NID=1,NCHARTS
        IDNOW=IDCHRT(NID)
        IF(ID.GT.0.AND.ID.NE.IDNOW) GO TO 10
        INPROG=0
        CALL STC_PLOT(ID)
        CALL JDEVOF(IDEV)
        CALL JDEND(IDEV)
   10 CONTINUE
      IDEV=1
        
  999 RETURN
      END
