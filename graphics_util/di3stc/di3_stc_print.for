      SUBROUTINE STC_PRINT(IUNIT,ID,ITRANG)
C----------------------------------------------------------------------
C-   Purpose and Methods : Print the contents of stripchart ID (0=all)
C-      to unit IUNIT.  ITRANG - 0=all times, 1=current chart range.
C-   Created  11-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
C----------------------------------------------------------------------
      DO 10 I=1,NCHARTS
        IDNOW=IDCHRT(I)
        IF((ID.GT.0).AND.(ID.NE.IDNOW)) GO TO 10
C        WRITE(IUNIT,20) ....
C.....
   10 CONTINUE
C
  999 RETURN
      END
