      SUBROUTINE STC_REWINDOW(ID,WNDNEW)
C----------------------------------------------------------------------
C-   Purpose and Methods : Allow for window to be modified after
C-     a plot has been booked.
C-   Inputs  : ID - plot identifier, WNDNEW(4) - new window 
C-   Created  13-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      REAL WNDNEW(4)
C----------------------------------------------------------------------
      NID=INDEXC(ID)
      IF(NID.LT.1) RETURN
      IF(NID.GT.NCHARTS) RETURN
      DO 10 I=1,4
        WNDO(I,NID)=WNDNEW(I)
   10 CONTINUE
  999 RETURN
      END
