      SUBROUTINE STC_OPTIONS(ID,IOPT,STATE)
C----------------------------------------------------------------------
C-   Purpose and Methods : Allow for plot options to be modified after
C-     a plot has been booked
C-   Inputs  : ID - plot identifier, IOPT- option number, 
C-             STATE - true or false
C-   Created  13-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      LOGICAL STATE
C----------------------------------------------------------------------
      NID=INDEXC(ID)
      IF(NID.LT.1) RETURN
      IF(NID.GT.NCHARTS) RETURN
      IF(IOPT.LT.1) RETURN
      IF(IOPT.GT.10) RETURN
      OPTNS(IOPT,NID)=STATE
  999 RETURN
      END
