      SUBROUTINE CHECK_RUNNUM(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECK ON LOCAL AND GLOBAL RUN NUMBERS TO MAKE
C-                         SURE WE DON'T HAVE EVENTS FROM PREVIOUS RUN
C-
C-   Inputs  :
C-   Outputs : IERR =0,1 if okay,bad event
C-   Controls:
C-
C-   Created  28-NOV-1990   Kathy Streets
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HEADER.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAD1.LINK/LIST'
C
      INTEGER LRUNNUM,GRUNNUM,IERR,ICNT
      CHARACTER*80 MSG_STRING
C
      DATA ICNT/0/
C----------------------------------------------------------------------
C
      LRUNNUM = IQ(LHEAD+6)
      GRUNNUM = IQ(LHEAD+12)
C
      IERR=0
      IF (LRUNNUM.GT.GRUNNUM)THEN
        IERR=1
        ICNT=ICNT+1
        WRITE(MSG_STRING,100)ICNT,LRUNNUM,GRUNNUM
        CALL INTMSG(MSG_STRING)
      ENDIF
C
C
  100 FORMAT(' BAD EVENT #, LOCAL,GLOBAL RUN # = ',I6,2I10)
C
  999 RETURN
      END

