      SUBROUTINE ZSHOW_PULSER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the selected pulser pattern to the
C-                         screen.
C-
C-   Inputs  : none, inputs from common block
C-   Outputs : none
C-   Controls: none
C-
C-   Created   1-NOV-1990   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZPULSER.INC'
C
      CHARACTER*6 QUAD
      CHARACTER*5 HALF
      CHARACTER*8 APOLARITY
      CHARACTER*7 APREAMP
      CHARACTER*80 MSG
C
C----------------------------------------------------------------------
C
      IF (QUADRANT.EQ.0) QUAD = 'NO    '
      IF (QUADRANT.EQ.1) QUAD = 'TOP   '
      IF (QUADRANT.EQ.2) QUAD = 'BOTTOM'
      IF (QUADRANT.EQ.3) QUAD = 'ALL   '
C
      HALF = 'Upper'
      IF (SHHALF.EQ.1) HALF = 'Lower'
C
      APOLARITY = 'Negative'
      IF (POLARITY.EQ.1) APOLARITY = 'Positive'
C
      APREAMP = 'Preamps'
      IF (PREAMP.EQ.1) APREAMP = 'Shapers'
C
      CALL INTMSG(' ')
      IF (.NOT.GOT_TARGET) THEN
        CALL INTMSG(' No Pulser has been selected ')
      ELSE
        WRITE(MSG,10)TARGET
        CALL INTMSG(MSG)
      ENDIF
      WRITE(MSG,11)AMPLTDA,AMPLTDB
      CALL INTMSG(MSG)
      WRITE(MSG,12)QUAD,HALF,SHCARD
      CALL INTMSG(MSG)
      WRITE(MSG,13)APOLARITY,APREAMP
      CALL INTMSG(MSG)
      CALL INTMSG(' ')
C
   10 FORMAT(' Pulser selected is ',A12)
   11 FORMAT(' Amplitude of Test Pulse A = ',I3.3,' Pulse B = ',I3.3)
   12 FORMAT(1X,A6,' channels in ',A5,' half of Shaper Card ',I2,
     &  ' selected')
   13 FORMAT(' Test Pulse polarity is ',A8,' fed through ',A7)
C
  999 RETURN
      END
