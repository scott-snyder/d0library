      LOGICAL FUNCTION GOOD_JET(LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 15 of
C-                         LJETS to see if this is a good Jet
C-
C-   Returned value  : 
C-              GOOD_JET = True/False for Good/Poor Candidate
C-   Inputs  : 
C-              LJETS - JETS Bank pointer   
C-   Outputs : 
C-              None
C-   Controls: 
C-              None
C-
C-   Created  24-SEP-1992   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LJETS,IFLAG(16),ITEST,JBIT
C
      CALL VZERO(IFLAG,16)
      GOOD_JET=.FALSE.
C
      IF(LJETS.GT.0) THEN
C
C *** Check Global Selection Bit from TOP_LEPTONS_JET_SELECT
C
       ITEST=JBIT(IQ(LJETS+15),17)
       IF(ITEST.LT.1) GOOD_JET=.TRUE.
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
