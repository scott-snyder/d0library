      LOGICAL FUNCTION TOP_LEPTONS_GOOD_JET(LJETS)
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
C-   Modified  4-Feb-1993   Diagnostic_Dump skip added
C-   Modified 15-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DIAGNOSTIC_DUMPS
C
      INTEGER LJETS,ITEST,JBIT,IER
C
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
C
C *** Read Diagnostic Dump flag from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_DIAGNOSTIC_DUMPS',DIAGNOSTIC_DUMPS,IER)
        IF(IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     1    'TOP_LEPTONS_GOOD_JET',' ','F')
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      TOP_LEPTONS_GOOD_JET=.FALSE.
C
      IF(DIAGNOSTIC_DUMPS) THEN
        TOP_LEPTONS_GOOD_JET=.TRUE.
        GO TO 999
      ENDIF
C
      IF(LJETS.GT.0) THEN
C
C *** Check Global Selection Bit from TOP_LEPTONS_JET_SELECT
C
        ITEST=JBIT(IQ(LJETS+15),17)
        IF(ITEST.LT.1) TOP_LEPTONS_GOOD_JET=.TRUE.
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
