      LOGICAL FUNCTION TOP_LEPTONS_GOOD_MUON(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 44 of
C-                         PMUO to see if this is a good muon
C-                         track candidate.
C-
C-   Returned value  : 
C-              GOOD_MUON = True/False for Good/Poor Candidate
C-   Inputs  : 
C-              LPMUO - PMUO Bank pointer   
C-   Outputs : 
C-              None
C-   Controls: 
C-              None
C-
C-   Created  14-SEP-1992   Stephen J. Wimpenny
C-   Modified 15-Mar-1993   Name changed for library compatibility
C-   Modified  1-Apr-1994   Updated for new version of CLEANMU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DIAGNOSTIC_DUMPS
C
      INTEGER LPMUO,ITEST,JBIT,IER
C
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
C
C *** Read Diagnostic Dump flag from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_DIAGNOSTIC_DUMPS',DIAGNOSTIC_DUMPS,IER)
        IF(IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     1    'TOP_LEPTONS_GOOD_MUON',' ','F')
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      TOP_LEPTONS_GOOD_MUON=.FALSE.
C 
      IF(DIAGNOSTIC_DUMPS) THEN
        TOP_LEPTONS_GOOD_MUON=.TRUE.
        GO TO 999
      ENDIF
C
      IF(LPMUO.GT.0) THEN
C
C *** Check on Bank version
C
        IF(IQ(LPMUO+1).GE.3) THEN
C
C *** Versions 3 and higher - test for Golden and/or Silver Muon
C ***                         quality
C
         ITEST=JBIT(IQ(LPMUO+45),26)+JBIT(IQ(LPMUO+45),27)
         IF(ITEST.GT.0) TOP_LEPTONS_GOOD_MUON=.TRUE.
        ELSE
C
C *** Versions 1 and 2 - test that global fail bit is not set
C
         ITEST=JBIT(IQ(LPMUO+44),17)
         IF(ITEST.LT.1) TOP_LEPTONS_GOOD_MUON=.TRUE.
       ENDIF
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
