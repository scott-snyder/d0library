      SUBROUTINE GOOD_MUON_CHECK(LPMUO, GOOD_MUON1, GOOD_MUON2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 44 of
C-                         PMUO to see if this is a good muon
C-                         track candidate.
C-
C-   Returned value  : GOOD_MUON = True/False for Good/Poor Candidate
C-   Inputs  : LPMUO - PMUO Bank pointer
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-SEP-1992   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL GOOD_MUON1, GOOD_MUON2
C
      INTEGER LPMUO, JBIT
      INTEGER I20, I21, I22, I23, I26, I27, I29, I30, I31
C
      GOOD_MUON1=.FALSE.
      GOOD_MUON2=.FALSE.
C
      IF(LPMUO.GT.0) THEN
C
        I20 = JBIT(IQ(LPMUO+44), 20)
        I21 = JBIT(IQ(LPMUO+44), 21)
        I22 = JBIT(IQ(LPMUO+44), 22)
        I23 = JBIT(IQ(LPMUO+44), 23)
        I26 = JBIT(IQ(LPMUO+44), 26)
        I27 = JBIT(IQ(LPMUO+44), 27)
        I29 = JBIT(IQ(LPMUO+44), 29)
        I30 = JBIT(IQ(LPMUO+44), 30)
C
        GOOD_MUON1 = (I20 .EQ. 0) .AND. (I22 .EQ. 0) .AND. (I26 .EQ. 0)
     &         .AND. (I27 .EQ. 0) .AND. (I29 .EQ. 0) .AND. (I30 .EQ. 0)
     &         .AND. (I21 .EQ. 0 .OR. I23 .EQ. 0) 
        GOOD_MUON2 = (I20 .EQ. 0) .AND. (I22 .EQ. 0) .AND. (I27 .EQ. 0)
     &         .AND. (I31 .EQ. 0) .AND. (I30 .EQ. 0) 
     &         .AND. (I21 .EQ. 0 .OR. I23 .EQ. 0)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
