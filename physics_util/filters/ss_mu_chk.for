      SUBROUTINE SS_MU_CHK(LPMUO,GOOD_MUON1)
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
C-   THIS VERSION FOR SS LEP FILT, 10-JAN-93, JTWHITE
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL GOOD_MUON1
C
      INTEGER LPMUO,JBIT
      INTEGER I20,I21,I22,I23,I25,I26,I27,I28,I29,I30,I31
C
      GOOD_MUON1=.FALSE.
C
      IF(LPMUO.GT.0) THEN
C
       I20 = JBIT(IQ(LPMUO+44),20) ! IFW4 cut
       I21 = JBIT(IQ(LPMUO+44),21) ! Muon-central tracking match
       I22 = JBIT(IQ(LPMUO+44),22) ! Impact parameter wrt CT vertex
       I25 = JBIT(IQ(LPMUO+44),25) ! Cal energy in 0.2 cone around muon
       I26 = JBIT(IQ(LPMUO+44),26) ! MUCTAG opposite track or hits
       I27 = JBIT(IQ(LPMUO+44),27) ! Crossing trigger octant cut
       I28 = JBIT(IQ(LPMUO+44),28) ! Bdl cut
       I29 = JBIT(IQ(LPMUO+44),29) ! Pt cut
       I30 = JBIT(IQ(LPMUO+44),30) ! Eta cut
C
       GOOD_MUON1 = (I20.EQ.0).AND.
     &             ((I21.EQ.0).OR.(I25.EQ.0)).AND.
     &              (I22.EQ.0).AND.
!    &              (I28.EQ.0).AND.
     &              (I29.EQ.0).AND.
     &              (I30.EQ.0)
C       GOOD_MUON2 = (I20.EQ.0).AND.(I22.EQ.0).AND.
C     &   (I27.EQ.0).AND.(I31.EQ.0).AND.(I30.EQ.0).AND.
C     &   (I21.EQ.0 .OR. I23.EQ.0)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
