      SUBROUTINE PRCPB8(IUNIT,ICARD,NBAD,BAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bad channel bank, 
C-                            for card ICARD, BANK = CPD8
C-
C-   Inputs  : IUNIT    = Unit number for output
C-             ICARD    = ADC card number
C-             NBAD     = number of bad channels found
C-             BAD(384) = flags for all possible channels (0->good)
C-   Outputs :
C-
C-   Created   1-FEB-1989   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUNIT
      INTEGER ICARD,NBAD,BAD(384)
      INTEGER ADC,BLS,TWR,LYR,CHAN
      INTEGER JBIT,J,K,IBAD,KBAD,LSTBAD(16)
C----------------------------------------------------------------------
      WRITE(IUNIT,210)ICARD,NBAD
  210 FORMAT(/,5X,' BAD CHANNEL LIST FOR BANK CPB8, ADC CARD ',I3,/,
     1  5X,' Number of bad channels in this bank is ',I7,/,
     2  5X,' Channels with flag 16 alone are not printed')
C
      KBAD = 0                          ! Number of bad channels for
C                                       ! this card (excluding flag 16)
      DO 100, CHAN=1,384
      BLS=(CHAN-1)/48
      TWR=((CHAN-1)-48*BLS)/12
      LYR=(CHAN-1)-48*BLS-12*TWR
      IBAD = 0
      DO 150 J = 1,16
        IF(JBIT(BAD(CHAN),J).EQ.0)GO TO 150
        IF(J.EQ.16 .AND. IBAD.EQ.0)THEN
          NBAD=NBAD-1       ! don't count unconnected as bad
          GO TO 100         ! ch. not connected
        ENDIF
        IBAD = IBAD+1
        LSTBAD(IBAD) = J
  150 CONTINUE
      IF (IBAD.NE.0) THEN
        KBAD = KBAD+1
        WRITE(IUNIT,200)ICARD,BLS,TWR,LYR,(LSTBAD(K),K=1,IBAD)
  200   FORMAT(10X,' ADC=',I2,' BLS=',I1,' TWR=',I1,' CHN=',I2,
     1           2X,' Flags ',16I3)
      ENDIF
  100 CONTINUE                          ! DO CHAN
      WRITE(IUNIT,215)KBAD
  215 FORMAT(5X,' Number of bad channels (excluding flag 16) ',I7)
  999 RETURN
      END
