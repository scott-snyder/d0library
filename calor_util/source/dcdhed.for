      SUBROUTINE DCDHED(WORD,LV1,SNC,NCD,CRT,PDS,RNM,ZER,MDE)
C---------------------------------------------------------------------
C-                                                                   -
C-     Routine to decode an ADC header word into components          -
C-                                                                   -
C-     INPUT:                                                        -
C-     WORD(1) = SYNC word                                           -
C-     WORD(2) = CONTROLLER word                                     -
C-                                                                   -
C-     OUTPUT:                                                       -
C-     LV1 - level 1
C-     SNC - should have all bits on
C-     NCD - number of ADC cards
C-     CRT - crate number
C-     PDS - pedestal subtracted
C-     RNM - renormalized
C-     ZER - zero suppressed
C-     MDE - mode
C-                                                                   -
C-                        AZ Nov,1986                                -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER WORD(2),WRD,LV1,SNC,NCD,CRT,PDS,RNM,ZER,MDE
      INTEGER I
      INTEGER JBIT,JBYT,IBITS
C
C--   for JBIT  bit=1 the least significant
C--   for IBITS bit=0 the least significant
C                             
      LV1=IBITS(WORD(1),16,16)
      SNC=IBITS(WORD(1),0,16)
C
      WRD=WORD(2)
      CRT  = IBITS(WRD,24,8)
      NCD  = IBITS(WRD,16,8)
      PDS  = JBIT(WRD,4)
      RNM  = JBIT(WRD,5)
      ZER  = JBIT(WRD,3)
      MDE  = JBYT(WRD,1,2)  
C
  999 RETURN
      END      
