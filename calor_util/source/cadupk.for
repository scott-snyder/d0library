      SUBROUTINE CADUPK(ICRATE,IDATA,CRATE,ADC,BLS,ROTOW,DEPTH,SCALE,
     &  NEGLIM)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Convert address part of raw calorimeter data
C                         word to electronics address. The ADC crate 
C                         number is passed from an input to output
C                         argument unaltered. The flags in the address
C                         word that affect data interpretation are also
C                         decoded and returned in the argument list.
C
C   Inputs  : ICRATE   ADC crate #
C             IDATA    raw data word from CADx
C
C   Outputs : CRATE    ADC crate # (= ICRATE!)
C             ADC      ADC card #
C             BLS      BLS card #
C             ROTOW    readout tower #
C             DEPTH    depth (hardware! 0-11)
C             SCALE    = 0 ==> X8; = 1 ==> X1
C             NEGLIM   = 1 ==> negative limit set: channel will be read
C                      out even in zero-suppressed mode
C
C   Created  25-OCT-1988   Wyatt Merritt
C
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER ICRATE,IDATA,CRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM
      INTEGER ADR
      INTEGER JBIT,JBYT
C----------------------------------------------------------------------
C
C
C       Bit fields in ADR (high order 16 bits of IDATA):
C         (REF. D0 notes 678, 774)
C
C  16  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1 
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
C |        ADC        |    BLS    | ROTOW |     DEPTH     |SCL|NEG|
C |                   |           |       |               |   |LIM|
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---| 
C
C   For JBIT and JBYT,  the least significant bit is called bit 1.
C                             
C----------------------------------------------------------------------
      CRATE = ICRATE
C
      ADR = ISHFT(IDATA,-16)
C
      NEGLIM = JBIT(ADR,1)
      SCALE = JBIT(ADR,2)
      DEPTH = JBYT(ADR,3,4)  
      ROTOW = JBYT(ADR,7,2)  
      BLS = JBYT(ADR,9,3) 
      ADC = JBYT(ADR,12,5)
C
  999 RETURN
      END      
