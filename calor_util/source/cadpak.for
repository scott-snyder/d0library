      SUBROUTINE CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,ADR)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Inverse of CADUPK; packs a scale bit, limit
C                         bit, and ADC address into 16 bits that mimic
C                         the raw data format.
C
C   Inputs  : ADC      ADC card #
C             BLS      BLS card #
C             ROTOW    readout tower #
C             DEPTH    depth (hardware! 0-11)
C             SCALE    = 0 ==> X8; = 1 ==> X1
C             NEGLIM   = 1 ==> negative limit set: channel will be read
C                      out even in zero-suppressed mode
C
C   Outputs : ADR      16-bit address string in CADx format 
C
C   Created  16-NOV-1988   K. Wyatt Merritt
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,ADR
C----------------------------------------------------------------------
C
C
C       Bit fields in ADR (high order 16 bits of raw data word):
C         (REF. D0 notes 678, 774)
C
C  16  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1 
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
C |        ADC        |    BLS    | ROTOW |     DEPTH     |SCL|NEG|
C |                   |           |       |               |   |LIM|
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---| 
C
C   For SBIT and SBYT,  the least significant bit is called bit 1.
C                             
C----------------------------------------------------------------------
C
      ADR = 0
C
      CALL SBIT(NEGLIM,ADR,1)
      CALL SBIT(SCALE,ADR,2)
      CALL SBYT(DEPTH,ADR,3,4)
      CALL SBYT(ROTOW,ADR,7,2)
      CALL SBYT(BLS,ADR,9,3)
      CALL SBYT(ADC,ADR,12,5)
C
  999 RETURN
      END
