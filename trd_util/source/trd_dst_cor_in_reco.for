      SUBROUTINE TRD_DST_COR_IN_RECO
     &  (LTRDT,VERSION,CORRECTION,ENERGY,DIAGNOSTIC_COR,RW,IW,EPICOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD_DST_COR for RECO jobs only
C-
C    Inputs  :
C       LTRDT         integer      LINK to TRDT
C
C    Outputs :
C       VERSION       real        version of TPRL
C       CORRECTION    logical(10) status word for corrections
C                                 (TRUE=done, FALSE=problem)
C                                 CORRECTION(1)  electronic gain correction
C                                 CORRECTION(2)  EPICOR calibration
C                                 CORRECTION(3)  additive pedestal correction
C                                 CORRECTION(4)  pedestal substraction
C                                 CORRECTION(5)  sector corection
C                                 CORRECTION(6)  wire correction
C                                 CORRECTION(7)  high voltage correction
C                                 CORRECTION(8)  angular correction
C                                 CORRECTION(9)  gas correction
C                                 CORRECTION(10) CD multiplicity correction
C       ENERGY         real(5)    energies in MIP
C                                 ENERGY(1) energy layer 1
C                                 ENERGY(2) energy layer 2
C                                 ENERGY(3) energy layer 3
C                                 ENERGY(4) total energy
C                                 ENERGY(5) truncated energy
C       DIAGNOSTIC_COR integer    coded word for special cases
C                                 0               <=> OK
C                                 bit 1 (LSB) = 1 <=> canary out of bounds
C                                 bit 2       =1  <=> wrong status word
C       RW             real(3,100)     output of UNPACK_TPRL for 3 layers
C       IW             integer(3,100)  output of UNPACK_TPRL for 3 layers
C       EPICOR         real(3)    EPICOR for layers 1,2,3
C
C    Controls: TRD_ANALYSIS.RCP,TRD_RCP
C-   Created  16-JUN-1994   Alain PLUQUET form TRD_DSt_COR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INTEGER LAYER,I,LTRDT,LTPRL
      LOGICAL CORRECTION(10)
      INTEGER DIAGNOSTIC_COR,IER,JBIT
      REAL VERSION
      REAL ENERGY(5),EPICOR(3)
      REAL RW(3,NWORD)
      INTEGER IW(3,NWORD)

      CALL VZERO(IW,3*NWORD)
      CALL VFILL(RW,3*NWORD,0.)
      CALL VFILL(EPICOR,3,0.)
      CALL VFILL(ENERGY,5,0.)
      VERSION=0.
      DIAGNOSTIC_COR=0
      DO LAYER=1,3
        CALL VZERO(INTEGER_WORD,NWORD)
        CALL VFILL(REAL_WORD,NWORD,0.)
        LTPRL=LQ(LTRDT-LAYER)
        IF (LTPRL.GT.0) THEN
          CALL UNPACK_TPRL (LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
          DO I=1,10
            CORRECTION(I)=JBIT(IQ(LTPRL+4),I).EQ.1
          ENDDO
          DO I=1,NWORD
            IW(LAYER,I)=INTEGER_WORD(I)
            RW(LAYER,I)=REAL_WORD(I)
          ENDDO
          ENERGY(LAYER)=Q(LTPRL+12)
          EPICOR(LAYER)=REAL_WORD(1)
        ENDIF 
      ENDDO
      ENERGY(4)=ENERGY(1)+ENERGY(2)+ENERGY(3)
      ENERGY(5)=ENERGY(4)-MAX(ENERGY(1),ENERGY(2),ENERGY(3))
      END
