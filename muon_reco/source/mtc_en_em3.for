      SUBROUTINE MTC_EN_EM3(IETA,IPHI,ENERGY,IER_CAEP)
C----------------------------------------------------------------------
C- MTC_EN_EM3: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Find the total energy (ENERGY) deposited in
C-   EM3 pads 3,4,5 and 6 at the input IETA,IPHI
C-
C-   Inputs  : ieta,iphi
C-   Outputs : ENERGY - the energy in EM3 at this ieta,iphi
C-             IER_CAEP =  0 if all is ok
C-                      = -5 if no energy was found
C-
C-   Created  23-JUN-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      INTEGER ieta,iphi
C- output
      REAL    energy
      INTEGER ier_caep
C- local
      REAL    esum3
      INTEGER ilyr3,ihere3
C- fnction
      INTEGER mtc_iwhere
ccC- for calling GTCAEH_ADDR ...
cc      REAL EX,EY,EZ,ET_CAEH,SEX,SEY,CWEIGHT
cc      INTEGER STATUS,IER_CAEH
C----------------------------------------------------------------------
      ESUM3 = 0.
      DO 115 ILYR3=3,6
        IHERE3 = MTC_IWHERE(IETA,IPHI,ILYR3)
        IF(IHERE3.EQ.0) GO TO 115
        ENERGY = 0.

cc        CALL GTCAEH_ADDR(IETA,IPHI,ILYR3,EX,EY,EZ,ENERGY,ET_CAEH,
cc     &          SEX,SEY,CWEIGHT,STATUS,IER_CAEH)

        call gtcaep_addr(ieta,iphi,ilyr3,energy,ier_caep)
        IF(IER_CAEP.EQ.0) ESUM3 = ESUM3 + ENERGY
  115 CONTINUE
      IF(ESUM3.GT.0.) THEN
        IER_CAEP = 0
        ENERGY   = ESUM3
      ELSE
        IER_CAEP = -5
        ENERGY = 0.
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
