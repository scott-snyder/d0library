      SUBROUTINE HOTTEST_EM3_CELL(LCASH,Neta,Nphi,Nlyr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retruns the address of the hottest em3 cell in a
C-   cash of cawx bank.
C-
C-   Inputs  : Pointer to CASH (or CAWx)
C-   Outputs : IETA,IPHI, and ILYR of hottest em3 cell
C-   Controls:
C-
C-   Created   8-AUG-1995   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCASH
      INTEGER I,NCELLS,PAKADR,POINTER,IETA,IPHI,LAYER
      INTEGER LR, Neta,Nphi,Nlyr
      REAL ENERGY,EMAX
      LOGICAL EM3
C
C----------------------------------------------------------------------
C
C
      EM3(LR) = ((lr.GE.lyem3a).AND.(lr.LE.lyem3d))
      ncells = iq(lcash+2)
      pointer=1
      emax = -999.
      Neta = 0
      Nphi = 0
      Nlyr = 0
C
C ****  loop over cells..
C
      DO 100 i = 1,ncells
        pointer = pointer+2
        pakadr = iq(lcash+pointer)
        energy = q(lcash+pointer+1)
        CALL CAEP_INDICES(PAKADR,IETA,IPHI,LAYER)
        IF (EM3(layer).AND.(energy.GT.emax)) THEN
          emax = energy
          neta = ieta
          nphi = iphi
          nlyr = layer
        ENDIF
  100 CONTINUE
  999 RETURN
      END
