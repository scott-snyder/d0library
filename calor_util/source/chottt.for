      SUBROUTINE CHOTTT(IHOTINDX1,L1IPHI,L1IETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert hot tower index to lv1 phi,eta indices
C-
C-   Inputs  : IHOTINDX1 - Relative hot tower address: 0 to
C-                         (2*NETAL1)*(NPHIL1)*(2)
C-   Outputs : L1IPHI    - L1 phi index: 1 to NPHIL1
C-             L1IETA    - L1 eta index: -NETAL1 to NETAL1 excluding 0
C-   Controls:
C-
C-   Created   5-FEB-1990   Dale A. Ross (MSU)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INTEGER IHOTINDX,L1IETA,L1IPHI,IHOTINDX1
      INTEGER IZ,IE,IP,IAD,IWORD,LINK,IBYTE,IETA,IPHI
      LOGICAL BTEST
C----------------------------------------------------------------------
      IHOTINDX = IHOTINDX1
      IAD = IHOTINDX
C---IAD is the relative position of this LV1 tower in the LV1 energy list
C---relative to EMTT(1,1) See D0note 967
C---***So IAD = 0 means we are at EMTT(1,1), IAD = 1 -->EMTT(1,17)
C---There are NETAL1*NPHIL1 such addresses in the positive eta section so
C---the addresses range from 0 to N-1   (+ eta EM)
C---                         N to 2*N-1 (- eta EM)
C---                       2*N to 3*N-1 (+eta HAD)
C---                       3*N to 4*N   (-eta HAD)
C---   where N = NETAL1*NPHIL1
C---Since we only want to return the eta,phi indices, we dont care whether
C---we are in the HAD or EM part
      IF (IAD .GE. 2*NETAL1*NPHIL1) IAD = IAD - 2*NETAL1*NPHIL1
C---Find out whether we are in positive or negative eta block
      IZ = 1                            ! Assume we are positive eta
      IF (IAD .GE. NETAL1*NPHIL1) THEN
        IZ = -1
        IAD = IAD - NETAL1*NPHIL1
      END IF
C---IAD is now in the first block, whether it was originally or not. But
C---remember, IAD is simply the relative positon of the tower with respect
C---to EMTT(+1,1), so if IAD is even PHI is from 1-16 else it is from
C---17-32
      IPHI = 0
      IF (BTEST( IAD, 0)) IPHI = 16

C---Now we must find out which word our byte is in. Remeber that the
C---first two towers have IAD = 0 and 1. So our byte is in word
      IWORD = (IAD)/2 + 1

C---Now the first 20*16 words have the formula 16e + p. So
C---IWORD = (IE-1)*16 + IP
      IE = (IWORD-1)/16 + 1
      IP = IWORD - (IE-1)*16

C---Now convert to IETA,IPHI
      IETA = IE*IZ
      IPHI = IP + IPHI

C---Now set the indices equal to our output variables:
      L1IETA = IETA
      L1IPHI = IPHI

  999 RETURN
      END
