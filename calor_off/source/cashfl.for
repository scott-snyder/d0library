      SUBROUTINE CASHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills the bank CASH
C-
C-   Inputs  :None
C-
C-   Outputs : CASH bank
C-   Controls: none
C-
C-   Created  25-FEB-1992 15:09:00.17  Norman A. Graf
C-   Updated   2-OCT-1992   Rajendran Raja  removed LCASH declaration
C-   Updated  27-MAY-1993   Marc Paterno    Squash cells found by AIDA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'

      INTEGER  GZCASH, NRCAEP

      INTEGER NCH, I, POINTER, CASHPNTR, PACKED_WORD, IHIT
      REAL ECELL
C----------------------------------------------------------------------
      NRCAEP = IQ(LCAEP+2)
      LCACH  = LQ(LCACL-1)
      NCH    = IQ(LCACH+2)
C
      CALL BKCASH(LCACL, NCH, LCASH)
      IQ(LCASH+1) = 1               ! Bank version
      IQ(LCASH+2) = NCH
C
      CASHPNTR = LCASH+3

      DO I = 1, NCH
        POINTER = (IQ(LCACH+2+I)-1)*NRCAEP+LCAEP
        PACKED_WORD = IQ(POINTER+4)
        ECELL = Q(POINTER+5)
        CALL GTCAID_PACKED_WORD (PACKED_WORD, IHIT)
        IF ( IHIT .GT. 0 ) ECELL = ECELL * HOTSUP
        IQ(CASHPNTR) = PACKED_WORD
        Q(CASHPNTR+1) = ECELL
        CASHPNTR = CASHPNTR + 2
      ENDDO

      RETURN

      END
