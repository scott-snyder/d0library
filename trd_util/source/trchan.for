      FUNCTION TRCHAN(ILAYER,IWIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DETERMINE CHANNEL NUMBER FOR FADC DIGITIZATIO
C-                         FOR THE TRD
C-
C-   Inputs  : ILAYER= TRD LAYER (1 to 6)
C-             IWIRE = HIT WIRE  (1-->256)
C-   Outputs :
C-
C-   Created  12-JAN-1988   A. ZYLBERSTEJN
C-   Updated  26-APR-1989   A. Zylberstejn  modified to deal with cathodes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICR,ILAYER,IMODU,ITRIP,IWIRE,TRCHAN,ICHAM
      INTEGER IADDR,IFOIS,ISUBAD,IOFF
C      INCLUDE 'D0$INC:D0LOG.INC'
C      INCLUDE 'D0$INC:GCUNIT.INC'
C
      DATA IFOIS/0/

      IFOIS=IFOIS+1
C
C
C CHAMBER NUMBER
      IF(ILAYER.LE.3)THEN
        ICHAM=ILAYER
        IOFF=0
      ELSE
        ICHAM=ILAYER-3
        IOFF=768
      END IF
C  MODULE NUMBER                                 0 TO 15
      IMODU=(IWIRE-1)/16
C  CRATE NUMBER                                  0 TO  3
      ICR=IMODU/4
C  TRIPLET NUMBER IN THE CRATE                   0 TO  3
      ITRIP=IMODU-ICR*4
C  ADDRESS OF THE FADC MODULE IN THE CRATE       0 TO 11
      IADDR=ITRIP*3+ICHAM-1
C  SUB_ADDRESS OF THE CHANNEL IN THE FADC MODULE 0 TO 15
      ISUBAD=MOD(IWIRE-1,16)
      TRCHAN=ISUBAD+16*IADDR+192*ICR+IOFF
      END
