      FUNCTION TRCHAN(ILAYER,IWIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DETERMINE CHANNEL NUMBER FOR FADC DIGITIZATIO
C-                         FOR THE TRD
C-
C-   Inputs  : ILAYER= TRD LAYER  ,  IWIRE= HIT WIRE
C-   Outputs :
C-
C-   Created  12-JAN-1988   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICR,ILAYER,IMODU,ITRIP,IWIRE,TRCHAN
      INTEGER IADDR,IFOIS,ISUBAD
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
      DATA IFOIS/0/

      IFOIS=IFOIS+1
C
C  EASY CODING: ADDRESS=LAYER NB. *1000 +WIRE NB.
      TRCHAN=ILAYER*1000+IWIRE
C
C           SOMETHING MORE SOPHISTICATED
C  TRY TO GUESS THE FINAL TRD ARRANGEMENT FROM D.C. De Groot NOTE ON:
C  "Fadc/Dezero Trigger Module Specification (19/2/87)"
C
C  ASSUME:
C           16 CHANNELS PER MODULE
C           12 MODULES PER CRATE
C            4 CRATES
C            4 "TRIPLETS" PER CRATE
C             A TRIPLE BEING DEFINED BY A SET OF 3 FADC MODULES-
C             ONE OF EACH LAYER- WITH THE SAME WIRES
C
C                  TRIPLET 0                      |   |       TRIPLET 3
C   +-----------+   +-----------+   +-----------+ |   |+-----------+
C   |  LAYER 1  |   |  LAYER 2  |   |  LAYER 3  | |   ||  LAYER 1  |
C   |  -----    |   |  -----    |   |  -----    | |___||  -----    |___
C   | WIRE  I   |   | WIRE  I   |   | WIRE  I   | |   || WIRE  I+48|
C   |       I+15|   |       I+15|   |       I+15| |   ||       I+63|
C   +-----------+   +-----------+   +-----------+ |   |+-----------+
C                                                 |   |
C  MODULE NUMBER                                 0 TO 15
      IMODU=(IWIRE-1)/16
C  CRATE NUMBER                                  0 TO  3
      ICR=IMODU/4
C  TRIPLET NUMBER IN THE CRATE                   0 TO  3
      ITRIP=IMODU-ICR*4
C  ADDRESS OF THE FADC MODULE IN THE CRATE       0 TO 11
      IADDR=ITRIP*3+ILAYER-1
C  SUB_ADDRESS OF THE CHANNEL IN THE FADC MODULE 0 TO 15
      ISUBAD=MOD(IWIRE-1,16)
      TRCHAN=ISUBAD+16*IADDR+256*ICR
      IF(IFOIS.LT.20.AND.PTRD.GE.10)THEN
        WRITE(LOUT,*)' IN TRCHAN,IWIRE,ILAYER',IWIRE,ILAYER
        WRITE(LOUT,*)' MODULE',IMODU,'CRATE',ICR,'TRIPLET',ITRIP
        WRITE(LOUT,*)' IADDR',IADDR,' WIRE SUB-ADDRESS',ISUBAD
      END IF
      END
