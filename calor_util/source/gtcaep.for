      SUBROUTINE GTCAEP(START,IETA,IPHI,ILYR,BITS,ENERGY,ICHAN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns data for the next channel from CAEP bank.
C-                         First call returns data for the first channel.
C-                         START *MUST* be set to true when getting first 
C-                         cell's data, and START *MUST* be false when getting
C-                         subsequent cells' data. 
C-                            NOTE: Between calls to GTCAEP
C-                         the path should not be changed - if PATHST is called
C-                         then the calling routine should make sure that 
C-                         the proper LCAEP in in ZLINKC before calling this
C-                         routine.
C-
C-   Inputs  : START    [L]    TRUE if starting over to unpack.
C-                             FALSE if getting subsequent data.
C-
C-   Outputs : IETA     [I]     Eta index
C-             IPHI     [I]     Phi index
C-             ILYR     [I]     Layer number
C-             BITS     [I]     bit 0= (LSB) is 1 if limit test overridden
C-                              bit 1= 0(x8), 1(x1)
C-                              bit 2= 0(ped sub.), 1(no ped sub)
C-                              bit 3= 0(gains coor.), 1(no gains corr.)
C-                              bit 4= 0(zero sup.), 1(not zero sup.)
C-                              bit 5= 0(E in Gev), 1(E in ADC counts)
C-                              bit 6= 0(live energy only),1(corrected for crack)
C-                              bit 7= 0(normal channel),1(hot channel suppresed)
C-             ENERGY   [R]     Energy in GeV
C-             ICHAN    [I]     Channel number
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- Reached end of CAEP bank
C-                              -2 --- No CAEP bank address
C-                              -3 --- Expecting dead material but not found
C-   Controls: NONE
C-
C-   Related routines:
C-            GTCAEP_HEADER(NV,NR,NCH,IER)
C-            to return version number, repetition number and total
C-            number of channels in the bank.
C-
C-            GTCAEP_ADDR(IETA,IPHI,ILYR,ENERGY,IER)
C-            to return energy for a given address.
C-
C-
C-   Created   22-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-   Updated  10-APR-1992    Chip S, Alexandre P. no check on NV for dead  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,ICHAN,IER,BITS,NR,NV
      REAL    ENERGY
      INTEGER PAKADD_WORD
      LOGICAL START
      CHARACTER*4 PATH

C
      INTEGER GZCAEP,POINT
      INTEGER ICHANNEL,NCHANNEL
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      SAVE POINT,ICHANNEL,NCHANNEL
      BYTE PAKADD_BYTE(4)
      EQUIVALENCE (PAKADD_WORD,PAKADD_BYTE)

C----------------------------------------------------------------------
C
      IER = 0
C
      IF (START) THEN
C
C ****  READ IN BANK HEADER AND THE FIRST CHANNEL
C
        CALL CZLINI
        LCAEP=GZCAEP()
        IF ( LCAEP .LE. 0 ) THEN
          IER = -2
          GOTO 999
        ENDIF
        ICHANNEL = 0
        NV       = IQ(LCAEP+1)
        NR       = IQ(LCAEP+2)
        NCHANNEL = IQ(LCAEP+3)
        POINT = 4-NR
      ENDIF
C
      ICHANNEL = ICHANNEL + 1
      IF ( ICHANNEL.GT.NCHANNEL ) THEN
C
C ****  REACHED THE END OF THE BANK
C ****  CHECK IF THERE IS A GEAN DEAD MATERIAL CAEP TO UNPACK
C
        CALL PATHGT(PATH)
        IF(PATH.EQ.'GEAN') THEN
          LCAEP=LQ(LCAEP)
          IF ( LCAEP .LE. 0 ) THEN
            IER = -3
            GOTO 999
          ENDIF
          ICHANNEL = 1
          NV       = IQ(LCAEP+1)
          NR       = IQ(LCAEP+2)
          NCHANNEL = IQ(LCAEP+3)
          POINT = 4-NR
        ELSE
          IER = -1
          GOTO 999
        END IF
      ENDIF
C
      POINT = POINT + NR
      PAKADD_WORD = IQ(LCAEP+POINT)
      IETA = PAKADD_BYTE(BYTE4)
      IPHI = PAKADD_BYTE(BYTE3)
      ILYR = PAKADD_BYTE(BYTE2)
      BITS = PAKADD_BYTE(BYTE1)
C
      ENERGY = Q(LCAEP+POINT+1)
      ICHAN = ICHANNEL
C
  999 RETURN
      END
