      SUBROUTINE GTCAEP_MAX_ENERGY(ENERGY_MAX,IETA,IPHI,ILYR,BITS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the maximum energy and the physics
C-                         address from CAEP bank.               
C-
C-   Inputs  : None
C-   Outputs : ENERGY_MAX [R]  Maximum energy(GeV) in the bank
C-             IETA       [I]  Eta index
C-             IPHI       [I]  Phi index
C-             ILYR       [I]  Layer number
C-             BITS       [I]   bit 0= (LSB) is 1 if limit test overridden
C-                              bit 1= 0(x8), 1(x1)
C-                              bit 2= 0(ped sub.), 1(no ped sub)
C-                              bit 3= 0(gains coor.), 1(no gains corr.)
C-                              bit 4= 0(zero sup.), 1(not zero sup.)
C-                              bit 5= 0(E in Gev), 1(E in ADC counts)
C-             IER        [I]  Error code; 0---OK
C-                                      -1---No CAEP bank address
C-
C-   Controls: None 
C-
C-   Created  28-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,BITS,IER
      REAL    ENERGY_MAX
      CHARACTER*4 PATH
      INTEGER PAKADD_WORD,NR,I
C
      INTEGER LCAEP,GZCAEP,POINT
      INTEGER ICHANNEL,NCHANNEL,FIRST_CHANNEL,CHANNEL_MAX
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      BYTE PAKADD_BYTE(4)
      EQUIVALENCE (PAKADD_WORD,PAKADD_BYTE)

C----------------------------------------------------------------------
C
      IER = 0
C
      LCAEP=GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        IER = -1
        GOTO 999
      ENDIF
      
      FIRST_CHANNEL = 1
      NR       = IQ(LCAEP+2)
      NCHANNEL = IQ(LCAEP+3)
      POINT    = LCAEP + 3 

      ENERGY_MAX    = 0.0
      CHANNEL_MAX   = 0

      DO I = FIRST_CHANNEL, NCHANNEL
        POINT = POINT + NR
        IF ( Q(POINT) .GT. ENERGY_MAX  ) THEN
          ENERGY_MAX  = Q(POINT)
          CHANNEL_MAX = I
        ENDIF
      ENDDO
      
      PAKADD_WORD = IQ(LCAEP+4+(CHANNEL_MAX-1)*NR)
      IETA = PAKADD_BYTE(BYTE4)
      IPHI = PAKADD_BYTE(BYTE3)
      ILYR = PAKADD_BYTE(BYTE2)
      BITS = PAKADD_BYTE(BYTE1)

  999 RETURN
      END
