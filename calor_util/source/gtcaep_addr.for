      SUBROUTINE GTCAEP_ADDR(IETA,IPHI,ILYR,ENERGY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns energy for a given PHYSICS address
C-                         from CAEP bank. Routine works with PTCAEP array 
C-                         in D0$INC:PTCAEP.INC common.  If this array is not 
C-                         already filled, then this routine will fill it and
C-                         then set the PTZFLG to .FALSE. 
C-                         NOTE: PTCAEP depends on the program reseting the 
C-                         array at the end of every event with a call to 
C-                         CPTCAZ. 
C-
C-   Inputs  : IETA     [I]     Eta index
C-             IPHI     [I]     Phi index
C-             ILYR     [I]     Layer number
C-             
C-   Outputs : ENERGY   [R]     Energy in GeV
C-             IER      [I]     Error code; 0 --- OK
C-                              -1 --- IETA,IPHI,ILYR not in this CAEP
C-                              -2 --- No CAEP bank address
C-                              -3 --- IETA,IPHI out of bounds 
C-                                  -- from D0$INC:CAL_OFFLINE.PARAMS
C-                              -4 --- ILYR for DEAD ENERGY not coded yet
C-   Controls: NONE
C-   
C-   Created   22-FEB-1990   W.G.D.Dharmaratna, Chip Stewart
C-   Updated   17-Mar-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IETA,IPHI,ILYR,IER
      REAL    ENERGY
C
      INTEGER GZCAEP,POINT
      INTEGER ICHANNEL,NCHANNEL,NV,NR,I
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C----------------------------------------------------------------------
C
      IER = 0
      ENERGY = 0
C      CALL CZLINI
      LCAEP=GZCAEP()
      
      IF ( LCAEP .LT. 0 ) THEN
        IER = -2
        GOTO 999
      ENDIF
      IF(ILYR.GT.NLYRL) THEN
C
C ****  LOOK FOR GEAN CAEP FOR DEAD ENERGY
C
        IER = -4
        GOTO 999
      END IF
C
      IF (PTZFLG) THEN
        CALL CPTCAF
      ENDIF
C ****  READ IN BANK HEADER AND THE FIRST CHANNEL
C
      NV       = IQ(LCAEP+1)
      NR       = IQ(LCAEP+2)
      NCHANNEL = IQ(LCAEP+3)
      IF (IABS(IETA).LE.NETAL .AND. IETA.NE.0 
     &      .AND. IPHI.LE.NPHIL ) THEN
        ICHANNEL = PTCAEP(IETA,IPHI,ILYR)
      ELSE
        IER= -3
        GOTO 999
      END IF
      IF ( ICHANNEL.EQ.0 .OR. ICHANNEL.GT.NCHANNEL ) THEN
C
C ****  NOT pointing IN the bank
C
            IER = -1 
            GOTO 999
      ENDIF
C
      ENERGY = Q(LCAEP+3+ICHANNEL*NR)
C
  999 RETURN
      END
