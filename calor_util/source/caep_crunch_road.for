      SUBROUTINE CAEP_CRUNCH_ROAD (ETA,PHI,DETA,DPHI,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To overwrite CAEP bank with cells only within
C-          an ETA PHI window of (IETA+-DETA, IPHI+-DPHI)
C-
C-   Inputs  : IETA,IPHI (tower of road)
C-             DETA,DPHI (window of road)
C-   Outputs : IER  0 = OK
C-   Controls: NONE
C-
C-   Created  10-DEC-1991   Chip Stewart
C-   Updated   8-MAR-1992   W.G.D. Dharmaratna- changed NCH to be the 
C-                          no. of cells after the crunch.           
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER ETA,PHI,DETA,DPHI,IER
      INTEGER IETA,IPHI,ILYR,NKEEP,NTOSS,NV,NR,NCH,I
      INTEGER LUSER,BITS,ICHAN,PACKCAEP,LCAEP,GZCAEP,NSQUEEZE
      LOGICAL START
      REAL    ENERGY,RETA,RETAC,RPHI,RPHIC,SIGN
      BYTE BYTES(4)
      EQUIVALENCE (PACKCAEP,BYTES)

C----------------------------------------------------------------------
      IER = -1
      PTZFLG = .FALSE.
      CALL CPTCAZ
      CALL GTCAEP_HEADER(NV,NR,NCH,IER)
C
C ****  BOOK TEMPORARY BANK FOR SUPPRESSED DATA
C
      CALL MZBOOK(IXMAIN,LUSER,LHEAD,-IZUSER,'USER',1,1,NCH*NR+3,2,0)
      IF(LUSER.EQ.0) GO TO 999
      NV = MOD(NV,10) + 2000
      IQ(LUSER+1) = NV
      IQ(LUSER+2) = NR
C
C ****  LOOP OVER CAEP CHANNELS
C
      START = .TRUE.
      NKEEP=3                          ! NUMBER WORDS KEEP
      NTOSS=0                          ! NUMBER WORDS TOSS
      RETAC = ETA
      RETAC = ETA - SIGN(0.5,RETAC)
      RPHIC = PHI - 0.5 
      DO I = 1,NCH
        CALL GTCAEP(START,IETA,IPHI,ILYR,BITS,ENERGY,ICHAN,IER)
        START = .FALSE.
        RETA = IETA
        RETA = IETA - SIGN(0.5,RETA)
        RPHI = IPHI - 0.5 
        IF (ABS(RETA-RETAC).LE.DETA.AND.ABS(RPHI-RPHIC).LE.DPHI)THEN
          NKEEP = NKEEP + 2
          BYTES(BYTE4) = IETA  
          BYTES(BYTE3) = IPHI  
          BYTES(BYTE2) = ILYR 
          BYTES(BYTE1) = BITS
          IQ(LUSER+NKEEP-1) = PACKCAEP
          Q (LUSER+NKEEP)   = ENERGY 
          PTCAEP(IETA,IPHI,ILYR) = (NKEEP - 3)/2
        ELSE 
          NTOSS = NTOSS + 2
        END IF
      END DO
      LCAEP = GZCAEP ()
      IQ(LUSER+3) = (NKEEP - 3)/2
      CALL UCOPY(IQ(LUSER+1),IQ(LCAEP+1),NKEEP )
      CALL MZPUSH(IXMAIN,LCAEP,0,-NTOSS,'R')
  998 CONTINUE
      CALL MZDROP(IXMAIN,LUSER,'L')
 999  RETURN
      END
