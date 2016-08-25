      SUBROUTINE ENJPTS(LJET0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ENJPTS is called at the end of each event. It
C-              replaces the addresses put into the JPTS bank at fill time with
C-              pointers to the CAEP bank. The routine also compresses the JPTS
C-              bank to the number of words actually required. Finally, it
C-              fills in the version number.
C-
C-   Inputs  : LJET0 the link to the first JETS bank
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-OCT-1988   Z. Wolf
C-   Updated  23-JAN-1989   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C-   Modified 27-APR-1989   Z. Wolf
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LJET0
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJPTS.LINK/LIST'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJETS,LJPTS
      INTEGER LCAEP,GZCAEP
      INTEGER NBOOK,NUSED,NADDR
      INTEGER I,NR,NCH,ICH,WORDN,POINTR
      INTEGER ADCAEP,IECAEP,IPCAEP,ILCAEP
      INTEGER ADJPTS,IEJPTS,IPJPTS,ILJPTS
      BYTE BYJPTS(4)
      BYTE BYCAEP(4)
      EQUIVALENCE (ADJPTS,BYJPTS)
      EQUIVALENCE (ADCAEP,BYCAEP)

      integer ihjpts/4HJPTS/
C----------------------------------------------------------------------
C
C--   CHECK LJET0
      IF(LJET0.LE.0)THEN
        WRITE(LOUT,*)'ENJPTS--> PROBLEM WITH LJET0'
        GOTO 999
      END IF
C
C--   BEGIN LOOP OVER JPTS BANKS
      LJETS=LJET0
   10 LJPTS=LQ(LJETS-IZJPTS)
C
C--   CHECK LINK
      IF(IQ(LJPTS-4).NE.ihJPTS)THEN
        WRITE(LOUT,*)'ENJPTS--> PROBLEM WITH LJPTS'
        GOTO 999
      END IF
C
C--   GET NUMBER OF WORDS BOOKED, NUMBER USED, AND NUMBER OF CELLS ADDRESSED
      NBOOK=IQ(LJPTS-1)
      NUSED=IQ(LJPTS+2)+2
      NADDR=IQ(LJPTS+2)
C
C--   CHECK
      IF(NUSED.GT.NBOOK)THEN
C       WRITE(LOUT,*)'ENJPTS--> NUSED>NBOOK'
      END IF
C
C--   COMPRESS BANK
      CALL MZPUSH(IXCOM,LJPTS,0,-(NBOOK-NUSED),'R')
C
C--   IF JPTS CONTAINS PACKED ADDRESSES, ADD 1000 TO VERSION NUMBER
      IQ(LJPTS+1)=IQ(LJPTS+1)+1000
C
C--   FIND POINTER TO CAEP
C--   IF CAEP DOESN'T EXIST, WE ARE DONE WITH THIS BANK
      LCAEP=GZCAEP()
      IF(LCAEP.LE.0)GO TO 11
C
C--   LOOP OVER JPTS ADDRESSES (TO REPLACE ADDRESS WITH POINTER IN CAEP)
      DO 100, I=1,NADDR
      ADJPTS=IQ(LJPTS+2+I)
      IEJPTS=BYJPTS(BYTE4)
      IPJPTS=BYJPTS(BYTE3)
      ILJPTS=BYJPTS(BYTE2)
C
C--   FIND ADDRESS IN CAEP
      NR=IQ(LCAEP+2)
      NCH=IQ(LCAEP+3)
      IF(NCH.EQ.0)GO TO 11
      DO ICH=1,NCH
        WORDN=4+(ICH-1)*NR
        ADCAEP=IQ(LCAEP+WORDN)
        IECAEP=BYCAEP(BYTE4)
        IPCAEP=BYCAEP(BYTE3)
        ILCAEP=BYCAEP(BYTE2)
        IF(IEJPTS.EQ.IECAEP.AND.
     +     IPJPTS.EQ.IPCAEP.AND.
     +     ILJPTS.EQ.ILCAEP)GO TO 201
      END DO
      WRITE(LOUT,*)'ENJPTS--> JPTS HIT NOT IN CAEP'  !HIT NOT FOUND IN CAEP
      GO TO 100
  201 POINTR=ICH     !HIT FOUND IN CAEP
C
C--   REPLACE JPTS ADDRESS WITH RELATIVE POINTER IN CAEP
      IQ(LJPTS+2+I)=POINTR
  100 CONTINUE   !END OF LOOP OVER JPTS ADDRESSES
C
C--   IF JPTS CONTAINS POINTERS KEEP ORIGINAL VERSION NUMBER
      IQ(LJPTS+1)=IQ(LJPTS+1)-1000
C
C--   READY FOR NEXT BANK
   11 LJETS=LQ(LJPTS+1)
      LJETS=LQ(LJETS)
      IF(LJETS.GT.0)GO TO 10
C
  999 RETURN
      END
