      SUBROUTINE PRJPTS(PRUNIT,LJPTS,NJPTS,CFL,IFL)
C
C******************************************************************************
C
C     PURPOSE: PRJPTS prints out the contents of the JPTS banks
C
C     INPUT: PRUNIT is the unit number for the printout
C            LJPTS is the link to the JPTS bank
C            NJPTS is the bank number, not used
C            CFL is a flag to control printout, not used, no linear chain
C            IFL is a flag giving the type of printout, not used
C
C     CREATED: 19-OCT-1988 by Z. Wolf
C
C******************************************************************************
C
      IMPLICIT NONE
C
C--   I/O
      INTEGER PRUNIT,LJPTS,NJPTS,IFL
      CHARACTER CFL*(*)
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
C--   INTERNAL VARIABLES
      INTEGER I
      INTEGER NHITS
      INTEGER IPNTR
      INTEGER PKADDR,IE,IP,IL
      BYTE BYTES(4)
      EQUIVALENCE (PKADDR,BYTES)
      CHARACTER*4 CHAR4
      INTEGER ICHAR4
      EQUIVALENCE (ICHAR4,CHAR4)
      DATA     CHAR4 /'JPTS'/
C
C--   CHECK LINK
      IF(IQ(LJPTS-4).NE.ICHAR4)THEN
        WRITE(PRUNIT,*)'PRJPTS--> PROBLEM WITH LJPTS, WILL PRINT ANYWAY'
      END IF
C
C--   WRITE HEADER
1     WRITE(PRUNIT,10)LJPTS
10    FORMAT(//' ','JPTS BANK'
     +  /' ','(Output of PRJPTS, LJPTS= ',I10,')'/)
C
C--   WRITE BANK CONTENTS
      WRITE(PRUNIT,20)IQ(LJPTS-5)
20    FORMAT(' ','BANK NUMBER = ',I5)
      WRITE(PRUNIT,21)IQ(LJPTS-4)
21    FORMAT(' ','BANK NAME = ',A4)
      WRITE(PRUNIT,22)IQ(LJPTS-3),IQ(LJPTS-2),IQ(LJPTS-1)
22    FORMAT(' ','NL= ',I5,5X,'NS= ',I5,5X,'ND= ',I5)
      WRITE(PRUNIT,31)IQ(LJPTS+1)
31    FORMAT(' ','BANK VERSION NUMBER = ',I5)
C
      NHITS=IQ(LJPTS+2)
      DO 40 I=1,NHITS
      IF(IQ(LJPTS+1).EQ.2)THEN   !VERSION NUMBER 2 MEANS PACKED ADDRESSES
        PKADDR=IQ(LJPTS+2+I)
        IE=BYTES(BYTE4)
        IP=BYTES(BYTE3)
        IL=BYTES(BYTE2)
        WRITE(PRUNIT,33)IE,IP,IL
33      FORMAT(' ','IE= ',I3,5X,'IP= ',I3,5X,'IL= ',I3)
      END IF
      IF(IQ(LJPTS+1).EQ.1)THEN   !VERSION NUMBER 1 MEANS POINTERS TO CAEP
        IPNTR=IQ(LJPTS+2+I)
        WRITE(PRUNIT,32)IPNTR
32      FORMAT(' ','IPNTR = ',I6)
      END IF
40    CONTINUE
C
      RETURN
      END
