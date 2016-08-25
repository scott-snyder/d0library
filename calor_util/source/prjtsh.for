      SUBROUTINE PRJTSH(PRUNIT,LJTSH,NJTSH,CFL,IFL)
C
C******************************************************************************
C
C     PURPOSE: PRJTSH prints out the contents of the JTSH banks
C
C     INPUT: PRUNIT is the unit number for the printout
C            LJTSH is the link to the JTSH bank
C            NJTSH is the bank number, not used
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
      INTEGER PRUNIT,LJTSH,NJTSH,IFL
      CHARACTER CFL*(*)
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER I
      INTEGER NHITS
      INTEGER IPNTR
      INTEGER IE,IP,IL

      integer ihjtsh/4HJTSH/
C
C--   CHECK LINK
      IF(IQ(LJTSH-4).NE.iHJTSH)THEN
        WRITE(PRUNIT,*)'PRJTSH--> PROBLEM WITH LJTSH, WILL PRINT ANYWAY'
      END IF
C
C--   WRITE HEADER
    1 WRITE(PRUNIT,10)LJTSH
   10 FORMAT(//' ','JTSH BANK'
     +  /' ','(Output of PRJTSH, LJTSH= ',I10,')'/)
C
C--   WRITE BANK CONTENTS
      WRITE(PRUNIT,20)IQ(LJTSH-5)
   20 FORMAT(' ','BANK NUMBER = ',I5)
      WRITE(PRUNIT,21)IQ(LJTSH-4)
   21 FORMAT(' ','BANK NAME = ',A4)
      WRITE(PRUNIT,22)IQ(LJTSH-3),IQ(LJTSH-2),IQ(LJTSH-1)
   22 FORMAT(' ','NL= ',I5,5X,'NS= ',I5,5X,'ND= ',I5)
      WRITE(PRUNIT,31)IQ(LJTSH+1)
   31 FORMAT(' ','BANK VERSION NUMBER = ',I5)
C
C
      WRITE(PRUNIT,100)IQ(LJTSH+2)
  100 FORMAT(' ','ALGORITHM NUMBER =',I4)
      WRITE(PRUNIT,101)Q(LJTSH+3),Q(LJTSH+4),Q(LJTSH+5)
  101 FORMAT(' ','ETA WIDTH =',F7.2,
     &  '   PHI WIDTH =',F7.2,'   EM FRAC =',F7.3)
      RETURN
      END
