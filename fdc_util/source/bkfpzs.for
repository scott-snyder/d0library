      SUBROUTINE BKFPZS(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC zero suppression bank
C-
C-   Inputs  : LSUP = Address of the supporting bank
C-             ND = length of the created bank
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFPZS.LINK'
C----------------------------------------------------------------------
      INTEGER LBANK,LSUP
      INTEGER NL,NS,ND,NIO
C
      CHARACTER*5 NDX
C
      DATA NL,NS /2,2/
C     NL = Number of Links
C     NS = Number of Structural Links
C      ND = INPUT = Length of bank
C     NIO = Data Type from MZFORM
C----------------------------------------------------------------------
C
C
      IF (LSUP.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
C  Note: NDX cannot be greater than 9999.
C
      WRITE(NDX,10)ND
   10 FORMAT(I4.4,'I')
      CALL MZFORM('FPZS', NDX, NIO)
C
C  Book FPZS bank
C
      LBANK = LC(LSUP - IZFPZS)
      IF (LBANK.EQ.0)
     & CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZFPZS,'FPZS',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
