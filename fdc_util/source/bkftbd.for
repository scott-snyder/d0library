      SUBROUTINE BKFTBD(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Times Bad channel Bank
C-
C-   Inputs  : LSUP = Address of the supporting bank
C-             ND = length of the created bank
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFTBD.LINK'
C----------------------------------------------------------------------
      INTEGER LBANK,LSUP
      INTEGER NL,NS,ND,NIO
C
      CHARACTER*5 NDX
C
      DATA NL,NS /2,2/
C
C      NL = NUMBER OF LINKS
C      NS = NUMBER OF STRUCTURAL LINKS
C      NIO = DATA TYPE from MZFORM
C----------------------------------------------------------------------
C
      IF (LSUP.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
      WRITE(NDX,10)ND
   10 FORMAT(I4.4,'I')
      CALL MZFORM('FTBD', NDX, NIO)
C
C  Book FTBD bank
C
      LBANK = LC(LSUP - IZFTBD)
      IF (LBANK.EQ.0)
     & CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZFTBD,'FTBD',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
