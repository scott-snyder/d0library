      SUBROUTINE BKVPZS(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX zero suppression bank
C-
C-   Inputs  : LSUP = Address of the supporting bank
C-             ND = length of the created bank
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPZS.LINK'
C----------------------------------------------------------------------
      INTEGER LBANK,LSUP
      INTEGER NL,NS,ND,NIO
      DATA NL,NS,NIO /2,2,2/
C     NL = Number of Links
C     NS = Number of Structural Links
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      IF (LSUP.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
C  Book VPZS bank
C
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZVPZS,'VPZS',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
