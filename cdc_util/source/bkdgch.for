      SUBROUTINE BKDGCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create CDC Gains Electronic Header Bank
C-
C-   Inputs  : none
C-   Outputs : LBANK = Address of the created bank
C-   Controls: none
C-
C-   Created  14-JUN-1989   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDGNH.LINK'
      INCLUDE 'D0$LINKS:IZDGCH.LINK'
C
      INTEGER LBANK,LDGCH
      INTEGER NL,NS,ND,NIO
      DATA NL,NS,ND,NIO /4,4,10,2/
C
C     NL = Number of Links
C     NS = Number os Structural Links
C     ND = Number of data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      LDGNH = LC(LSCDC - IZDGNH)
      IF (LDGNH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LDGCH,LDGNH,-IZDGCH,'DGCH',NL,NS,ND,NIO,0)
      LBANK = LDGCH
C
  999 RETURN
      END
