      SUBROUTINE BKVTCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Times Electronic Header Bank
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
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INCLUDE 'D0$LINKS:IZVTCH.LINK'
C
      INTEGER LBANK,LVTCH
      INTEGER NL,NS,ND,NIO
      DATA NL,NS,ND,NIO /4,4,10,2/
C
C     NL = Number of Links
C     NS = Number os Structural Links
C     ND = Number of data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      LVTMH = LC(LSVTX - IZVTMH)
      IF (LVTMH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LVTCH,LVTMH,-IZVTCH,'VTCH',NL,NS,ND,NIO,0)
      LBANK = LVTCH
C
  999 RETURN
      END
