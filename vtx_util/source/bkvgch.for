      SUBROUTINE BKVGCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Gains Electronic Header Bank
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
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
      INCLUDE 'D0$LINKS:IZVGCH.LINK'
C
      INTEGER LBANK,LVGCH
      INTEGER NL,NS,ND,NIO
      DATA NL,NS,ND,NIO /4,4,10,2/
C
C     NL = Number of Links
C     NS = Number os Structural Links
C     ND = Number of data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      LVGNH = LC(LSVTX - IZVGNH)
      IF (LVGNH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LVGCH,LVGNH,-IZVGCH,'VGCH',NL,NS,ND,NIO,0)
      LBANK = LVGCH
C
  999 RETURN
      END
