      SUBROUTINE BKFGCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Gains Electronic Header Bank
C-
C-   Inputs  : none
C-   Outputs : LBANK = Address of the created bank
C-   Controls: none
C-
C-   Created  14-JUN-1989   Srini Rajagopalan
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call;
C-                          Check if FGCH already exists before booking.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGNH.LINK'
      INCLUDE 'D0$LINKS:IZFGCH.LINK'
C
      INTEGER LBANK,LFGCH
      INTEGER NL,NS,ND,NIO
      INTEGER ICALL
      SAVE NIO
C
      DATA ICALL /0/
      DATA NL,NS,ND/4,4,10/
C
C     NL = Number of Links
C     NS = Number os Structural Links
C     ND = Number of data words
C     NIO = Data Type from MZFORM
C----------------------------------------------------------------------
C
      LFGNH = LC(LSFDC - IZFGNH)
      IF (LFGNH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('FGCH', '10I', NIO)
      ENDIF
C
      LFGCH = LC(LFGNH - IZFGCH)
      IF (LFGCH.EQ.0)
     & CALL MZBOOK(IDVSTP,LFGCH,LFGNH,-IZFGCH,'FGCH',NL,NS,ND,NIO,0)
C
      LBANK = LFGCH
C
  999 RETURN
      END
