      SUBROUTINE BKFTCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Times Electronic Header Bank
C-
C-   Inputs  : none
C-   Outputs : LBANK = Address of the created bank
C-   Controls: none
C-
C-   Created  14-JUN-1989   Srini Rajagopalan
C-   Updated   5-MAY-1992   Srini Rajagopalan Include MZFORM call;
C-                          Check if FTCH already exists before booking.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
      INCLUDE 'D0$LINKS:IZFTCH.LINK'
C
      INTEGER LBANK,LFTCH
      INTEGER NL,NS,ND,NIO
      INTEGER ICALL
C
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
      LFTMH = LC(LSFDC - IZFTMH)
      IF (LFTMH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('FTCH', '10I', NIO)
      ENDIF
C
      LFTCH = LC(LFTMH - IZFTCH)
      IF (LFTCH.EQ.0)
     & CALL MZBOOK(IDVSTP,LFTCH,LFTMH,-IZFTCH,'FTCH',NL,NS,ND,NIO,0)
C
      LBANK = LFTCH
C
  999 RETURN
      END
