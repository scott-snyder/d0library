      SUBROUTINE BKFPCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Pedestal Electronic Header Bank
C-
C-   Inputs  : none
C-   Outputs : LBANK = Address of the created bank
C-   Controls: none
C-
C-   Created  14-JUN-1989   Srini Rajagopalan
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call;
C-                          Check if FPCH already exists before booking.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFPDH.LINK'
      INCLUDE 'D0$LINKS:IZFPCH.LINK'
C
      INTEGER LBANK,LFPCH
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
      LFPDH = LC(LSFDC - IZFPDH)
      IF (LFPDH.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        CALL MZFORM('FPCH', '10I', NIO)
        ICALL = 1
      ENDIF
C
      LFPCH = LC(LFPDH - IZFPCH)
      IF (LFPCH.EQ.0)
     & CALL MZBOOK(IDVSTP,LFPCH,LFPDH,-IZFPCH,'FPCH',NL,NS,ND,NIO,0)
C
      LBANK = LFPCH
C
  999 RETURN
      END
