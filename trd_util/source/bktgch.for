      SUBROUTINE BKTGCH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create TRD Gains Electronic Header Bank
C-
C-   Inputs  : none
C-   Outputs : LBANK = Address of the created bank
C-   Controls: none
C-
C-   Created  14-JUN-1989   Srini Rajagopalan
C-   Updated  28-OCT-1991   JFG changed the number of links 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZTGCH.LINK'
C
      INTEGER LBANK,LTGCH
      INTEGER NL,NS,ND,NIO
      INTEGER ICALL
      DATA NL,NS,ND /5,5,10/
      DATA ICALL /0/
      SAVE NIO
C
C     NL = Number of Links
C     NS = Number os Structural Links
C     ND = Number of data words
C     NIO = Data Type (Integer) from MZFORM
C----------------------------------------------------------------------
C
      LTGAI = LC(LSTRD - IZTGAI)
      IF (LTGAI.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('TGCH','10I',NIO)
      ENDIF
C
      LTGCH = LC(LTGAI - IZTGCH)
      IF (LTGCH.LE.0)
     & CALL MZBOOK(IDVSTP,LTGCH,LTGAI,-IZTGCH,'TGCH',NL,NS,ND,NIO,0)
C
      LBANK = LTGCH
C
  999 RETURN
      END

