      SUBROUTINE BKTPDH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create TRD Pedestal header bank under STRD(-1)
C-
C-   Inputs  : noe
C-   Output  : LBANK = address of the created bank TPDH
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,NIO,ND /7,7,2,5/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      IF (LSTRD.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LTPDH,LSTRD,-IZTPDH,'TPDH',NL,NS,ND,NIO,0)
      LBANK = LTPDH
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
