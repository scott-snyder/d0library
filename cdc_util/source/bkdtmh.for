      SUBROUTINE BKDTMH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create CDC Times header bank under SCDC(-3)
C-
C-   Inputs  : none
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated  22-SEP-1992   Herb Greenlee  fixed Zebra I/O characteristic 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDTMH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER NL,NS,ND,NIO
      INTEGER LORUN,HIRUN
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,ND,NIO /9,9,2,2/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in the bank DTMH
C     NIO = Data type (Integer)
C----------------------------------------------------------------------
C
      IF (LSCDC.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LDTMH,LSCDC,-IZDTMH,'DTMH',NL,NS,ND,NIO,0)
      LBANK = LDTMH
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
