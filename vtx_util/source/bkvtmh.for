      SUBROUTINE BKVTMH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Times header bank under SVTX(-3)
C-
C-   Inputs  : none
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER NL,NS,ND,NIO
      INTEGER LORUN,HIRUN
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,NIO,ND /10,10,2,5/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in the bank VTMH
C     NIO = Data type (Integer)
C----------------------------------------------------------------------
C
      IF (LSVTX.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LVTMH,LSVTX,-IZVTMH,'VTMH',NL,NS,ND,NIO,0)
      LBANK = LVTMH
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
