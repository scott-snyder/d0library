      SUBROUTINE BKVPDH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Pedestal header bank under SVTX(-1)
C-
C-   Inputs  : noe
C-   Output  : LBANK = address of the created bank VPDH
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,NIO,ND /10,10,2,5/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      IF (LSVTX.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LVPDH,LSVTX,-IZVPDH,'VPDH',NL,NS,ND,NIO,0)
      LBANK = LVPDH
C
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
