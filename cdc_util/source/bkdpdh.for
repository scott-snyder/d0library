      SUBROUTINE BKDPDH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create CDC Pedestal header bank under SCDC(-1)
C-
C-   Inputs  : noe
C-   Output  : LBANK = address of the created bank DPDH
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,NIO,ND /5,5,2,5/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words
C     NIO = Data Type (Integer)
C----------------------------------------------------------------------
C
      IF (LSCDC.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LDPDH,LSCDC,-IZDPDH,'DPDH',NL,NS,ND,NIO,0)
      LBANK = LDPDH
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
