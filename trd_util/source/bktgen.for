      SUBROUTINE BKTGEN(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create TRD Times header bank under STRD(-3)
C-
C-   Inputs  : none
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated  15-FEB-1996   A. Zylberstejn  add 2 more links, replace word 1 by
C-   version nb. (was 0 before)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTGEN.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER NL,NS,ND,NIO
      INTEGER LORUN,HIRUN
C
      DATA LORUN,HIRUN /0,9999/
      DATA NL,NS,ND,NIO /5,5,2,10/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in the bank TGEN
C     NIO = Data type (Integer)
C----------------------------------------------------------------------
C
      IF (LSTRD.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      CALL MZBOOK(IDVSTP,LTGEN,LSTRD,-IZTGEN,'TGEN',NL,NS,ND,NIO,0)
      LBANK = LTGEN
C
C      IC(LBANK+1)=LORUN                 ! Lowest valid run number
C      IC(LBANK+2)=HIRUN                 ! Highest valid run number
      IC(LTGEN+1)=1
C
  999 RETURN
      END
