      SUBROUTINE BKFTMH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Times header bank under SFDC(-3)
C-
C-   Inputs  : none
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated  16-MAR-1990   Jeffrey Bantly  increase validity default 
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call;
C-                          Check if FTMH already exists before booking.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER NL,NS,ND,NIO
      INTEGER LORUN,HIRUN
      INTEGER ICALL
C
      SAVE NIO
C
      DATA ICALL /0/
      DATA LORUN,HIRUN /999999998,999999999/
      DATA NL,NS,ND/3,3,5/
C
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in the bank FTMH
C     NIO = Data type from MZFORM
C----------------------------------------------------------------------
C
      IF (LSFDC.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('FTMH', '5I', NIO)
      ENDIF
C
      LFTMH = LC(LSFDC - IZFTMH)
      IF (LFTMH.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LFTMH,LSFDC,-IZFTMH,'FTMH',NL,NS,ND,NIO,0)
        IC(LFTMH+1)=LORUN                 ! Lowest valid run number
        IC(LFTMH+2)=HIRUN                 ! Highest valid run number
      ENDIF
      LBANK = LFTMH
C
C
  999 RETURN
      END
