      SUBROUTINE BKFPDH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Pedestal header bank under SFDC(-1)
C-
C-   Inputs  : noe
C-   Output  : LBANK = address of the created bank FPDH
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated  16-MAR-1990   Jeffrey Bantly  increase validity default 
C-   Updated   5-MAY-1992   Srini Rajagopalan, Include MZFORM call;
C-                          Check if FPDH bank already exists before booking.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFPDH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
      INTEGER ICALL
C
      SAVE NIO
C
      DATA LORUN,HIRUN /999999998,999999999/
      DATA NL,NS,ND /3,3,5/
      DATA ICALL /0/
C
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words
C     NIO = Data Type from MZFORM = 5I
C----------------------------------------------------------------------
C
      IF (LSFDC.EQ.0) THEN
        LBANK = 0                       ! supporting bank does not exist
        GO TO 999
      ENDIF
C
C  Form the bank type
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('FPDH', '5I', NIO)
      ENDIF
C
C  Book FPDH bank
C
      LFPDH = LC(LSFDC - IZFPDH)
      IF (LFPDH.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LFPDH,LSFDC,-IZFPDH,'FPDH',NL,NS,ND,NIO,0)
        IC(LFPDH+1)=LORUN                 ! Lowest valid run number
        IC(LFPDH+2)=HIRUN                 ! Highest valid run number
      ENDIF
      LBANK = LFPDH                       ! Return address of created bank
C
  999 RETURN
      END
