      SUBROUTINE BKFGNH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create FDC Gains header bank
C-
C-   Inputs  : none
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-   Updated  16-MAR-1990   Jeffrey Bantly  increase validity default 
C-   Updated   5-MAY-1992   Srini Rajagopalan  Include MZFORM call;
C-                          Check if FGNH exists before booking. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGNH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
      INTEGER ICALL
C
      SAVE NIO
C
      DATA ICALL /0/
      DATA NL,NS,ND /3,3,5/
      DATA LORUN,HIRUN /999999998,999999999/
C
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in bank FGNH
C     NIO = Data Type filled by MZFORM
C----------------------------------------------------------------------
C
      IF (LSFDC.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL MZFORM('FGNH', '5I', NIO)
      ENDIF
C
      LFGNH = LC(LSFDC - IZFGNH)
      IF (LFGNH.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LFGNH,LSFDC,-IZFGNH,'FGNH',NL,NS,ND,NIO,0)
        IC(LFGNH+1)=LORUN                 ! Lowest valid run number
        IC(LFGNH+2)=HIRUN                 ! Highest valid run number
      ENDIF
C
      LBANK = LFGNH
C
  999 RETURN
      END
