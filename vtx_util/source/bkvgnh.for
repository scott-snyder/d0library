      SUBROUTINE BKVGNH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create VTX Gains header bank
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
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
C----------------------------------------------------------------------
C
      INTEGER LBANK
      INTEGER LORUN,HIRUN
      INTEGER NL,NS,ND,NIO
      DATA NL,NS,NIO,ND /10,10,2,5/
      DATA LORUN,HIRUN /0,9999/
C     NL = Number of Links
C     NS = Number of Structural Links
C     ND = Number of Data words in bank VGNH
C     NIO = Data Type (Integer)      
C----------------------------------------------------------------------
C
      IF (LSVTX.EQ.0) THEN
        LBANK = 0                       ! Supporting bank does not exist
        GO TO 999
      ENDIF

      CALL MZBOOK(IDVSTP,LVGNH,LSVTX,-IZVGNH,'VGNH',NL,NS,ND,NIO,0)
      LBANK = LVGNH
C
      IC(LBANK+1)=LORUN                 ! Lowest valid run number
      IC(LBANK+2)=HIRUN                 ! Highest valid run number
C
  999 RETURN
      END
