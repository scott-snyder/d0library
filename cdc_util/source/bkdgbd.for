      SUBROUTINE BKDGBD(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create CDC pedestal Bad channel Bank
C-
C-   Inputs  : LSUP = Address of the supporting bank
C-             ND = length of the created bank
C-   Output  : LBANK = address of the created bank
C-
C-   Created  29-DEC-1988   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDGBD.LINK'
C----------------------------------------------------------------------
      INTEGER LBANK,LSUP
      INTEGER NL,NS,ND,NIO
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      PARAMETER( NIO = 0 )              ! DATA TYPE (UNDEFINED)
C----------------------------------------------------------------------
C
      IF (LSUP.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
C  Book DGBD bank
C
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZDGBD,'DGBD',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
