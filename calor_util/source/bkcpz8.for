      SUBROUTINE BKCPZ8(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking zero suppression bank, CPZ8
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPZ8 bank.
C-   Controls: none
C-
C-   Created  14-DEC-1989   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPZ8.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPZ8
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPZ8',NCPZ8 )
        STP_LSLINK(NCPZ8) = LSUP
        CALL MZFORM('CPZ8','-I',NIO)
        LSUP = STP_LSLINK(NCPZ8)
        CALL STP_RSLINK('BKCPZ8',NCPZ8 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPZ8','BKCPZ8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCPZ8,'CPZ8',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
