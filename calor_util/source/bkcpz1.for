      SUBROUTINE BKCPZ1(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking zero suppression bank, CPZ1
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPZ1 bank.
C-   Controls: none
C-
C-   Created  14-DEC-1989   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPZ1.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPZ1
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPZ1',NCPZ1 )
        STP_LSLINK(NCPZ1) = LSUP
        CALL MZFORM('CPZ1','-I',NIO)
        LSUP = STP_LSLINK(NCPZ1)
        CALL STP_RSLINK('BKCPZ1',NCPZ1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPZ1','BKCPZ1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCPZ1,'CPZ1',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
