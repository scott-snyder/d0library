      SUBROUTINE BKCGN1(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CGN1 bank for calorimeter.
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK: link in supporting bank. 
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGN8 bank.
C-   Controls: none
C-
C-   Created  28-FEB-1989   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGN1
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGN1',NCGN1 )
        STP_LSLINK(NCGN1) = LSUP
        CALL MZFORM('CGN1','30I -F',NIO)
        LSUP = STP_LSLINK(NCGN1)
        CALL STP_RSLINK('BKCGN1',NCGN1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGN1','BKCGN1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CGN1',NL,NS,ND,NIO,0)
  999 RETURN
      END
