      SUBROUTINE BKCGTR(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CGTR bank for calorimeter
C-                          Used in cal calib for gains from cal trigger
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGTR bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created  16-MAR-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGTR
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGTR',NCGTR )
        STP_LSLINK(NCGTR) = LSUP
        CALL MZFORM('CGTR','30I -F',NIO)
        LSUP = STP_LSLINK(NCGTR)
        CALL STP_RSLINK('BKCGTR',NCGTR )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGTR','BKCGTR','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CGTR',NL,NS,ND,NIO,0)
  999 RETURN
      END
