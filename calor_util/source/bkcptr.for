      SUBROUTINE BKCPTR(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CPTR bank for calorimeter
C-                          Used in cal calib for pedestals from cal trigger
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPTR bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created  15-SEP-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPTR
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPTR',NCPTR )
        STP_LSLINK(NCPTR) = LSUP
        CALL MZFORM('CPTR','30I -F',NIO)
        LSUP = STP_LSLINK(NCPTR)
        CALL STP_RSLINK('BKCPTR',NCPTR )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPTR','BKCPTR','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CPTR',NL,NS,ND,NIO,0)
  999 RETURN
      END
