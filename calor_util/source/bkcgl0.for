      SUBROUTINE BKCGL0(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CGL0 bank for calorimeter
C-                          Used in cal calib for gains - level-0 channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGL0 bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGL0
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGL0',NCGL0 )
        STP_LSLINK(NCGL0) = LSUP
        CALL MZFORM('CGL0','30I -F',NIO)
        LSUP = STP_LSLINK(NCGL0)
        CALL STP_RSLINK('BKCGL0',NCGL0 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGL0','BKCGL0','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CGL0',NL,NS,ND,NIO,0)
  999 RETURN
      END
