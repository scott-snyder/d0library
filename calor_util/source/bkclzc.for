      SUBROUTINE BKCLZC(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CLZC bank for calorimeter 
C-                          Used in cal calib for corrected gains 
C-                              - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CLZC bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created   2-NOV-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCLZC
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLZC',NCLZC )
        STP_LSLINK(NCLZC) = LSUP
        CALL MZFORM('CLZC','30I -F',NIO)
        LSUP = STP_LSLINK(NCLZC)
        CALL STP_RSLINK('BKCLZC',NCLZC )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLZC','BKCLZC','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CLZC',NL,NS,ND,NIO,0)
  999 RETURN
      END
