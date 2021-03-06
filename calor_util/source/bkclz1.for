      SUBROUTINE BKCLZ1(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CLZ1 bank for calorimeter - x1
C-                          Used in cal calib for gains - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CLZ1 bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created  16-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCLZ1
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLZ1',NCLZ1 )
        STP_LSLINK(NCLZ1) = LSUP
        CALL MZFORM('CLZ1','30I -F',NIO)
        LSUP = STP_LSLINK(NCLZ1)
        CALL STP_RSLINK('BKCLZ1',NCLZ1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLZ1','BKCLZ1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CLZ1',NL,NS,ND,NIO,0)
  999 RETURN
      END
