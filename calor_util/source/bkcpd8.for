      SUBROUTINE BKCPD8(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CPD8 bank for calorimeter.
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPD8 bank.
C-   Controls: none
C-
C-   Created  28-FEB-1989   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-   Updated  19-MAY-1992   Jan Guida  Set NL=NS=3 (cpmo bank) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPD8
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPD8',NCPD8 )
        STP_LSLINK(NCPD8) = LSUP
        CALL MZFORM('CPD8','30I -F',NIO)
        LSUP = STP_LSLINK(NCPD8)
        CALL STP_RSLINK('BKCPD8',NCPD8 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPD8','BKCPD8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CPD8',NL,NS,ND,NIO,0)
  999 RETURN
      END
