      SUBROUTINE BKCGNH(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CGNH bank for calorimeter.
C-                                 Gain header bank.
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGNH bank.
C-   Controls: none
C-
C-   Created  24-FEB-1989   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-   Updated   5-MAR-1994   Jan Guida  Change number of links from 4 to 5
C-                                      in order to add LV0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGNH
      PARAMETER( NL = 6 )               ! NUMBER OF LINKS
      PARAMETER( NS = 6 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGNH',NCGNH )
        STP_LSLINK(NCGNH) = LSUP
        CALL MZFORM('CGNH','-I',NIO)
        LSUP = STP_LSLINK(NCGNH)
        CALL STP_RSLINK('BKCGNH',NCGNH )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGNH','BKCGNH','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCGNH,'CGNH',NL,NS,ND,NIO,0)
  999 RETURN
      END
