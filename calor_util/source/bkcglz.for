      SUBROUTINE BKCGLZ(LSUP,IZLINK,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CGLZ bank for calorimeter 
C-                          Used in cal calib for gains - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             IZLINK:  link in supporting bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGLZ bank.
C-   Controls: none
C-   Controls: none
C-
C-   Created  16-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGLZ
      PARAMETER( NL = 3 )               ! NUMBER OF LINKS
      PARAMETER( NS = 3 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGLZ',NCGLZ )
        STP_LSLINK(NCGLZ) = LSUP
        CALL MZFORM('CGLZ','-I',NIO)
        LSUP = STP_LSLINK(NCGLZ)
        CALL STP_RSLINK('BKCGLZ',NCGLZ )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGLZ','BKCGLZ','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZLINK,'CGLZ',NL,NS,ND,NIO,0)
  999 RETURN
      END
