      SUBROUTINE BKCGTB(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking cgtb bank for calorimeter.
C-                              BAD CHANNEL FLAGS - cal trigger - gains
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGTB bank.
C-   Controls: none
C-
C-   Created  16-MAR-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCGTB.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGTB
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGTB',NCGTB )
        STP_LSLINK(NCGTB) = LSUP
        CALL MZFORM('CGTB','-I',NIO)
        LSUP = STP_LSLINK(NCGTB)
        CALL STP_RSLINK('BKCGTB',NCGTB )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGTB','BKCGTB','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCGTB,'CGTB',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
