      SUBROUTINE BKCPTB(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking cptb bank for calorimeter.
C-                              BAD CHANNEL FLAGS - cal trigger
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPTB bank.
C-   Controls: none
C-
C-   Created  15-SEP-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPTB.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPTB
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPTB',NCPTB )
        STP_LSLINK(NCPTB) = LSUP
        CALL MZFORM('CPTB','-I',NIO)
        LSUP = STP_LSLINK(NCPTB)
        CALL STP_RSLINK('BKCPTB',NCPTB )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPTB','BKCPTB','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCPTB,'CPTB',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
