      SUBROUTINE BKCLBC(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CLBC bank for calorimeter - corrected gains
C-                         Gains BAD CHANNEL FLAGS - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CLBC bank.
C-   Controls: none
C-
C-   Created   2-NOV-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCLBC.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCLBC
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLBC',NCLBC )
        STP_LSLINK(NCLBC) = LSUP
        CALL MZFORM('CLBC','-I',NIO)
        LSUP = STP_LSLINK(NCLBC)
        CALL STP_RSLINK('BKCLBC',NCLBC )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLBC','BKCLBC','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCLBC,'CLBC',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
