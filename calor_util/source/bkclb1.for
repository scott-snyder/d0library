      SUBROUTINE BKCLB1(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CLB1 bank for calorimeter.  - x1
C-                         Gains BAD CHANNEL FLAGS - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CLB1 bank.
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCLB1.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCLB1
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLB1',NCLB1 )
        STP_LSLINK(NCLB1) = LSUP
        CALL MZFORM('CLB1','-I',NIO)
        LSUP = STP_LSLINK(NCLB1)
        CALL STP_RSLINK('BKCLB1',NCLB1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLB1','BKCLB1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCLB1,'CLB1',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
