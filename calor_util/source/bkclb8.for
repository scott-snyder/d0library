      SUBROUTINE BKCLB8(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CLB8 bank for calorimeter.  - x8
C-                         Gains BAD CHANNEL FLAGS - ICD Laser channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CLB8 bank.
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCLB8.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCLB8
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLB8',NCLB8 )
        STP_LSLINK(NCLB8) = LSUP
        CALL MZFORM('CLB8','-I',NIO)
        LSUP = STP_LSLINK(NCLB8)
        CALL STP_RSLINK('BKCLB8',NCLB8 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLB8','BKCLB8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCLB8,'CLB8',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
