      SUBROUTINE BKCG0B(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CG0B bank for calorimeter.
C-                         Gains BAD CHANNEL FLAGS - level-0 channels
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CG0B bank.
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCG0B.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCG0B
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCG0B',NCG0B )
        STP_LSLINK(NCG0B) = LSUP
        CALL MZFORM('CG0B','-I',NIO)
        LSUP = STP_LSLINK(NCG0B)
        CALL STP_RSLINK('BKCG0B',NCG0B )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CG0B','BKCG0B','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCG0B,'CG0B',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
