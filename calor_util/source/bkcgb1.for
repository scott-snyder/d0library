      SUBROUTINE BKCGB1(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking cpb1 bank for calorimeter.
C-                              BAD CHANNEL FLAGS.
C-
C-   Inputs  : LSUP:  address of SUPPORT bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CGB1 bank.
C-   Controls: none
C-
C-   Created   12-NOV-1988   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCGB1.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCGB1
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGB1',NCGB1 )
        STP_LSLINK(NCGB1) = LSUP
        CALL MZFORM('CGB1','-I',NIO)
        LSUP = STP_LSLINK(NCGB1)
        CALL STP_RSLINK('BKCGB1',NCGB1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGB1','BKCGB1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCGB1,'CGB1',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
