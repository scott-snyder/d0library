      SUBROUTINE BKCPB1(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking cpb1 bank for calorimeter.
C-                              BAD CHANNEL FLAGS.
C-
C-   Inputs  : LSUP:  address of SUPPORT bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPB1 bank.
C-   Controls: none
C-
C-   Created   2-NOV-1988   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPB1
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPB1',NCPB1 )
        STP_LSLINK(NCPB1) = LSUP
        CALL MZFORM('CPB1','-I',NIO)
        LSUP = STP_LSLINK(NCPB1)
        CALL STP_RSLINK('BKCPB1',NCPB1 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPB1','BKCPB1','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCPB1,'CPB1',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
