      SUBROUTINE BKCPB8(LSUP,ND,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking cpb8 bank for calorimeter.
C-                              BAD CHANNEL FLAGS.
C-
C-   Inputs  : LSUP:  address of support bank.
C-             ND:  length of bank.
C-   Outputs : LBANK:  address of CPB8 bank.
C-   Controls: none
C-
C-   Created   2-NOV-1988   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPB8.LINK'
      INTEGER NL,NS,ND,NIO,LBANK,LSUP,IZLINK,NCPB8
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPB8',NCPB8 )
        STP_LSLINK(NCPB8) = LSUP
        CALL MZFORM('CPB8','-I',NIO)
        LSUP = STP_LSLINK(NCPB8)
        CALL STP_RSLINK('BKCPB8',NCPB8 )
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPB8','BKCPB8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LBANK,LSUP,-IZCPB8,'CPB8',NL,NS,ND,NIO,0)
C
  999 RETURN
      END
