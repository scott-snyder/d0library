      SUBROUTINE BKCP0M(LSUP,MDATA,LCP0M)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - pedestals - level-0 channels
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCP0M - address of CP0M bank
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCP0M.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCP0M,NIO,MDATA,IZLINK,NCP0M
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCP0M',NCP0M)
        STP_LSLINK(NCP0M) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCP0M)
        CALL STP_RSLINK('BKCP0M',NCP0M)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CP0M','BKCP0M','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCP0M,LSUP,-IZCP0M,'CP0M',1,1,MDATA,NIO,0)
  999 RETURN
      END
