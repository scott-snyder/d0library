      SUBROUTINE BKCG0M(LSUP,MDATA,LCG0M)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - gains - level-0 channels
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCG0M - address of CG0M bank
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCG0M.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCG0M,NIO,MDATA,IZLINK,NCG0M
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCG0M',NCG0M)
        STP_LSLINK(NCG0M) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCG0M)
        CALL STP_RSLINK('BKCG0M',NCG0M)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CG0M','BKCG0M','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCG0M,LSUP,-IZCG0M,'CG0M',1,1,MDATA,NIO,0)
  999 RETURN
      END
