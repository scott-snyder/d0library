      SUBROUTINE BKCPTM(LSUP,MDATA,LCPTM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - trigger pedestals
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCPTM - address of CPTM bank
C-   Controls: none
C-
C-   Created  15-SEP-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPTM.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCPTM,NIO,MDATA,IZLINK,NCPTM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPTM',NCPTM)
        STP_LSLINK(NCPTM) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCPTM)
        CALL STP_RSLINK('BKCPTM',NCPTM)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPTM','BKCPTM','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCPTM,LSUP,-IZCPTM,'CPTM',1,1,MDATA,NIO,0)
  999 RETURN
      END
