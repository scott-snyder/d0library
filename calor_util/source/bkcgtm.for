      SUBROUTINE BKCGTM(LSUP,MDATA,LCGTM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - trigger gains
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCGTM - address of CGTM bank
C-   Controls: none
C-
C-   Created  16-MAR-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGTM.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCGTM,NIO,MDATA,IZLINK,NCGTM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGTM',NCGTM)
        STP_LSLINK(NCGTM) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCGTM)
        CALL STP_RSLINK('BKCGTM',NCGTM)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGTM','BKCGTM','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCGTM,LSUP,-IZCGTM,'CGTM',1,1,MDATA,NIO,0)
  999 RETURN
      END
