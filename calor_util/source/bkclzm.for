      SUBROUTINE BKCLZM(LSUP,MDATA,LCLZM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - gains - ICD Laser channels
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCLZM - address of CLZM bank
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCLZM.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCLZM,NIO,MDATA,IZLINK,NCLZM
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCLZM',NCLZM)
        STP_LSLINK(NCLZM) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCLZM)
        CALL STP_RSLINK('BKCLZM',NCLZM)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CLZM','BKCLZM','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCLZM,LSUP,-IZCLZM,'CLZM',1,1,MDATA,NIO,0)
  999 RETURN
      END
