      SUBROUTINE BKCPMO(LSUP,MDATA,LCPMO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - pedestals
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCPMO - address of CPMO bank
C-   Controls: none
C-
C-   Created  30-MAR-1992   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPMO.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCPMO,NIO,MDATA,IZLINK,NCPMO
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCPMO',NCPMO)
        STP_LSLINK(NCPMO) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCPMO)
        CALL STP_RSLINK('BKCPMO',NCPMO)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CPMO','BKCPD8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCPMO,LSUP,-IZCPMO,'CPMO',1,1,MDATA,NIO,0)
  999 RETURN
      END
