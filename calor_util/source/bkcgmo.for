      SUBROUTINE BKCGMO(LSUP,MDATA,LCGMO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CMOM bank for calorimeter calib higher
C-                          moments - gains
C-
C-   Inputs  : LSUP - supporting bank - not used (yet)
C-             MDATA - length of bank
C-   Outputs : LCGMO - address of CGMO bank
C-   Controls: none
C-
C-   Created  30-MAR-1992   Jan Guida
C-   Updated  27-APR-1992   Jan Guida, C.Stewart  Add MZFORM and link area
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGMO.LINK'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSUP,LCGMO,NIO,MDATA,IZLINK,NCGMO
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL STP_INZLNK
        CALL STP_GSLINK('BKCGMO',NCGMO)
        STP_LSLINK(NCGMO) = LSUP
        CALL MZFORM('CMOM','30I -F',NIO)
        LSUP = STP_LSLINK(NCGMO)
        CALL STP_RSLINK('BKCGMO',NCGMO)
        FIRST = .FALSE.
      ENDIF
      IF(LSUP.EQ.0) THEN
        CALL ERRMSG('NO SUPPORT BANK FOR CGMO','BKCGN8','NO BOOK','W')
        GOTO 999
       END IF
      CALL MZBOOK(IDVSTP,LCGMO,LSUP,-IZCGMO,'CGMO',1,1,MDATA,NIO,0)
  999 RETURN
      END
