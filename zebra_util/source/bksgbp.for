      SUBROUTINE BKSGBP(LSGBP,NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOKS SGBP BANK
C-
C-   Inputs  : NDATA number of data words
C-   Outputs : LSGBP = link of booked bank.
C-   Controls: 
C-
C-   Created  19-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSGBP.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INTEGER LSTPC,LSGEN,NFORM,LSGBP,NDATA
C----------------------------------------------------------------------
      LSTPC = LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0)GO TO 998
      LSGEN = LC(LSTPC-IZSGEN)
C
      IF(LSGEN.EQ.0)GO TO 998
C
      CALL MZFORM ( 'SGBP','1I 2H 1I 2H 2I 3F 1I -F',NFORM )
      CALL MZBOOK ( IXSTP, LSGBP, LSGEN, -IZSGBP, 'SGBP', 0, 0,
     &              NDATA, NFORM, 0 )
      IC(LSGBP+1) = 1                   ! VERSION NUMBER
      RETURN
  998 CONTINUE
      CALL ERRMSG ('BKSGBP','BKSGBP',
     &  'PARENT BANKS NOT PRESENT FOR BOOKING','F')
  999 RETURN
      END
