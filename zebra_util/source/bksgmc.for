      SUBROUTINE BKSGMC(LSGMC,NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books SGMC bank.
C-
C-   Inputs  : NDATA number of data words
C-   Outputs : LSGMC = link of booked bank.
C-   Controls: 
C-
C-   Created  19-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSGMC.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INTEGER LSTPC,LSGEN,NFORM,LSGMC,NDATA
C----------------------------------------------------------------------
      LSTPC = LC(LSTPH-IZSTPC)
      IF(LSTPC.EQ.0)GO TO 998
      LSGEN = LC(LSTPC-IZSGEN)
C
      IF(LSGEN.EQ.0)GO TO 998
C
      CALL MZFORM ( 'SGMC','1I 2H 1I 2H 2I 3F 1I -F',NFORM )
      CALL MZBOOK ( IXSTP, LSGMC, LSGEN, -IZSGMC, 'SGMC', 0, 0,
     &              NDATA, NFORM, 0 )
      IC(LSGMC+1) = 1                   ! VERSION NUMBER
      RETURN
  998 CONTINUE
      CALL ERRMSG ('BKSGMC','BKSGMC',
     &  'PARENT BANKS NOT PRESENT FOR BOOKING','F')
  999 RETURN
      END
