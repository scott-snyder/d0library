      SUBROUTINE HMATRIX_SET_LINKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup links in ZHMATRIX of banks
C-   after they have been
C-   read in from rz.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZAVER.LINK'
      INCLUDE 'D0$LINKS:IZEMAT.LINK'
      INCLUDE 'D0$LINKS:IZHMAT.LINK'
      INCLUDE 'D0$LINKS:IZHVIS.LINK'
      INCLUDE 'D0$LINKS:IZHINV.LINK'
      INCLUDE 'D0$LINKS:IZEIGN.LINK'
      INCLUDE 'D0$LINKS:IZUMAT.LINK'
      INCLUDE 'D0$LINKS:IZHRCP.LINK'
      INCLUDE 'D0$LINKS:IZQUAN.LINK'
      INCLUDE 'D0$LINKS:IZDIAG.LINK'
      INCLUDE 'D0$LINKS:IZWORK.LINK'
      INCLUDE 'D0$LINKS:IZPROD.LINK'
C
C----------------------------------------------------------------------
      LAVER = LC(LHMTR - IZAVER)
      LEMAT = LC(LHMTR - IZEMAT)
      LHMAT = LC(LHMTR - IZHMAT)
      LHVIS = LC(LHMTR - IZHVIS)
      LHINV = LC(LHMTR - IZHINV)
      LEIGN = LC(LHMTR - IZEIGN)
      LUMAT = LC(LHMTR - IZUMAT)
      LCRCP = LC(LHMTR - IZHRCP)
      LQUAN = LC(LHMTR - IZQUAN)
      LDIAG = LC(LHMTR - IZDIAG)
      LWORK = LC(LHMTR - IZWORK)
      LPROD = LC(LHMTR - IZPROD)
C
C
C ****  RENAME HMATRIX_RZ_RCP TO NEW PLACE
C
      CALL EZUNNAME('HMATRIX_RZ_RCP')
      CALL EZNAME('HMATRIX_RZ_RCP',LHMTR,IZHRCP)
C
  999 RETURN
      END
