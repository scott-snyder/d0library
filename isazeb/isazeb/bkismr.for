      SUBROUTINE BKISMR(LISMR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     book ISMR bank
C-
C-   Outputs : 
C-      LISMR = pointer to ISMR bank
C-
C-   Created  18-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISMR.LINK'
      INTEGER LISMR,LISAC,GZISAC,GZISMR
C----------------------------------------------------------------------
      LISMR=GZISMR()
      IF(LISMR.NE.0) GOTO 999  ! already exists
      LISAC=GZISAC()
      CALL MZBOOK(IXMAIN,LISMR,LISAC,-IZISMR,
     $            'ISMR',0,0,9,3,-1)
  999 RETURN
      END
