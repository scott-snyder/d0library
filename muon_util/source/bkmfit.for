      SUBROUTINE BKMFIT(IMUON,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-      IMUON    I   address of support bank.
C-                    if 0, default support bank for current PATH
C-                    is taken.
C-   Outputs :
C-      LADDR    I   address of bank, MFIT
C-   Controls:
C-
C-   Created   06-NOV-1991   SHAHRIAR ABACHI
C-   Modified  21-FEB-1992   SHAHRIAR ABACHI   Reference link to MUOT added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INCLUDE 'D0$LINKS:IZMFIT.LINK/LIST'
C  -- variable in arguments...
      INTEGER IMUON,LADDR
C  -- local variables...
      LOGICAL FIRST
C  -- external...
      INTEGER LSUP,IOH
      INTEGER NDAT
C  -- initialize data...
      SAVE NDAT, IOH
      DATA FIRST, NDAT /.TRUE., 33/
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('MFIT','10I-F',IOH)
        FIRST = .FALSE.
      ENDIF
C
      LSUP = LRLINK(IMUON)
C
      CALL MZBOOK(IXMAIN,LADDR,LSUP,-IZMFIT,
     &                      'MFIT',1,0,NDAT,IOH,-1)
C
      IQ(LADDR+1)=1      ! version number
C
  999 RETURN
      END
