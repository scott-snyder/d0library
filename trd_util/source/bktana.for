      SUBROUTINE BKTANA(LTDST,LAYER,ltana)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank TANA(layer). Processed data for TRD
C-
C-   Inputs  : LTDST  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-             layer        = layer nb. (1 to 3)
C-   Outputs : ltana
C-   Controls: None
C-
C-   Created  29-JUN-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTDST,ltana
      INTEGER LAYER
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER XND,XNL,XNS
      INTEGER GZTDST
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
      DATA XND,XNL,XNS/601,0,0/
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TANA','300F -I',IXIO)        ! Describe Bank format
      ENDIF
      ltana=0
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LTDST .LE. 0 ) THEN
        LTDST = GZTDST()
      ENDIF
      IF ( LTDST .LE. 0 ) THEN
        CALL ERRMSG('TDST not found','  BKTANA',' ','W')
        GOTO 999
      ENDIF
C
      NL = XNL
      NS = XNS
      ND = XND
      CALL MZBOOK(IXMAIN,LTANA,LTDST,-LAYER,'TANA',NL,NS,ND,IXIO,0)
  999 RETURN
      END
