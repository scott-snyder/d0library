      FUNCTION GZEMSV(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get pointer to EMSV bank I
C-
C-   Returned value  : Address.
C-   Inputs  : I = bank number, if I=0 return first bank (last booked)
C-   Outputs :
C-   Controls:
C-
C-   Created  30-SEP-1994 Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER L2FRES,GZFRES,GZEMSV,L2EMSV,I,BPASS
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZEMSV.LINK'
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
C--   GET LINK TO SUPPORTING FRES BANK
      GZEMSV = 0
      L2FRES = GZFRES()
      L2EMSV = 0
C
C--   CHECK L2FRES
      IF ( L2FRES .LE. 0 ) THEN
        CALL ERRMSG('FRES-NOT-FOUND','GZEMSV',
     &    'FRES BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO EMSV
      IF (L2FRES.NE.0) THEN
        L2EMSV=L2FRES-IZEMSV
C          find pointer to requested bank
    1   CONTINUE
        L2EMSV=LQ(L2EMSV)
        BPASS=IQ(L2EMSV-5)
        IF(BPASS.NE.I.AND.L2EMSV.NE.0.AND.I.NE.0) GOTO 1
      ENDIF
C
      GZEMSV = L2EMSV
C
  999 RETURN
      END
