      INTEGER FUNCTION GZTHIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to THIT bank
C-
C-   Returned value  : Link to 1st element of THIT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-JUL-1991 16:29:49.47  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTHIT.LINK/LIST'
      INTEGER LTRDH,GZTRDH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTHIT=0
C
C--   GET LINK TO SUPPORTING TRDH BANK
      LTRDH=GZTRDH()
C
C--   CHECK LTRDH
      IF(LTRDH.LE.0)THEN
c        CALL ERRMSG('TRD:','GZTHIT',
c     &    'TRDH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO THIT
      GZTHIT=LQ(LTRDH-IZTHIT)
  999 RETURN
      END
