      FUNCTION GZJNEP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-          return pointer to first JNEP bank
C-   Returned value  : pointer
C-
C-   Created  22-NOV-1991   Dhiman Chakraborty
C-   Updated  14-APR-1993   Dhiman Chakraborty  Changed declaration of
C-                          GZJNEP from LOGICAL to INTEGER for UNIX
C-                          compatibility      
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZJNEP
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJNEP.LINK/LIST'
      INTEGER LJETS,GZJETS
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZJNEP=0
C
C--   get link to supporting JETS bank
      LJETS=GZJETS()
C
C--   CHECK LJETS
      IF(LJETS.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZJNEP',
     &    'JETS BANK DOES NOT EXIST ' ,'W')
      ELSEIF (IQ(LJETS+1).GT.1) THEN   ! Check JETS bank version
C
C--   find link to JNEP
        GZJNEP=LQ(LJETS-IZJNEP)
      ENDIF
C
  999 RETURN
      END
