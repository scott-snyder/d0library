      INTEGER FUNCTION GZCATD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CATD bank
C-
C-   Returned value  : Link to 1st element of CATD linear structure
C-   Controls: None
C-
C-   Modified 15-APR-1993 Nobuaki Oshima - Take care MDST Path, too.
C-   Created   9-DEC-1991 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
      INTEGER LPROC,GZPROC
      INTEGER LANLS,GZANLS
      CHARACTER*4 PATH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCATD=0
C-
      CALL PATHGT(PATH)
      IF (PATH .EQ. 'MDST') THEN
C-
C--   GET LINK TO SUPPORTING ANLS BANK
        LANLS = GZANLS()
C-
C--   CHECK LANLS
C-
        IF(LANLS.LE.0)THEN
          CALL ERRMSG('CALORIMETER','GZCATD',
     &      'ANLS BANK DOES NOT EXIST ' ,'W')
          GO TO 999
        ENDIF
C-
C--   FIND LINK TO CATD
        GZCATD = LQ(LANLS-2)
C-
      ELSE
C-
C--   GET LINK TO SUPPORTING PROC BANK
        LPROC = GZPROC()
C-
C--   CHECK LPROC
C-
        IF(LPROC.LE.0)THEN
          CALL ERRMSG('CALORIMETER','GZCATD',
     &      'PROC BANK DOES NOT EXIST ' ,'W')
          GO TO 999
        ENDIF
C-
C--   FIND LINK TO CATD
        GZCATD = LQ(LPROC-IZCATD)
      ENDIF
C-
  999 RETURN
      END
