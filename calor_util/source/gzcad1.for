      FUNCTION GZCAD1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the CAD1 link
C-
C-   Returned value  : Link of CAD1
C-
C-   Created   7-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZCAD1
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
C----------------------------------------------------------------------
      GZCAD1 = 0
      IF(LHEAD.EQ.0)THEN
        CALL ERRMSG('GZCAD1','GZCAD1',
     &    'HEAD BANK DOES NOT EXIST','W')
        RETURN
      ENDIF
      GZCAD1 = LQ(LHEAD-IZCAD1)
  999 RETURN
      END
