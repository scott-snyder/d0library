      FUNCTION GZCAD2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the CAD2 link
C-
C-   Returned value  : Link of CAD2
C-
C-   Created   7-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZCAD2
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
C----------------------------------------------------------------------
      GZCAD2 = 0
      IF(LHEAD.EQ.0)THEN
        CALL ERRMSG('GZCAD2','GZCAD2',
     &    'HEAD BANK DOES NOT EXIST','W')
        RETURN
      ENDIF
      GZCAD2 = LQ(LHEAD-IZCAD2)
  999 RETURN
      END
