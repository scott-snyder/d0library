      SUBROUTINE NOI_DROP_OLD_CAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop the original CAD banks in the 
C-                         NOISY package
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-JAN-1992   Allen I. Mincer
C-   Modified  9-MAR-1993   Allen I. Mincer
C-                          Remove NOI_EVENT_SELECT call to allow
C-                          more general usage
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INTEGER LCAD1,LCAD2
C----------------------------------------------------------------------
      LCAD1=LQ(LHEAD-IZCAD1)
      LCAD2=LQ(LHEAD-IZCAD2)
      IF(LCAD1.LE.0 .OR. LCAD2.LE.0)THEN
        CALL ERRMSG('NOISY','NOI_DROP_OLD_CAD',
     &    'NO CAD BANKS TO DROP','W')
      ELSE
        CALL MZDROP(IXMAIN,LCAD1,' ')
        LCAD2=LQ(LHEAD-IZCAD2)
        CALL MZDROP(IXMAIN,LCAD2,' ')
      ENDIF
  999 RETURN
      END
