      LOGICAL FUNCTION VINPHI(LVTXT, PHIMIN, PHIMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check whether VTXT is in PHI window
C-                         (PHIMIN, PHIMAX) 
C-
C-   Returned value  : TRUE, FALSE
C-   Inputs  :         LVTXT 
C-                     PHIMIN
C-                     PHIMAX
C-   Outputs :         TRUE, FALSE 
C-   Controls: 
C-
C-   Created   1-OCT-1993   Liang-ping Chen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER  LVTXT
      INTEGER  EZERROR, IER 
      REAL     PHIMIN, PHIMAX, PHI, PHICEN, PHI1, PHI2
      REAL     VERTX, VERTY, VERTXMC, VERTYMC 
      LOGICAL  PHIRD, FIRST/.TRUE./
C----------------------------------------------------------------------

      IF (FIRST) THEN
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) .ne. 0) THEN
          CALL ERRMSG('ZTRAKS','VINPHI',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET( 'VERTX', VERTX, IER )
        CALL EZGET( 'VERTY', VERTY, IER )
        CALL EZGET( 'VERTXMC', VERTXMC, IER )
        CALL EZGET( 'VERTYMC', VERTYMC, IER )
        CALL EZRSET
        IF ( IQ(LHEAD + 1) .GT. 1000 ) THEN  ! it is MC data
          VERTX = VERTXMC
          VERTY = VERTYMC
        ENDIF
        FIRST=.FALSE.
      ENDIF
      VINPHI=.FALSE.  
      PHIRD =.FALSE.
      PHI=Q(LVTXT+6)
      IF (PHIMIN .GE. 0.0 .AND. PHIMAX .LE. TWOPI) THEN
        IF (PHI .GE. PHIMIN .AND. PHI .LE. PHIMAX)
     &                        PHIRD = .TRUE.
      ELSE
        IF (PHIMIN .LT. 0.0) THEN
          PHI1 = PHI - TWOPI
          IF (PHI .GE. 0.0 .AND. PHI .LE. PHIMAX
     &      .OR. PHI1 .GE. PHIMIN .AND. PHI1 .LE. 0.0)
     &                          PHIRD = .TRUE.
         ENDIF
        IF (PHIMAX .GT. TWOPI) THEN
          PHI2 = PHI + TWOPI
          IF (PHI .GE. PHIMIN .AND. PHI .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PHIMAX)
     &                          PHIRD = .TRUE.
        ENDIF
      ENDIF
      IF (PHIRD) THEN
        PHICEN=ATAN2(Q(LVTXT+8)-VERTY, Q(LVTXT+7)-VERTX)
        PHIRD = .FALSE.
        IF (PHICEN .LE. 0.0) PHICEN = PHICEN + TWOPI
        IF (PHIMIN .GE. 0.0 .AND. PHIMAX .LE. TWOPI) THEN
          IF (PHICEN .GE. PHIMIN .AND. PHICEN .LE. PHIMAX)
     &      PHIRD = .TRUE.
        ELSE
          IF (PHIMIN .LT. 0.0) THEN
            PHI1 = PHICEN - TWOPI
            IF (PHICEN .GE. 0.0 .AND. PHICEN .LE. PHIMAX
     &      .OR. PHI1 .GE. PHIMIN .AND. PHI1 .LE. 0.0)
     &                          PHIRD = .TRUE.
          ENDIF
          IF (PHIMAX .GT. TWOPI) THEN
            PHI2 = PHICEN + TWOPI
            IF (PHICEN .GE. PHIMIN .AND. PHICEN .LE. TWOPI
     &      .OR. PHI2 .GE. TWOPI .AND. PHI2 .LE. PHIMAX)
     &                          PHIRD = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      IF (PHIRD) THEN
        VINPHI=.TRUE.  
      ENDIF
  999 RETURN
      END
