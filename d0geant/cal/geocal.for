      LOGICAL FUNCTION GEOCAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for Calorimeter Geometry
C-                         Geometrical data are obtained from the
C-                         appropriate SRCP banks, either:
C-
C-                         SRCP_UCAL or SRCP_ECAL.
C-
C-   Inputs  : /D0LOG/ logical switches specifying detectors that are "on"
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated  22-NOV-1988   Harrison B. Prosper
C-                          Cryostat routine GEOCRO has been split into
C-                          the two routines GEOUCR and GEOECR in
C-                          analogy to the calorimeter routines
C-                          GEOUCA and GEOECA. The appropriate SRCP
C-                          geometry ba
C-   Updated 13-JUN-1989    N. A. Graf
C-                          Cryostat routines have been moved to the
C-                          end to speed up the search list.
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Made into pbd logical function; call DETCAL here 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      GEOCAL = .TRUE.
      IF ( DCAL .LT. 1 ) GOTO 999
C
C ****  ENDCAP CALORIMETER
C
      IF (DECA.GE.1) THEN
        CALL EZPICK('SRCP_ECAL')     ! Select SRCP bank 'SRCP_ECAL'
        CALL MXCAL('EC_MIXTURES')    ! Set up EC mixtures
        CALL GEOECA                  ! Set up EC Geometry
      ENDIF
C
C ****  CENTRAL CALORIMETER
C
      IF (DUCA.GE.1) THEN
        CALL EZPICK('SRCP_UCAL')     ! Select SRCP bank 'SRCP_UCAL'
        CALL MXCAL('CC_MIXTURES')    ! Set up CC Mixtures
        CALL GEOUCA                  ! Set up CC Geometry
      ENDIF
C
C ****  ENDCAP CRYOSTAT
C
      IF (DECA.GE.1.AND.DCRY.GE.1) THEN
        CALL EZPICK('SRCP_ECAL')
        CALL GESCIN                  ! Set up scintillator geometry
        CALL GEOECR   ! Set up endcap cryostats
      ENDIF
C
C ****  CENTRAL CRYOSTAT
C
      IF (DUCA.GE.1.AND.DCRY.GE.1) THEN
        CALL EZPICK('SRCP_UCAL')     ! Select SRCP bank 'SRCP_UCAL'
        CALL GEOUCR   ! Set up central cryostat geometry
      ENDIF
C
      IF ( DCAL .GE. 2 ) CALL DETCAL    ! Set up detector sets
C
  999 RETURN
      END
