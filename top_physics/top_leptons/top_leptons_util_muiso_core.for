      SUBROUTINE TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO,CORECONE_SIZE,
     &     ISOCONE_SIZE,ISOCONE_CUT,ERADG,ERAD,ECOR,EISO,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines isolation on basis of PMUO energy
C-                         in between a core cone and an isolation cone.
C-                         (Called by MUISO routine). Also returns an
C-                         estimate of muon radiated energy.
C-
C-   Inputs  : LPMUO            PMUO bank address
C-             CORECONE_SIZE    Core cone size (0.2,0.4 allowed)
C-             ISOCONE_SIZE     Isolation cone size (0.4,0.6 allowed)
C-             ISOCONE_CUT      Energy cut value for isolation
C-
C-   Outputs : ERADG            Geant Estimate of muon radiated energy
C-             ERAD             Estimate of true raiated energy
C-             ECOR             Energy in core cone
C-             EISO             Energy in isolation cone
C-             IOK              Isolation flag :
C-                                   1 - isolated
C-                                  -1 - non-isolated
C-   Controls:
C-
C-   Created  22-DEC-1992   SHAHRIAR ABACHI
C-   Modified 22-DEC-1992   SHAHRIAR ABACHI  ECOR,EISO outputed
C-   Modified  9-May-1993   Coverted to Subroutine for use in TOP_LEPTONS
C-                          IOK added to return argument list
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LPMUO,IOK
      INTEGER ICONEC,ICONEI
C
      REAL CORECONE_SIZE,ISOCONE_SIZE,ISOCONE_CUT
      REAL EISO,ECOR,ERAD,ERADG,DE
C
      IOK = -1
      ECOR = 0.
      EISO = 0.
      ERADG=Q(LPMUO+33)
      ERAD=ERADG
C
      ICONEC = 10. * CORECONE_SIZE
      ICONEI = 10. * ISOCONE_SIZE
      IF(ICONEI .LE. ICONEC) THEN
        CALL ERRMSG('Isolation cone is smaller or equal to core cone.',
     &      'TOP_LEPTONS_UTIL_MUISO_CORE',' ','F')
        GOTO 999
      ENDIF
C
      IF(ICONEC .LT. 2 .OR. ICONEC .GT. 4 .OR. ICONEC .EQ. 3 .OR.
     &      ICONEC .EQ. 5) THEN
        CALL ERRMSG('Disallowed core cone size',
     &      'TOP_LEPTONS_UTIL_MUISO_CORE',' ','F')
        GOTO 999
      ENDIF
      IF(ICONEC .EQ. 2) ICONEC = 0
      IF(ICONEC .EQ. 4) ICONEC = 1
C
      IF(ICONEI .LT. 2 .OR. ICONEI .GT. 6 .OR. ICONEI .EQ. 3 .OR.
     &      ICONEI .EQ. 5) THEN
        CALL ERRMSG('Disallowed isolation cone size',
     &      'TOP_LEPTONS_UTIL_MUISO_CORE',' ','F')
        GOTO 999
      ENDIF
      IF(ICONEI .EQ. 2) ICONEI = 0
      IF(ICONEI .EQ. 4) ICONEI = 1
      IF(ICONEI .EQ. 6) ICONEI = 2
C
      EISO = Q(LPMUO + 34 + ICONEI)
      ECOR = Q(LPMUO + 34 + ICONEC)
      DE = EISO - ECOR
C
      IF ( DE .LT. ISOCONE_CUT ) THEN
C
C *** If energy cone cut is satisfied then set muon isolation flag
C *** and return radaited energy as energy in dR=0.2 cone
C
        IOK = 1
        ERAD = Q(LPMUO + 34)
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
