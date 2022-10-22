      SUBROUTINE PUPHI_RESET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset PHI( center & width ) at the begin of
C-                         events, if 'PHI CONTROL' is .TRUE.
C-
C-
C-   Modified 12-OCT-1994   Nobuaki Oshima (Reset CAL IETACEN, too.)
C-   Updated   2-APR-1992   Lupe Howell Select CALDIS only if that package is
C-                          present 
C-   Modified 20-JAN-1992   Nobuaki Oshima (Reset CAL PHI&DPHI, too.)
C-   Created  16-MAY-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL    CENPHI,WIDPHI
      INTEGER RUN,ID,RUNSAVE,IDSAVE,IER
      INTEGER IPHI,IDPHI,IETACN
      SAVE RUNSAVE,IDSAVE
      LOGICAL LPHICON,EZERROR,CALFLAG,FIRST
      DATA FIRST /.TRUE./
      DATA IETACN / 0 /
C----------------------------------------------------------------------
C
C ****  Checking if the CALDIS package is ON
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALFLAG = .FALSE.
        CALL PBD_GET_FLAG('CALDIS',CALFLAG)
      ENDIF
C
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAVE.OR.ID.NE.IDSAVE) THEN
        RUNSAVE = RUN
        IDSAVE  = ID
C-
C--- Check PHI CONTROL in PX_SYSTEM_RCP, then reset if it was true.
C-
        CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PCEVEN','Bank PX_SYSTEM_RCP NOT FOUND',
     &      'W')
          GOTO 999
        ENDIF
        CALL PUGET_l('PHI CONTROL',LPHICON)
        IF(LPHICON) THEN     ! Reset to the default PHI
          CALL PUGETV('PHI DEF CEN',CENPHI)
          CALL PUGETV('PHI DEF WID',WIDPHI)
          CALL PUSETV('PHI CENTER',CENPHI)
          CALL PUSETV('PHI WIDTH',WIDPHI)
        ENDIF
        CALL EZRSET
C-
C--- Select PX_CALDIS.RCP bank IF present
C-
        IF ( CALFLAG ) THEN
          CALL EZPICK('PX_CALDIS_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('PIXIE','PUPHI_RESET',
     &        'Unable to pick RCP bank PX_CALDIS_RCP','W')
            GOTO 999
          ELSE
            IF(LPHICON) THEN     ! Reset to the default PHI & ETA
              CALL PUPHI_DEGTOSEG(CENPHI,WIDPHI,IPHI,IDPHI)
              CALL PUSET_i('CAL DPHI',IDPHI)
              CALL PUSET_i('CAL PHI',IPHI)
              CALL PUSET_i('CAL IETACEN',IETACN)
            ENDIF
            CALL EZRSET
          ENDIF
        ENDIF
      ENDIF
C-
  999 RETURN
      END
