      FUNCTION TB90_CALOR_UNPACK ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO READ TB90 CAD1 DATA AND CREATE A RECO CAEP
C-                         WITH ALL GANGING, SORTING, PEDESTAL-SUBTRACTION
C-                         & GAINS HANDLED.
C-
C-   Returned value  : TRUE IF CAEP CREATED
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  22-FEB-1990   Chip Stewart
C-   Modified 18-APR-1990   W. Dharmaratna- added TBESFL
C-   Updated  25-JUL-1990   Chip Stewart   - made hooks into entries
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISTAT,IER
      LOGICAL L,CHTINI,TRIGGER,BEAM,TRIGGER_BIT_CHECK
      CHARACTER*50 MSG
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'  ! NEEDED BY PTCAEP
      INCLUDE 'D0$INC:PTCAEP.INC'
      LOGICAL TB90_CALOR_UNPACK
      LOGICAL TB90_CALOR_UNPACK_SETUP
      LOGICAL TB90_CALOR_UNPACK_RESET
      LOGICAL TB90_CALOR_UNPACK_BEGIN
      LOGICAL FIRST,TBES, FIRST_RESET,FIRST_SETUP,RESET
      DATA  FIRST/.TRUE./,FIRST_RESET/.TRUE./,FIRST_SETUP/.TRUE./
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('TB90_CALOR_UNPACK_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0 ) THEN
          CALL EZGET('FILL_TBES',TBES,IER)
          CALL EZGET('SELECT_TRIGGERS',TRIGGER,IER)
          IF( IER.NE.0) THEN
            CALL ERRMSG('NOTRIGSWTCH','TB90_CALOR_UNPACK',
     &      'NO TRIGGER SWITCH IN TB90_CALOR_UNPACK_RCP','W')
            TRIGGER = .FALSE.
          END IF
        END IF
C
        CALL EZRSET
        BEAM = .TRUE.
      END IF
      TB90_CALOR_UNPACK = .TRUE.
      IF(TRIGGER) BEAM = TRIGGER_BIT_CHECK ('TB90_CALOR_UNPACK_RCP')
      IF(.NOT.BEAM) GOTO 999
      CALL TB90_CAEPFL(IER)
      IF ( IER.NE.0 ) THEN
        TB90_CALOR_UNPACK = .FALSE.
        GOTO 999
      ENDIF
      IF (TBES) CALL TBESFL(IER)
      IF ( IER.NE.0 ) THEN
        TB90_CALOR_UNPACK = .FALSE.
        GOTO 999
      ENDIF
  999 RETURN
C
C
      ENTRY TB90_CALOR_UNPACK_SETUP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO READ IN TB90 CALORIMTER ADDRESSING TABLES
C-
C-   Returned value  : TRUE IF READ OK
C-
C----------------------------------------------------------------------
      TB90_CALOR_UNPACK_SETUP = .FALSE.
      IF ( FIRST_SETUP) THEN
        CALL TB90_READ_TABLE(ISTAT)
        IF ( ISTAT.NE.0) THEN
          WRITE(MSG,10)'Error reading ADDRESS TABLE',ISTAT
          CALL ERRMSG('BADREADTB90TABLE','TB90_READ_TABLE',MSG,'F')
        END IF
C
C ****  read in TB90_CALOR_UNPACK_RCP
C
        CALL INRCP('TB90_CALOR_UNPACK_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 1999              ! failed
        CALL EZPICK('TB90_CALOR_UNPACK_RCP') ! select TB90_CALOR_UNPACK bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          TB90_CALOR_UNPACK_SETUP = .TRUE.
          CALL EZGET ('FILL_PTCAEP',RESET,IER)
          IF(IER.NE.0)THEN
            CALL ERRMSG('NOFILL_PTCAEP','TB90_CALOR_UNPACK_SETUP',
     &       'No FILL_PTCAEP IN RCP','W')
            RESET = .FALSE.
          END IF
        ELSE
          CALL ERRMSG('BADRCPBANK','TB90_CALOR_UNPACK_SETUP',
     &      'No TB90_CALOR_UNPACK bank','W')
        ENDIF
        CALL EZRSET
        CALL CZLINI                       ! Initialize /ZLINKC/
        IF (RESET) PTZFLG = .TRUE.        ! CAEP POINTER ARRAY INITALLY ZERO
      END IF
      TB90_CALOR_UNPACK_SETUP = .TRUE.
 1999 RETURN
   10 FORMAT(A39,I5)
C
C
      ENTRY TB90_CALOR_UNPACK_RESET ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To reset PTCAEP array if needed
C-
C-   Returned value  : TRUE IF OK
C----------------------------------------------------------------------
      TB90_CALOR_UNPACK_RESET = .TRUE.
C
C ****  RESET if DESIRED
C
      IF (BEAM .AND. RESET) CALL CPTCAZ
C
 2999 RETURN
C
      ENTRY TB90_CALOR_UNPACK_BEGIN ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To write TB90_CALOR_UNPACK_RCP to begin run record
C-
C-   Returned value  : TRUE IF OK
C----------------------------------------------------------------------
      TB90_CALOR_UNPACK_BEGIN = .TRUE.
C
C ****   copy TB90_CALOR_UNPACK_RCP to begin run division
C
      CALL BRCPFL('TB90_CALOR_UNPACK_RCP')
 3999 RETURN
      END
