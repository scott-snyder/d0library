      SUBROUTINE L2JETS_DUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump results from L2JETS tool
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2LINK.INC'       ! l2 link common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! Control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! Parameters
      INTEGER IL2UNIT                   ! Unit to print to
      INTEGER IEVT                      ! Event number
      INTEGER IPAR,IER,LJPAR,GZJPAR, GZJAUX
      LOGICAL FIRST,OK
      DATA IEVT/ -90000/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C---Write the parameter sets available: only once:

      IF (FIRST) THEN                 ! Write parameter sets
C---Get unit and open file:
C---Open file. Read it in with RCP and close again...
        CALL GTUNIT(L2JETS_USER,IL2UNIT,IER)
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' GTUNIT failure'
          NOWSMESS   = 'DUMP fail'
          GOTO 900
        END IF
        OPEN( IL2UNIT, FILE='L2JETS_DUMP', STATUS='NEW')
C        CALL D0OPEN(IL2UNIT,'L2JETS_DUMP','OF',OK)
C        IF (.NOT. OK) THEN
C          NOWERRMESS = ' DUMP OUTPUT file open failed '
C          NOWSMESS   = 'DUMP FAIL'
C          GOTO 900
C        END IF
C---Print parameter sets this once:
        LJPAR = GZJPAR()
        IF (LJPAR .GT. 0) CALL PRJPAR(IL2UNIT,LJPAR,0,'ALL',0)

        FIRST = .FALSE.
      END IF
C---First lets print a event header. But only when the event changes.
      IF (IEVT .NE. NOWEVT) THEN
        IEVT = NOWEVT                   ! Write event number
        WRITE(IL2UNIT,*)'************ EVT: ',IEVT,
     &    '************************'
      END IF

C---Now dump jaux
      LJAUX = GZJAUX()
      CALL PRJAUX(IL2UNIT,LJAUX,0,'ALL',0)
      GOTO 999
  900 CONTINUE                          ! Error
      CALL ERRMSG(NOWSMESS,'L2JETS_INIT',NOWERRMESS,'W')
  999 RETURN
      END
