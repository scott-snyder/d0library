      SUBROUTINE ENSURE_CAEH(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : NONE
C-   Outputs : OK     - TRUE IF CAEH FOUND OR MADE
C-   Controls: 
C-
C-   Created  28-NOV-1994   Ian Adam
C-   Updated   2-OCT-1995   Ian Adam  Omit force of CAEH for each event 
C-   Updated   5-NOV-1995   Meenakshi Narain  FIx INRCP statement 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'

      INTEGER LCAEH,GZCAEH,LCAEP,GZCAEP,LCAEQ,GZCAEQ,IER
      LOGICAL OK

      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
      INTEGER RCPLINK
C----------------------------------------------------------------------
      OK = .FALSE.
      
C - CHECK THAT CAHITS_RCP HAS BEEN READ, IF NOT THEN READ IT

      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC('CAHITS_RCP',RCPLINK)
        IF (RCPLINK.LE.0) THEN
          CALL INRCP('CAHITS_RCP',IER)
          CALL EZLOC('CAHITS_RCP',RCPLINK)
          IF (RCPLINK.LE.0) THEN
            CALL ERRMSG('NO CAHITS_RCP','ENSURE_CAEH',
     &       'NO WINDOW ENERGIES AVAILABLE','W')
            GOTO 999
          ENDIF
        ENDIF
      ENDIF

C - MAKE A CAEH BANK AND SET POINTERS

      LCAEH = GZCAEH()
      IF (LCAEH.LE.0) THEN
        LCAEP = GZCAEP()
        IF (LCAEP.LE.0) THEN
          LCAEQ = GZCAEQ()
          IF (LCAEQ.LE.0) THEN
            CALL ERRMSG('NO CAEQ','ENSURE_CAEH','ROUTINE FAILS','W')
            GOTO 999
          ELSE
            CALL CAEQ_TO_CAEP
          ENDIF
        ENDIF
        CALL CAEHFL
        PTZFLG = .TRUE.
        CALL CPTCAZ
        CALL CPTCAF
      ENDIF

      LCAEH = GZCAEH()
      IF (LCAEH.LE.0) THEN
        OK =.FALSE.
      ELSE
        OK = .TRUE.
      ENDIF

  999 RETURN
      END
