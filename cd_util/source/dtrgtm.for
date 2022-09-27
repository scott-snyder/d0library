      SUBROUTINE DTRGTM(TRGTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get trigger timing pulse signal from CDC crate
C-                         for Central Detector
C-
C-   Inputs  : none
C-   Outputs : 
C-         TRGTIM: time in ns
C-
C-   Created   8-FEB-1991   Qizhong Li-Demarteau
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
C
      INTEGER TRGLAY, TRGSEC, TRGWIR, ERR, IER
      INTEGER NPULSE(0:11), NEVOLD, HISTON(10)
      INTEGER MXHTOT
      PARAMETER( MXHTOT= 500 )
      LOGICAL TRGFLG, FIRST, DONE
      LOGICAL EZERROR
      REAL HITLST(8,MXHTOT), TRTIME, TRGOFF, TRGTIM
C
      DATA  FIRST/.TRUE./
      DATA  NEVOLD / -1 /
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTRGTM',
     &       'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('TRGLAY',TRGLAY,ERR)
        CALL EZGET_i('TRGSEC',TRGSEC,ERR)
        CALL EZGET_i('TRGWIR',TRGWIR,ERR)
        CALL EZGET('TRGOFF',TRGOFF,ERR)
        CALL EZGET_iarr('HISTON(1)',HISTON(1),ERR)
        CALL EZRSET
      ENDIF
C
      IF( LHEAD .EQ. 0 ) GOTO 999
      IF ( IQ( LHEAD+9 ) .NE. NEVOLD ) THEN
        NEVOLD = IQ( LHEAD+9 )
        LAYER  = TRGLAY
        SECTOR = TRGSEC
        WIRE   = TRGWIR
        CALL CDPULS(NPULSE(0), HITLST(1,1), MXHTOT)
        IF ( NPULSE(0) .NE. 0  ) THEN
          TRTIME = HITLST(2,1) - TRGOFF      ! default time...
        ELSE
          TRTIME = 0.
        ENDIF
C
        IF (HISTON(9) .NE. 0) THEN
          CALL HF1(1991,HITLST(2,1),1.)
          CALL HF1(1992,HITLST(4,1),1.)
          CALL HF1(1993,HITLST(5,1),1.)
        ENDIF
      ENDIF
C
      TRGTIM = TRTIME
  999 RETURN
      END
