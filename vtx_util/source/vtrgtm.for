      SUBROUTINE VTRGTM(TRGTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get trgger timing pulse signal for Central
C-                         Detector
C-
C-   Inputs  : none
C-   Outputs : 
C-         TRGTIM: time in ns
C-
C-   Created   8-FEB-1991   Qizhong Li-Demarteau
C-   Updated  19-FEB-1991   Vertex Account  Update for VTX use 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ERR
      INTEGER NPULSE(0:7,0:1), NEVOLD, TRGCHN
      INTEGER TYPE, LAY, SEC, WIR, STR, END, UB
      INTEGER MXHTOT
      PARAMETER( MXHTOT= 500 )
      LOGICAL FIRST, ZFLAG
      REAL HITLST(8,MXHTOT), TRTIME, TRGTIM, TRGOFF
C
      DATA  FIRST/.TRUE./
      DATA  NEVOLD / -1 /
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('TRGCHN',TRGCHN,ERR) ! Logical channel number for
                                        ! timing pulse
        CALL EZGET('TRGOFF', TRGOFF, ERR)
        CALL EZRSET
        ZFLAG = .FALSE.
        CALL VCODER(TRGCHN,TYPE,LAY,SEC,WIR,STR,END,UB,1)
        IF ( TYPE .EQ. 1 ) ZFLAG = .TRUE.
      ENDIF
C
      IF( LHEAD .EQ. 0 ) THEN
        TRTIME = 0.
        GOTO 666
      ENDIF
      IF ( IQ( LHEAD+9 ) .NE. NEVOLD ) THEN
        NEVOLD = IQ( LHEAD+9 )
        CALL VTPULS(TRGCHN,NPULSE(0,0), HITLST(1,1), MXHTOT, ZFLAG)
        IF ( NPULSE(0,0) .NE. 0  ) THEN
C
C ****  Trigger time = timing pulse time - trigger offset
C ****  This assumes the timing pulse is the first hit in HITLST
C
          TRTIME = HITLST(2,1) - TRGOFF
        ELSE
          TRTIME = 0.
        ENDIF
      ENDIF
C
  666 CONTINUE
      TRGTIM = TRTIME
  999 RETURN
      END
