      SUBROUTINE PFWAIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Wait for a PF-key to be struck in full screen 
C-                         mode
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: PF is changed in this routine but acted upon externally 
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER READPF
C----------------------------------------------------------------------
      PF=0                        !RESET to avoid looping over commands
      IF(FULSCR) THEN
        CALL OUTMSG('0Done with task, use PF key to continue')
        DO WHILE (PF.EQ.0)
          PF=READPF()
        ENDDO
      ENDIF
      IF(PF.EQ.4) PF=0            ! To avoid direct exit when returning
      WAIFLG=.TRUE.               ! Indicate that PF came from this routine
      RETURN
      END
