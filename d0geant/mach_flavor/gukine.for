      SUBROUTINE GUKINE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call user hook LUKINE to fill JVERT and JKINE
C-                         Geant banks.
C-                         Also form and optionally fill D0'S /ZEBCOM/.
C-   Inputs  : None
C-   Outputs : JVERT, JKINE and optionally /ZEBCOM/ zebra banks
C-
C-   Created  11-JAN-1986  DH,SL,SK,AJ
C-   Updated   7-JUL-1989   Harrison B. Prosper
C-   Now calls user hook LUKINE
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC'
      INTEGER IUNIT,IER
      LOGICAL EXISTR
C----------------------------------------------------------------------
      CALL LUKINE
C
C ****  Write RUNSAVE or ABORT
C&IF VAXVMS,ETA10,SIUNIX,ULTRIX,IBMAIX,SUNOS,ALFOSF
      INQUIRE(FILE='RUNSAVE',EXIST=EXISTR)
      IF(EXISTR) THEN
        CALL GTUNIT(10,IUNIT,IER)
        OPEN(UNIT=IUNIT,FILE='RUNSAVE',STATUS='OLD') ! Dump to file
        WRITE (IUNIT,9255) IEVENT,NRNDM
 9255   FORMAT(3X,'EVT.NUM AND NRNDM ',3I10)
        CLOSE(UNIT=IUNIT,STATUS='KEEP')
        CALL RLUNIT(10,IUNIT,IER)
      ELSE
        CALL UGLAST       ! signal for abort,file has been deleted
        STOP
      END IF
C&ENDIF
  999 RETURN
      END
