      SUBROUTINE D0DAD_TESTDF_FRAMES(IERR)
C------------------------------------------------------------------------------
C-
C-  Read events using the same method used within the D0 frameworks.
C-
C-   OUTPUT:  IERR - I - IERR.ne.0 ==> Error during processing.
C------------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:quest.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR,NEV
      LOGICAL OK
C
C  Open all necessary files...
C
      CALL EVOPIN(FDFNAM,'X',IDADDF,OK)
      IF( .NOT.OK ) THEN
         IF( LDDBG.GT.0 ) WRITE(*,9001) IERR
 9001    FORMAT(' D0DAD_TESTDF: Error ',I4,' from D0DAD_SYSOPEN')
         IERR = -1
         GOTO 999
      ENDIF
C
C  Read loop...
C
      NEV=0
 10   CONTINUE
         CALL EVTIN(IDADDF,IERR)
         IF( IERR.EQ.3 ) GOTO 999
         IF( IERR.NE.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9002) IERR
 9002       FORMAT(' D0DAD_TESTDF: Error',I4,' from D0DAD_DFREAD')
            IERR = -2
            GOTO 999
         ELSEIF( LDDBG.GT.4 ) THEN
           WRITE(*,9003) IQ(LHEAD+6),IQ(LHEAD+9)
 9003      FORMAT(' Read R/E: ',2I10)
         ENDIF
         NEV=NEV+1
         IF( MOD(NEV,100).EQ.0 ) THEN
           WRITE(*,1003) NEV
 1003      FORMAT('     Read sequential event number ',I6)
C& IF VAXVMS
C& ELSE
C&           CALL FLUSH_STD
C& ENDIF
         ENDIF
      GOTO 10
C
 999  CONTINUE
      WRITE(*,1001) NEV
 1001 FORMAT(' Read ',I8,' events using d0dad file')
      RETURN
      END
