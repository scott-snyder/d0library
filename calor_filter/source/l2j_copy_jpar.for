      SUBROUTINE L2J_COPY_JPAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Place a copy of JPAR under FRES so it will be
C-                         written out. 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-MAR-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJPAR.LINK'
      INTEGER GZJPAR, GZFRES, LJPAR, LFRES
C---------------------------------------------------------------------- 
      LJPAR = GZJPAR()            ! Get location of JPAR in ZEBSTP
      LFRES = GZFRES()            ! Get location of FRES in ZEBCOM
C: Copy JPAR to link 6 under FRES
      IF ( LJPAR .GT. 0 .AND. LFRES .GT. 0 ) THEN
        CALL MZCOPY( IDVSTP, LJPAR, IXMAIN, LFRES, -6, ' ')
      ELSE
        CALL ERRMSG('JPAR copy problem','L2J_COPY_JPAR',
     &    ' Problem copying JPAR to FRES ','W')
      END IF
  999 RETURN
      END
