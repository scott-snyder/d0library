      SUBROUTINE MUISTP ( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code
C-
C-   Purpose and Methods :
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  23-MAY-1988   Ghita Rahal-Callot
C-   Updated   5-APR-1991   Silvia T. Repond, Harrison B. Prosper 
C-      Call MRZCON with ALL option to read complete STP tree once ONLY
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INTEGER IERR
      LOGICAL OK,FIRST
      SAVE OK,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Open the requested file
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MRZCON('ALL',FILNAM(1:LEN(FILNAM)),0,OK)    !  READ MUON GEOMETRY
        IF ( OK ) THEN
          CALL INTMSG(' MUON: Geometry initialization OK')
        ELSE
          CALL INTMSG(' MUON: Geometry initialization FAILED')
        ENDIF
      ENDIF
      IF ( OK ) THEN
        IERR = 0
      ELSE
        IERR =-1
      ENDIF
  999 RETURN
      END
