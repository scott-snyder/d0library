      SUBROUTINE FCDUMP(ILUN,IOPTS,NOPTS,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Produce a formatted dump of a file catalog
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,NOPTS,IOPTS(NOPTS),IERR
C-----------------------------------------------------------------------
C
      CALL FCLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -1
        GOTO 999
      ENDIF
C
      WRITE(*,1001)
 1001 FORMAT(/,/,' File catalogs are text files.  Use TYPE or MORE to',
     +  ' display',/,/)
C
  999 RETURN
      END
