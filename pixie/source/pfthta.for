      SUBROUTINE PFTHTA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine that will display THETA
C-                         showing delay line hits and isajet tracks
C-
C-
C-   Created  22-FEB-1989   Lupe Rosas
C-   Updated   1-MAY-1989   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  change to handle DSTs,STAs
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-   Updated   8-AUG-1991   Robert E. Avery  only prompt for half if
C-                              tracks are in both halves. 
C-   Updated   8-NOV-1991   Robert E. Avery  Clean up. 
C-   Updated  24-JAN-1992   Robert E. Avery  Clean up more.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHALF
      INTEGER LKFDCH,GZFDCH
      LOGICAL EZERROR
      DATA IHALF/0/
C----------------------------------------------------------------------
      CALL PFPICK_HALF(IHALF)
C
      LKFDCH=GZFDCH()
      IF(LKFDCH.GT.5) THEN
        CALL PFGETD   ! Find delay line hits
      ENDIF
C
      CALL PUOPEN
      CALL PFUMES(' ')
      CALL PFHALF( IHALF )
      CALL JRCLOS
C----------------------------------------------------------------------
  999 RETURN
      END
