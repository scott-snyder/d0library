      SUBROUTINE FETCH_MBR_Z ( WANTED, FOUND, INFO, OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get vertex information on the first WANTED primary
C-   interaction vertices.
C-
C-   NOTE TO ALL USERS:  the safe way to call this routine is as follows:
C-
C-      INTEGER   wanted, found
C-      PARAMETER (wanted = 3)
C-      LOGICAL   ok
C-      REAL      info(3, wanted)
C-      .
C-      .
C-      .
C-      CALL VERTEX_INFO (wanted, found, info, ok)
C-      DO i = 1, min(wanted, found)
C-          z = info(1, i)
C-          dz = info(2, i)
C-          ntracks = ifix(info(3,i))
C-      ENDDO
C-
C-   This will make sure that one does not read (or write) off the end of the
C-   arrays Z and DZ.
C-
C-   Inputs  : WANTED   [I] the number of vertices to return
C-   Outputs : FOUND    [I] the number of vertices found in the event
C-             INFO     [R*(3,WANTED)] array containing information about the
C-                                     vertices:
C-                INFO(1, i) : Z location of vertex i
C-                INFO(2, i) : error in Z location of vertex i
C-                INFO(3, i) : number of tracks contributing to this vertex
C-
C-             OK        [L]  .TRUE. if all is well, .FALSE. on error
C-
C-   Controls: none
C-
C-   Created  15-OCT-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER  WANTED, FOUND, WORDS
      PARAMETER (WORDS = 3)
      REAL     INFO(WORDS,WANTED)
C----------------------------------------------------------------------
      LOGICAL  OK
      INTEGER  GZVERT, LVERT, NZBANK, I, NTRK
      EXTERNAL GZVERT, NZBANK
C----------------------------------------------------------------------
      CALL VZERO (INFO, WANTED*WORDS)
      OK = .TRUE.
      FOUND = 0

      LVERT = GZVERT(1)
      IF (LVERT .LT. 0) THEN
        OK = .FALSE.
        RETURN
      ELSE IF (LVERT .EQ. 0) THEN
        RETURN
      ENDIF
C
C ****  Fill the INFO array
C
      INFO(1, 1) = Q(LVERT+6)
      INFO(2, 1) = Q(LVERT+7)
      INFO(3, 1) = Q(LVERT+10)
      INFO(1, 2) = Q(LVERT+11)
      INFO(2, 2) = Q(LVERT+12)
C
      RETURN
      END
