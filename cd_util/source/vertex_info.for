      SUBROUTINE VERTEX_INFO ( WANTED, FOUND, INFO, OK )
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
C-   Updated  26-SEP-1995   sss - Warn about old MC data if
C-                                FIX_SHOWERLIB_VERTEX hasn't been run.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER  WANTED, FOUND, WORDS
      PARAMETER (WORDS = 3)
      REAL     INFO(WORDS, WANTED)
C----------------------------------------------------------------------
      LOGICAL  OK
      integer  reco_version_major, reco_version_pass
      INTEGER  GZVERT, LVERT, NZBANK, I
      EXTERNAL GZVERT, NZBANK

      logical  monte_carlo_data, flgchk
      external monte_carlo_data, flgchk
C----------------------------------------------------------------------

c
c *** If this is MC, reco version is <12,
c *** and the fix_showerlib_vertex package has not been called,
c *** issue a warning.
c
      call reco_version (reco_version_major, reco_version_pass)
      if (monte_carlo_data () .and.
     &    reco_version_major .le. 11 .and.
     &    .not. flgchk ('FIX_SHLIB_VERTEX')) then
        call errmsg ('FIX_SHOWERLIB_VERTEX missing',
     &               'VERTEX_INFO',
     &               'Needed for reco <12 showerlib data',
     &               'W')
      endif

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
C ****  Count the number of vertex banks
C
      FOUND = NZBANK(IXCOM, LVERT)
C
C ****  Fill the INFO array
C
      DO I = 1, MIN (FOUND, WANTED)
        LVERT = GZVERT(I)
        INFO(1, I) = Q(LVERT+5)
        INFO(2, I) = Q(LVERT+8)
        INFO(3, I) = FLOAT(IBITS(IQ(LVERT+2), 8, 8))
      ENDDO                             ! i = 1, found
      RETURN
      END
