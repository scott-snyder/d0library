      SUBROUTINE SAMUS_BOOK_FLAGS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book logiacl flags for SAMUS package.
C-
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-   Created  10-MAY-1991   O.Eroshin
C-   Modified  06-JUN-1991  S. Abachi   Name of flags changed to be different
C-                                      from MURECO package
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFLAGS
      INTEGER IERR
      INTEGER I
C
      PARAMETER (NFLAGS = 4)
C
      CHARACTER*16 FLAG_NAMES(NFLAGS)
C
      LOGICAL INITIAL_VALUES(NFLAGS)
C----------------------------------------------------------------------
      DATA FLAG_NAMES / 'SHIST_ON    ',  !if histogram pkg gets called
     &                  'SDISPLAY_ON ',  !if display pkg gets called
     &                  'SPED_READ   ',  !if pedestal file has been read
     &                  'SDMPREQ     '/  !if dump has been requested
C
      DATA INITIAL_VALUES /     .TRUE.,
     &                          .FALSE.,
     &                          .FALSE.,
     &                          .FALSE./
C
C----------------------------------------------------------------------
      CALL FLGBK (FLAG_NAMES,NFLAGS)
      CALL FLGERR(IERR)
      IF (IERR .NE. 0) THEN
        CALL INTMSG(' Error in booking user flags.')
      ELSE
        CALL INTMSG(' User flags booked successfully.')
      ENDIF
      DO I = 1,NFLAGS
        CALL FLGSET(FLAG_NAMES(I),INITIAL_VALUES(I))
      ENDDO
  999 RETURN
      END
