      SUBROUTINE MUON_BOOK_FLAGS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book logiacl flags for MUON package.
C-
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-   Created   8-OCT-1989   Shuichi Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
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
      DATA FLAG_NAMES / 'HIST_ON         ',!if histogram pkg gets called
     &                  'DISPLAY_ON      ',!if display pkg gets called
     &                  'PED_READ        ',!if pedestal file has been read
     &                  'DMPREQ          '/!if dump has been requested
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
