      SUBROUTINE ERRFND( KEY, POS, FOUND )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search the position of KEY by binary search
C-                         algorithm.
C-
C-   Inputs  : KEY         A character string to be searched
C-
C-   Outputs : POS         The position where KEY is located if found
C-                         otherwise, position at which to insert
C-             FOUND       If KEY is found then return .TRUE.,
C-                         otherwise return .FALSE.
C-
C-   Controls: OVFL        .TRUE.  if key not found and no room for new entries
C-                         .FALSE. otherwise
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32 KEY
      LOGICAL FOUND
      INTEGER POS
      INTEGER LOW, HIGH, MID
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C
      FOUND = .FALSE.
      IF (NENTRY .EQ. 0) THEN
        OVFL = .FALSE.
        POS = 1
      ELSE
        LOW = 1
        HIGH = NENTRY
C
C **** Binary search
C
   10   CONTINUE
        IF ( ( LOW .LE. HIGH ) .AND. ( .NOT. FOUND )) THEN
          MID = (LOW + HIGH ) / 2
          IF ( KEY .EQ. ID(MID) ) THEN
            FOUND = .TRUE.
            POS = MID
          ELSEIF ( KEY .LT. ID(MID) ) THEN
            HIGH = MID - 1
          ELSEIF ( KEY .GT. ID(MID) ) THEN
            LOW = MID + 1
          ENDIF
          GO TO 10
        ENDIF
        IF ( FOUND ) THEN
          POS = MID
        ELSE
          POS = LOW
        ENDIF
C
C **** check if overflow happens ?
C
        IF ( ( .NOT. FOUND ) .AND. ( NENTRY .GE. MAXSTR ) ) THEN
          OVFL = .TRUE.
          NENTRY = MAXSTR
        ENDIF
      ENDIF
C
  999 RETURN
      END
