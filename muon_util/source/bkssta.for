C+
      SUBROUTINE BKSSTA (STATION, NDATA, VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create bank SSTA and fill it with geometric
C-                         parameters of the SAMUS station GEANT volume
C-
C-   Inputs  : STATION - SAMUS station number
C-             NDATA - number of data words
C-             VALUE(NDATA) - array with values for volume description
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  30-APR-1991   Andrei Kiryunin
C-   Updated  12-NOV-1992   Alexander Efimov   
C-   Updated  09-MAY-1993   E.Kozlovsky   MZFORM is carried out with 
C-                                       the SSTA-bank description
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER STATION, NDATA, VALUE(*)
      INTEGER LSSTH, GZSSTH, NFORM, LSSTA, I
      EXTERNAL GZSSTH
      CHARACTER*80 MSGSTR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C ****  Initialization
C
      IF (FIRST) THEN
        CALL MZFORM ('SSTA', '2I 2H 1I 2H 2I -F', NFORM)
        FIRST = .FALSE.
      END IF
C
C ****  Check existence of the header bank SSTH
C
      LSSTH = GZSSTH()
      IF (LSSTH .EQ. 0) THEN
        MSGSTR = ' *** BKSSTA: supporting bank SSTH does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      END IF
C
C ****  Check station number
C
      IF (VALUE(1) .NE. STATION) THEN
        VALUE(1) = STATION
        MSGSTR = ' BKSSTA: link number must be equal to the '//
     &           'station number '
        CALL INTMSG (MSGSTR)
      END IF
C
C ****  Book bank SSTA
C
      CALL MZBOOK (IDVSTP, LSSTA, LSSTH, -STATION, 'SSTA', 6, 6,
     &             NDATA, NFORM, 0)
C
C ****  Fill bank SSTA with data
C
      DO I = 1, NDATA
        IC(LSSTA+I) = VALUE(I)
      END DO
C
  999 RETURN
      END
