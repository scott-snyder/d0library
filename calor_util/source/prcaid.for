      SUBROUTINE PRCAID (U, DUMMY1, DUMMY2, CDUMMY, DUMMY3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out for CAID (calorimeter anomalous isolated
C-   deposit) bank.
C-
C-   Inputs  : U      [I] the unit number to which we print
C-             DUMMY1 [I] dummy argument to satisfy template -- ignored
C-             DUMMY2 [I] dummy argument to satisfy template -- ignored
C-             CDUMMY [C*(*)] dummy argument to satisfy template -- ignored
C-             DUMMY3 [I]  dummy argument to satisfy template -- ignored
C-
C-   Outputs : printout
C-   Controls: none
C-
C-   Created   8-APR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  U, DUMMY1, DUMMY2, DUMMY3
      CHARACTER*(*) CDUMMY
C----------------------------------------------------------------------
      INTEGER  LCAID, GZCAID, VERS, NREP, NMAX, NAID, NCAND, I, IETA
      INTEGER  IPHI, ILYR, SCALED_RATIO
      EXTERNAL GZCAID
      REAL     ET_THRESH, RATIO, ENERGY
      LOGICAL  OK
C----------------------------------------------------------------------
      WRITE (UNIT = U, FMT = 100)
  100 FORMAT (/, 1X, '--------------------', /)
      WRITE (UNIT = U, FMT = 110)
  110 FORMAT (1X, 'Calorimeter Anomalous Isolated Deposit bank (CAID)')

      LCAID = GZCAID()
      IF (LCAID .LE. 0) THEN
        WRITE (UNIT = U, FMT = 100)
  200   FORMAT (1X, 'No CAID bank for this event')
        WRITE (UNIT = U, FMT = 100)
        RETURN
      ENDIF

      CALL GTCAID_GLOBAL (VERS, NREP, NMAX, NAID, NCAND,
     &                   ET_THRESH, RATIO, OK)
      IF (OK) THEN
        WRITE (UNIT = U, FMT = 250)
  250   FORMAT (/, 1X, 'VERS', 1X, 'NREP', 1X, 'NMAX', 1X, 'NAID',1X,
     &    'NCAND', 1X, 'ET_THRESH', 4X, 'RATIO', /)

        WRITE (UNIT = U, FMT = 275) VERS, NREP, NMAX, NAID,
     &                            NCAND, ET_THRESH, RATIO
  275   FORMAT (2X, I3, 3X, I2,1X, I4,1X, I4,1X, I5, 3X, F7.2, 2X, F7.2)
      ENDIF

      WRITE (UNIT = U, FMT = 300)
  300 FORMAT (//, 1X, 'IETA',1X, 'IPHI',1X, 'ILYR', 3X, 'RATIO',1X,
     &  'ENERGY', /)
      DO I = 1, NAID
        CALL GTCAID (I, IETA, IPHI, ILYR, SCALED_RATIO, ENERGY, OK)
        IF (OK) THEN
          WRITE (UNIT = U, FMT = 400) IETA, IPHI, ILYR,
     &                                FLOAT(SCALED_RATIO)/255.0,
     &                                ENERGY
  400     FORMAT (2X, I3, 3X, I2, 3X, I2, 3X, F5.3,1X, F7.1)
        ENDIF                           ! if ok
      ENDDO                             ! i = 1, naid
      WRITE (UNIT = U, FMT = 100)

      RETURN
      END
