C+
      SUBROUTINE SASTAT (NST, ST_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define GEANT volume of SAMUS station
C-
C-   Inputs  : NST - station number
C-   Outputs : ST_NAME - name of the station volume
C-                       (as mother for the tubes)
C-
C-   Created  29-SEP-1990   A.Kiryunin
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH.
C-   Updated  23-NOV-1992   Alexander Efimov: change format of the
C-                          station geometry banks - add Ailer angles.
C-   Updated  17-JAN-1994   Alexander Efimov: fix bags in rotating
C-                          parameters.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SRCPR.INC'
      INTEGER NST
      CHARACTER*4 ST_NAME
      INTEGER LSSTA, GZSSTA, I, J
      REAL    R1, R2, ZC
      REAL    CENTER(3), ANGLES(3), SIZE(3), HOLE(3)
      REAL*8  V1(3), V2(3), SYS(6), ST
      REAL    THETA(3), PHI(3)
      CHARACTER*80 MSGSTR               ! Error message
      REAL    PI, TWOPI, DEGRAD
      PARAMETER (PI=3.14159265, TWOPI=2.0*PI, DEGRAD=PI/180.0)
C
C ****  Get address of the needed SSTA bank
C
      LSSTA = GZSSTA(NST)
      IF (LSSTA.EQ.0) THEN
        MSGSTR = ' *** SASTAT: needed bank SSTA does not exist '
        CALL INTMSG (MSGSTR)
        GOTO 999
      ENDIF
C
C ****  Get parameters for VOLPOS1
C
      DO I = 1, 10
        ISRCPR(I) = IC(LSSTA+2+I)
      END DO
      NPAR = 10
      PAR(1) = 45.0
      PAR(2) = 360.0
      PAR(3) = 4.0
      PAR(4) = 2.0
      CALL SAGSTA (NST, CENTER, ANGLES, SIZE, HOLE)
      R1 = AMIN1 (HOLE(1), HOLE(2))           ! Minimum size of hole
      R2 = AMAX1 (SIZE(1), SIZE(2))           ! Maximum size of station
      ZC = SIZE(3)
      PAR( 5) = -ZC
      PAR( 6) = R1
      PAR( 7) = R2
      PAR( 8) = +ZC
      PAR( 9) = R1
      PAR(10) = R2
C
C ****  create rotation matrix
C
      IROT = IC(LSSTA+8)
      DO J = 1, 3
        SYS(J) = 0.0
        SYS(J+3) = ANGLES(J)
      END DO
      DO I = 1, 3
        DO J = 1, 3
          V1(J) = 0.0
        END DO
        V1(I) = 1.0
        CALL SAGLSY (V1, SYS, V2)
        THETA(I) = ACOS (V2(3))
        ST = SIN (THETA(I))
        IF (ABS(ST) .LT. 1.0E-13) THEN
          PHI(I) = 0.0
        ELSE
          PHI(I) = ACOS (V2(1) / ST)
          IF (V2(2) .LT. 0.0) PHI(I) = TWOPI - PHI(I)
        END IF
        THETA(I) = THETA(I) / DEGRAD
        PHI(I) = PHI(I) / DEGRAD
      END DO
      CALL GSROTM (IROT, THETA(1), PHI(1), THETA(2), PHI(2),
     &                   THETA(3), PHI(3))
C
C ****  Define GEANT volume
C
      CALL VOLPOS1
C
      CALL UHTOC (NAME, 4, ST_NAME, 4)
C
  999 RETURN
      END
