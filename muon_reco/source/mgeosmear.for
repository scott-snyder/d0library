      SUBROUTINE MGEOSMEAR( IMOD, SHIFT, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WAMUS geometry smearing
C-                         currently support only shift in drift view
C-
C-   Inputs  : IMOD     module ID
C-             SHIFT    shift in drift view [cm]
C-   Outputs : IER      error code 0-ok, 1-bad
C-   Controls: none
C-
C-   Created   9-MAR-1993   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IMOD, IER
      REAL     SHIFT
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  LM, GZMGEO, IB
      INTEGER  I1, I2
C----------------------------------------------------------------------
      IER = 1
C
      LM = GZMGEO(IMOD)
      IF ( LM.EQ.0 ) GOTO 999
C
      I1 = MOD( IMOD, 10 )
      I2 = MOD( IMOD,100 )/10
C
      IF ( I2.LE.4 ) THEN
        IB = 22
      ELSE
        IF ( I1.EQ.1 .OR. I1.EQ.2 .OR.
     1       I1.EQ.5 .OR. I1.EQ.6 ) THEN
          IB = 21
        ELSE IF ( I1.EQ.3 .OR. I1.EQ.4 .OR.
     1            I1.EQ.7 .OR. I1.EQ.0 ) THEN
          IB = 20
        ELSE
          GOTO 999
        END IF
      END IF
C
      C(LM+IB) = C(LM+IB) + SHIFT
C
      IER = 0
C
  999 RETURN
      END
