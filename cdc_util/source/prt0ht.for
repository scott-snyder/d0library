C----------------------------------------------------------------------
      SUBROUTINE PRT0HT( LUN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0HT
C-
C-   Inputs  : LUN    [I] : The fortran output unit
C-   Outputs : Dump T0HT bank(s) on the specified unit
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, LT0HT, GZT0HT, I
C
      LT0HT = GZT0HT()
      IF (LT0HT .EQ. 0) THEN
        WRITE (LUN,'(''0PRT0HT: no T0HT bank for this event'')')
        RETURN
      ENDIF
      WRITE(LUN,1000) (IQ(LT0HT+I),I=0,2)
C
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('0Dump of the T0HT bank:',/,
     &       ' Status = ',Z8.8,/,
     &       ' Tracks numbers in different banks: T0FH = ',I1,
     &       ', T0DH = ',I2)
      END
