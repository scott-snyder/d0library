C----------------------------------------------------------------------
      SUBROUTINE PRT0TH( LUN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0TH
C-
C-   Inputs  : LUN    [I] : The fortran output unit
C-   Outputs : Dump T0TH bank(s) on the specified unit
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, LT0TH, GZT0TH, I
C
      LT0TH = GZT0TH()
      IF (LT0TH .LE. 0) THEN
        WRITE (LUN,'(''0PRT0TH: no T0TH bank for this event'')')
        RETURN
      ENDIF
C
      WRITE(LUN,1000) (IQ(LT0TH+I),I=0,3),(Q(LT0TH+I),I=4,5)
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT('0Dump of the T0TH bank:',/,
     &       ' Status = ',Z8.8,/,
     &       ' Tracks numbers in different banks: T0TR = ',I2,
     &       ', T0TD = ',I2,', T0TZ = ',I1,/
     &       ' Vertex position: ',F7.2,' +-',F5.2)
      END
