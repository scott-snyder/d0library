C----------------------------------------------------------------------
      SUBROUTINE PRT0TD( LUN, NT0TD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0TD
C-
C-   Inputs  : LUN    [I] : The fortran output unit
C-             NT0TD  [I] : Bank number  if 0 - all tracks
C-   Outputs : Dump T0TD bank(s) on the specified unit
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, NT0TD, LT0TD, GZT0TD, LT0TH, GZT0TH
      INTEGER       I, ISTR, IEND, NTRACK, IC
C
      IF (NT0TD .LT. 0) THEN
        WRITE (LUN,'(''0PRT0TD error: invalid track number: '',I2)')
     &    NT0TD
        RETURN
      ENDIF
C
      LT0TH = GZT0TH()
      IF (LT0TH .EQ. 0) THEN
        WRITE (LUN,'(''0PRT0TD: no T0TH bank for this event'')')
        RETURN
      ENDIF
C
      NTRACK = IQ(LT0TH+2)
      IF ((NTRACK .LE. 0).OR.(GZT0TD(NT0TD).EQ.0)) THEN
        WRITE (LUN,'(''0PRT0TD: no T0TD bank for this event'')')
        RETURN
      ENDIF
C
      IF (NT0TD .GT. NTRACK) THEN
        WRITE (LUN,'(''0PRT0TD: incorrect track number '',I2)') NT0TD
        RETURN
      ENDIF
C
      WRITE (LUN,1000) NTRACK
C
      IF (NT0TD .EQ. 0) THEN
        ISTR = 1
        IEND = NTRACK
      ELSE
        ISTR = NT0TD
        IEND = NT0TD
      ENDIF
      DO I=ISTR,IEND
        LT0TD = GZT0TD(I)
        WRITE (LUN,1010) I,(IQ(IC),IC=LT0TD+1,LT0TD+4),Q(LT0TD+5),
     &    Q(LT0TD+14),Q(LT0TD+6),Q(LT0TD+7),Q(LT0TD+15),
     &    Q(LT0TD+8),Q(LT0TD+16),Q(LT0TD+9),Q(LT0TD+10),
     &    Q(LT0TD+17),Q(LT0TD+11),MIN(Q(LT0TD+12),999.99),IQ(LT0TD+13)
      ENDDO
C
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT(
     &  '0===================== T0TD bank dump ========== Number of',
     &  ' tracks in bank = ',I2,' ===========================',/,
     &  '  # DTRK T0TZ NHIT NZ   Phi    Err    X0     Y0  ',
     &  ' Err   Theta  Err     R      Z    Err   ChiXY  ChiRZ Ndeg')
 1010 FORMAT(1X,I2,2X,I2,3X,I2,3X,I2,2X,I2,1X,F6.4,1X,F6.4,1X,F5.2,
     &  1X,F6.2,1X,F5.3,1X,F6.4,1X,F6.4,1X,F5.2,1X,F6.2,1X,F4.2,1X,F7.2,
     &  1X,F6.2,2X,I2)
      END
