C----------------------------------------------------------------------
      SUBROUTINE PRT0TZ( LUN, NT0TZ, ILEVEL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0TZ
C-
C-   Inputs  : LUN    [I] : The fortran output unit
C-             NT0TZ  [I] : Bank number (if 0 - dump all tracks)
C-             ILEVEL [I] : Print level (if >=1, print direction cosines)
C-   Outputs : Dump T0TZ bank(s) on the specified unit
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, NT0TZ, ILEVEL, GZT0TZ, LT0TZ, GZT0TH, LT0TH
      INTEGER       NTRACK, IC, I, ISTR, IEND
C
      IF (NT0TZ .LT. 0) THEN
        WRITE (LUN,'(''0PRT0TZ error: invalid track number: '',I2)')
     &    NT0TZ
        RETURN
      ENDIF
C
      LT0TH = GZT0TH()
      IF (LT0TH .EQ. 0) THEN
        WRITE (LUN,'(''0PRT0TZ: no T0TH bank for this event'')')
        RETURN
      ENDIF
C
      NTRACK = IQ(LT0TH+3)
      IF (NT0TZ .GT. NTRACK) THEN
        WRITE (LUN,'(''0PRT0TZ: incorrect track number '',I2)') NT0TZ
        RETURN
      ENDIF
C
      IF ((NTRACK .LE. 0).OR.(GZT0TZ(NT0TZ).LE.0)) THEN
        WRITE (LUN,'(''0PRT0TZ: no T0TZ bank for this event'')')
        RETURN
      ENDIF
C
      IF (NT0TZ .EQ. 0) THEN
        ISTR = 1
        IEND = NTRACK
      ELSE
        ISTR = NT0TZ
        IEND = NT0TZ
      ENDIF
      WRITE (LUN,1000) NTRACK
C
      DO I=ISTR,IEND
        LT0TZ = GZT0TZ(I)
        WRITE (LUN,1010) I,(IQ(IC),IC=LT0TZ+1,LT0TZ+4),
     &    IQ(LT0TZ+7).AND.'0000FFFF'X,
     &    IQ(LT0TZ+7)/65536,
     &    IQ(LT0TZ+8).AND.'0000FFFF'X,
     &    IQ(LT0TZ+8)/65536,
     &    IQ(LT0TZ+9),IQ(LT0TZ+10),
     &    Q(LT0TZ+5),Q(LT0TZ+6),
     &    Q(LT0TZ+13),Q(LT0TZ+19),Q(LT0TZ+14),Q(LT0TZ+15),
     &    Q(LT0TZ+20),Q(LT0TZ+16),Q(LT0TZ+21),Q(LT0TZ+17),
     &    Q(LT0TZ+18),Q(LT0TZ+22),Q(LT0TZ+11),Q(LT0TZ+12)
        IF ( ILEVEL .GE. 1 ) WRITE (LUN,1020) (Q(LT0TZ+IC),IC=23,28)
      ENDDO
C
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT(
     &  '0===================== T0TZ bank dump ========== Number of',
     &  ' tracks in bank = ',I2,' =================================',
     &  '===============',/,
     &  '  # ZTRK VTX CDC T0TD NVTX  NCDC NTOT   QPhi  QTht   Phi',
     &  '   Err    X0    Y0  ',
     &  ' Err  Theta  Err     R      Z    Err   ChiXY  ChiRZ')
 1010 FORMAT(1X,I2,2X,I2,2X,I2,2X,I2,3X,I2,2X,
     &  I2,'/',I2,1X,I2,'/',I1,1X,I2,'/',I2,1X,F5.4,1X,F5.4,
     &  1X,F6.4,1X,F5.4,1X,F5.2,1X,F6.2,1X,F4.3,1X,F6.4,1X,F5.4,1X,F5.2,
     &  1X,F6.2,1X,F5.2,1X,F6.1,1X,F6.1)
 1020 FORMAT(38X,'Cos(Alpha) = ',F7.4,'+-',F6.4,';  ',
     &           'Cos(Beta) = ',F7.4,'+-',F6.4,';  ',
     &           'Cos(Gamma) = ',F7.4,'+-',F6.4)
      END
