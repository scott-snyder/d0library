C----------------------------------------------------------------------
      SUBROUTINE PRT0DH( LUN, NT0TD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank T0DH
C-
C-   Inputs  : LUN    [I] : The FORTRAN output unit
C-             NT0TD  [I] : Bank number  if 0 - all tracks
C-   Outputs : Dump T0DH bank(s) on the specified unit
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INTEGER       LUN, NT0DH, LT0DH, LT0HT, GZT0HT, NHITS, IC
      INTEGER       I, ISTR, IEND, ILEVEL, NT0TD, LT0TH, IOF, NW, J
      INTEGER       NTRACK, GZT0DH,  GZT0TD

      integer z00000001
      data z00000001 / z'00000001' /
      integer z000000FE
      data z000000FE / z'000000FE' /
      integer z00000700
      data z00000700 / z'00000700' /
      integer z0000F800
      data z0000F800 / z'0000F800' /
      integer z00030000
      data z00030000 / z'00030000' /
      integer z00040000
      data z00040000 / z'00040000' /
      integer z00080000
      data z00080000 / z'00080000' /
      integer z00100000
      data z00100000 / z'00100000' /
      integer z00200000
      data z00200000 / z'00200000' /
      integer z00400000
      data z00400000 / z'00400000' /
      integer z0000FFFF
      data z0000FFFF / z'0000FFFF' /
C
      IF (NT0TD .LT. 0) THEN
        WRITE (LUN,'(''0PRT0DH error: invalid track number: '',I2)')
     &    NT0TD
        RETURN
      ENDIF
C
      LT0HT = GZT0HT()
      IF (LT0HT .EQ. 0) THEN
        WRITE (LUN,'(''0PRT0DH: no T0HT bank for this event'')')
        RETURN
      ENDIF
C
      NTRACK = IQ(LT0HT+2)
      IF (NT0TD .GT. NTRACK) THEN
        WRITE (LUN,'(''0PRT0DH: incorrect track number '',I2)') NT0TD
        RETURN
      ENDIF
C
      IF ((NTRACK.EQ.0).OR.(GZT0DH(NT0TD).LE.0)) THEN
        WRITE (LUN,'(''0PRT0DH: no T0DH bank for this event'')')
        RETURN
      ENDIF
C
      IF (NT0TD .EQ. 0) THEN
        ISTR = 1
        IEND = NTRACK
      ELSE
        ISTR = NT0TD
        IEND = NT0TD
      ENDIF
      DO J = ISTR,IEND
        LT0DH = GZT0DH(J)
        IF (LT0DH .NE. LQ(GZT0TD(J)-1))
     &    CALL ERRMSG('T0D','PRT0DH','T0DH/T0TD mismatching','S')
        NHITS = IQ(LT0DH+1)
        IF ((NHITS .LT. 0).OR.( NHITS.GT.28)) THEN
          CALL ERRMSG('T0D','PRT0DH','Incorrect number of hits','S')
          RETURN
        ENDIF
C
        NW    = IQ(LT0DH+2)
        IF (NW .NE. 3) THEN
          CALL ERRMSG('T0D','PRT0DH','Words per hit (NW) <> 3','S')
          RETURN
        ENDIF
C
        WRITE (LUN,1000) J,NHITS
        DO I=1,NHITS
          IOF = NW*(I-1) + 3
          WRITE (LUN,1010) I,
     &      iand(IQ(LT0DH+IOF),z00000001),
     &      iand(IQ(LT0DH+IOF),z000000FE)/2,
     &      iand(IQ(LT0DH+IOF),z00000700)/256,
     &      iand(IQ(LT0DH+IOF),z0000F800)/2048,
     &      iand(IQ(LT0DH+IOF),z00030000)/65536,
     &      iand(IQ(LT0DH+IOF),z00040000)/262144,
     &      iand(IQ(LT0DH+IOF),z00080000)/524288,
     &      iand(IQ(LT0DH+IOF),z00100000)/1048576,
     &      iand(IQ(LT0DH+IOF),z00200000)/2097152,
     &      iand(IQ(LT0DH+IOF),z00400000)/4194304,
     &      IQ(LT0DH+IOF)/8388608,
     &      iand(IQ(LT0DH+IOF+1),z0000FFFF)*0.1,
     &      (IQ(LT0DH+IOF+1)/65536)/100.,
     &      Q(LT0DH+IOF+2)
        ENDDO
      ENDDO
C
      RETURN
C----------------------------------------------------------------------
 1000 FORMAT(
     &  '0== T0DH bank ',I2,
     &  ' dump. Number of hits in bank = ',I2,' ==',/,
     &  '  # Sd Hit Wire Sct Lr SW OLS OL1 OL2 CDC ZTRK',
     &  ' D-Time   Z-pos   FitR')
 1010 FORMAT(1X,I2,2X,I1,1X,I3,2X,I2,2X,I2,2X,
     &  I2,1X,I2,2X,I1,3X,I1,3X,I1,3X,I1,3X,I2,2X,F6.1,1X,F7.2,1X,F6.3)
      END
