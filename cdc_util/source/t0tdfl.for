C----------------------------------------------------------------------
      SUBROUTINE T0TDFL(LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store a new track in the T0TD bank
C-
C-   Inputs  : LDTRK - pointer to DTRK bank with track
C-   Outputs : New DTRK bank
C-
C-   Created  24-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$LINKS:IZT0TD.LINK'
      INTEGER       ISETVN
      INTEGER       LDTRK, T0TZFL, GZT0TH, LT0TD1, GZT0TD, LT0TH1
      INTEGER       MPT0TD(5), NTRACK
      LOGICAL       L_FIRST
C
      DATA          MPT0TD / 0, 1, 0, 18, 0 /
      DATA          L_FIRST / .TRUE. /
C
      SAVE          L_FIRST
C----------------------------------------------------------------------
      IF ( L_FIRST ) THEN
        L_FIRST = .FALSE.
        CALL UCTOH ('T0TD', MPT0TD(1), 4, 4)
        CALL MZFORM('T0TD','4I 8F 1I 4F', MPT0TD(5))
      ENDIF
      T0DLNT2(1) = 1        ! Activate temporary link area
C
      IF ( LT0TD .EQ. 0 ) THEN
C
C ****  First track. Book from T0TH.
C
        LT0TH = GZT0TH()
        IF (LT0TH .LE. 0) CALL BKT0TH( LT0TH1 )
        LT0TH = LT0TH1
        CALL MZLIFT( IXMAIN, LT0TD1, LT0TH, -IZT0TD, MPT0TD, 0 )
        LT0TD = LT0TD1
        IQ( LT0TD - 5 ) = 1
        NTRACK = 0
      ELSE
        NTRACK = NTRACK + 1
        CALL MZLIFT( IXMAIN, LT0TD1, LT0TD, 0, MPT0TD, 0 )
        LT0TD = LT0TD1
      ENDIF
C
C ****  Update DTRAK tracks counter
C
      IQ( LT0TH+2 ) = IQ( LT0TH+2 ) + 1
C
C ****  Now fill the values
C
      IQ( LT0TD   ) = ISETVN(IQ(LT0TD),0)
      IQ( LT0TD+1 ) = IQ( LDTRK - 5 )
      IQ( LT0TD+2 ) = T0TZFL( LDTRK )
      IQ( LT0TD+3 ) = IQ( LDTRK + 2 )
      IQ( LT0TD+4 ) = IQ( LDTRK + 5 )
      Q(  LT0TD+5 ) = Q(  LDTRK + 6 )
      Q(  LT0TD+6 ) = Q(  LDTRK + 7 )
      Q(  LT0TD+7 ) = Q(  LDTRK + 8 )
      Q(  LT0TD+8 ) = Q(  LDTRK + 9 )
      Q(  LT0TD+9 ) = Q(  LDTRK + 10)
      Q(  LT0TD+10) = Q(  LDTRK + 11)
      Q(  LT0TD+11) = Q(  LDTRK + 12)
      Q(  LT0TD+12) = Q(  LDTRK + 13)
      IQ( LT0TD+13) = IQ( LDTRK + 14)
      Q(  LT0TD+14) = Q(  LDTRK + 16)
      Q(  LT0TD+15) = Q(  LDTRK + 17)
      Q(  LT0TD+16) = Q(  LDTRK + 18)
      Q(  LT0TD+17) = Q(  LDTRK + 19)
      Q(  LT0TD+18) = Q(  LDTRK + 22)
C
C ****  Now fill hits bank
C
      CALL T0DHFL
      LQ(LT0TD - 1) = LT0DH
C
      T0DLNT2(1) = 0        ! Deactivate temporary link area
      RETURN
      END
