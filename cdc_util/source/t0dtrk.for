C----------------------------------------------------------------------
      SUBROUTINE T0DTRK( LDTRK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Selects useful tracks for T0 detector and
C-                         books necessary track banks
C-
C-   Inputs  : LDTRK - pointer to the current DTRK track
C-   Outputs : None (banks created if track is good)
C-   Controls: None
C-
C-   Created  19-MAY-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT    NONE
      INCLUDE    'D0$INC:ZEBCOM.INC'
      INCLUDE    'D0$INC:T0DREC.INC'
      INTEGER     LDTRK
      REAL        XP, YP, ZP, RP
      REAL        RT0MIN, RT0MAX, ZT0MAX
      PARAMETER ( RT0MIN = -1., RT0MAX = 25. )
      PARAMETER ( ZT0MAX = 100. )
C
      CALL T0HITC(Q(LDTRK+7), Q(LDTRK+8), Q(LDTRK+11),
     &            Q(LDTRK+6), Q(LDTRK+9), XP, YP, ZP, RP)
      IF (XP .LE. 0) RETURN ! Bad track
C
      IF ((RP .LT. RT0MIN) .OR. (RP .GT. RT0MAX)) RETURN
      IF (ABS(ZP) .GT. ZT0MAX)                    RETURN
C
      IF (L_HIST) CALL HFILL(3,RP,0.,1.)
      CALL T0TDFL(LDTRK)
C----------------------------------------------------------------------
      RETURN
      END
