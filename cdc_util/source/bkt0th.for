C----------------------------------------------------------------------
      SUBROUTINE BKT0TH( LT0TH_0 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the structure up to and including DT0T
C-               T0 detector tracks bank
C-
C-   Inputs  : none
C-   Outputs : LT0TH_0 pointing to the T0TH bank created
C-
C-   Created  23-APR-1992   Gregory L. Landsberg
C-   Updated  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$LINKS:IZT0TH.LINK'
      INTEGER       LZTRH, GZZTRH, MPT0TH(5), ISETVN, GZHSTR, LT0TH_0
      INTEGER       LVERT, GZVERT
      LOGICAL       L_FIRST
C
      DATA          L_FIRST / .TRUE. /
      DATA          MPT0TH / 0, 4, 3, 9, 0 /
C
      SAVE     L_FIRST
C----------------------------------------------------------------------
      IF( L_FIRST ) THEN
        L_FIRST = .FALSE.
        CALL UCTOH  ('T0TH', MPT0TH(1), 4, 4)
        CALL MZFORM('T0TH', '3I 6F', MPT0TH(5))
      ENDIF
      LZTRH = GZZTRH()
      IF (LZTRH.LE.0) CALL BKZTRH(LZTRH)
C
C ****  Bank T0TH, header for T0 Detector Tracks
C
      LT0TH = LQ( LZTRH - IZT0TH )
      IF ( LT0TH .LE. 0 ) THEN
        CALL MZLIFT( IXMAIN, LT0TH, LZTRH, -IZT0TH, MPT0TH, 0 )
        LT0TD = 0
        LT0TZ = 0
      ENDIF
      LQ(LT0TH - 4) = GZHSTR()        ! Reference Link to latest History
      IQ(LT0TH)     = IOR( IAND(IQ(LT0TH),'FFFC0000'X),
     &                     IAND(IQ(LZTRH),'0003FFFF'X) )
      IQ(LT0TH)     = ISETVN(IQ(LT0TH),0)
      LVERT         = GZVERT(1)
      IF (LVERT .GT. 0) THEN
        Q(LT0TH+4)    = Q(LVERT+5)
        Q(LT0TH+5)    = Q(LVERT+8)
        Q(LT0TH+6)    = Q(LVERT+3)
        Q(LT0TH+7)    = Q(LVERT+6)
        Q(LT0TH+8)    = Q(LVERT+4)
        Q(LT0TH+9)    = Q(LVERT+7)
      END IF
      LT0TH_0 = LT0TH
C
  999 RETURN
      END
