      SUBROUTINE SAVE_VTXT(LVTXT,LTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-            o  Set the IUSED on the track with link LVTXT.
C-            o  Use VTXT reference link to store LTRAK link (not a ZTRK)
C-            o  Delete all VTXTs that do NOT have the IUSED bit set;
C-            o  Update track count in VTRH bank
C-
C-   Inputs  : LVTXT -- vertex track link matched to LTRAK
C-             LTRAK -- CDC or FDC link matched to VTXT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-DEC-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
c I/O:
      INTEGER LVTXT,LTRAK
c Locals:
      INTEGER LVTRH,LVTXT1,DROP,LNEXT,TOTAL,TRK
c External:
      INTEGER JBIT,GZVTRH,NZBANK
C----------------------------------------------------------------------
      LVTRH = GZVTRH()
      IF (LVTRH .EQ. 0) GO TO 999
      IF (LVTXT .GT. 0) THEN
        LQ(LVTXT-2) = LTRAK
        CALL MZFLAG(IXCOM,LVTXT,IUSED,' ')
        CALL MZFLAG(IXCOM,LTRAK,IUSED,' ')
      ENDIF
      LVTXT1= LQ(LVTRH - IZVTXT)
      TOTAL = NZBANK(IXCOM,LVTXT1)
      DROP = 0
      DO WHILE (LVTXT1 .GT. 0)
        LNEXT = LQ(LVTXT1)
        IF (JBIT(IQ(LVTXT1),IUSED) .EQ. 0) THEN
          IQ(LVTRH+2) = IQ(LVTRH+2) - 1
          IQ(LVTRH+5) = IQ(LVTRH+5) - 1
          DROP = DROP + 1
          CALL MZDROP(IXCOM,LVTXT1,' ')
        ENDIF
        LVTXT1 = LNEXT
      ENDDO
      IF (DROP .GT. 0) THEN
        TRK = TOTAL - DROP
        LVTXT1 = LQ(LVTRH-IZVTXT)
        DO WHILE (LVTXT1 .GT. 0)
          IQ(LVTXT1-5) = TRK
          TRK = TRK - 1
          LVTXT1 = LQ(LVTXT1)
        ENDDO
      ENDIF
  999 RETURN
      END
