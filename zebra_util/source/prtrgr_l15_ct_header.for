      SUBROUTINE PRTRGR_L15_CT_HEADER(LUN, LTRGR_LEVEL15)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out the summary of the Level 1.5 Datablock
C-                         from the selected TRGR bank.
C-
C-   Inputs  : LUN       The unit number to write to.
C-             LTRGR_LEVEL15 The offset into IQ of the L15 Datablock crate
C-
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  23-NOV-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      INTEGER LUN, LTRGR_LEVEL15
      INTEGER CH_NLWF
C
C-----------------------------------------------------------------------
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Level 1.5 Datablock Contents (PRTRGR)'
      WRITE (LUN,*) '====================================='
      WRITE (LUN,*)
C
      IF (LTRGR_LEVEL15 .LE. 0) THEN
        WRITE (LUN,*) 'Couldn''t find LEVEL 1.5 crate'
        CALL ERRMSG('NO LEVEL 1.5 CRATE', 'PRTRGR_L15_FW_AND_CT_DBLOCK',
     &    ' Couldn''t find LEVEL 1.5 crate ', 'W')
        GOTO 999
      ENDIF
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Level 1.5 Crate Header'
      WRITE (LUN,*) '----------------------'
      WRITE (LUN,*)
      CALL PRTRGR_L15_CRATE_HEADER(LUN, IQ(LTRGR_LEVEL15), CH_NLWF)
C----------------------------------------------------------------------
  999 RETURN
      END
