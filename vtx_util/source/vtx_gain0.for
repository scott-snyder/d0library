      SUBROUTINE VTX_GAIN0(LAY,SEC,WIR,VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the dynamic adjustment portion of the VGNL
C-               bank, that is, the adjustment made by VTX_DYNADJ.
C-
C-    NOTE:  user must CALL VTX_GAIN_SAVE prior to the first call to VTX_DYNADJ
C-           (e.g. before VTRAKS or VTROAD is called.)  If multiple runs are
C-           processed (why would you want that?), it must be called before
C-           VTX_DYNADJ is called for each RUN
C-
C-   Inputs  : LAY,SEC,WIR = wire id
C-   Outputs : VALUE       = (gain w/dymamic adjust)/(gain w/o dynamic adjust)
C-   Controls: 
C-
C-   Created  18-JAN-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAY,SEC,WIR
      REAL    VALUE
C LOCALS:
      INTEGER LAYER,SECTOR,WIRE,PT,LVGNL,LAST_RUN,DYN_BIT
      REAL    SAVE(0:767)
C EXTERNALS:
      INTEGER GZVGNL,RUNNO,GZVGNH
C DATA:
      DATA LAST_RUN/-1/
      PARAMETER (DYN_BIT = 0)
C----------------------------------------------------------------------
      LVGNL = GZVGNL(LAY)
      IF (SAVE(256*LAY+8*SEC+WIR) .EQ. 0.)
     &    CALL ERRMSG('VGNL not saved',
     &                'VTX_GAIN0',
     &                'VTX_GAIN_SAVE not called prior to VTX_DYNADJ',
     &                'F')
      PT = LVGNL + (SEC*16 + WIR*2) + 3*41 + 6
      VALUE = C(PT)/SAVE(256*LAY+8*SEC+WIR)
      GO TO 999
C
C ****  SAVE VGNL
C
      ENTRY VTX_GAIN_SAVE
      IF (LAST_RUN .NE. -1) GO TO 999  ! Entry must be called only once per job
      LAST_RUN = RUNNO()
C
C ****  check to see if a new VGNH was read in (from DBL3).  If not, then
C ****  already saved VGNL's are still valid
C
      LVGNH = GZVGNH()
      IF (BTEST(IC(LVGNH),DYN_BIT)) GO TO 999
      CALL ERRMSG('VGNL banks saved',
     &            'VTX_GAIN_SAVE',
     &            'VTX gain banks saved for VTX_GAIN0','W')
      DO LAYER = 0,2
        LVGNL = GZVGNL(LAYER)
        DO SECTOR = 0,IC(LVGNL+5) - 1
          DO WIRE = 0,7
            PT = LVGNL + (SECTOR*16+WIRE*2) + 3*41 + 6
            SAVE(256*LAYER + 8*SECTOR + WIRE) = C(PT)
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
