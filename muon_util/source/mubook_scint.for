      SUBROUTINE MUBOOK_SCINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books Histograms for scintillator T0 calibration
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-DEC-1994   R. Markeloff
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*3 CMODU
      CHARACTER*40 DTTIT
      INTEGER MODNUM, ID, I, J
C
C     Loop over CMC modules and book scintillator histograms
C
      DO I = 200, 240, 10
        DO 90 J = 0, 7
          IF (I .EQ. 220 .AND. (J .EQ. 5 .OR. J .EQ. 6)) GOTO 90
          MODNUM = I + J
          WRITE(CMODU,101)MODNUM
          ID = 40000 + MODNUM
          DTTIT = 'TOF - Scintillator Time, Mod '//CMODU
          CALL HBOOK1(ID,DTTIT,200,-100.,100.,0.)
          ID = 41000 + MODNUM
          DTTIT = 'Raw Scint. Times, Mod '//CMODU
          CALL HBOOK1(ID,DTTIT,240,0.,2400.,0.)
   90   CONTINUE
      ENDDO
C
      CALL HBOOK1(49500,'T0 corrections',40,-20.,20.,0.)
      CALL HBOOK1(49600,'Sigmas',40,0.,20.,0.)
C
  101 FORMAT(I3)
  102 FORMAT(I2)
  999 RETURN
      END
