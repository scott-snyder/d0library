      SUBROUTINE MUTWAM (JQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  reconstruct muon tracks in WAMUS detector
C-                          Uses MUTRK2
C-
C-   Inputs  :              JQUAD  Quadrant number 1-4 are central
C-                                                 5-8 are north,
C-                                                 9-12 are south
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  15-JUN-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER JQUAD,ITRAK,OLDTRK,NEWTRK
C----------------------------------------------------------------------
      CALL GTMTRH(OLDTRK)
C
C         Find tracks by quadrant
C
      CALL MUTRK2(JQUAD)
C
C         Test for found tracks
C
      CALL GTMTRH(NEWTRK)
      IF (NEWTRK.EQ.OLDTRK) RETURN
C
C         Flag track conditions for new tracks
C
      DO ITRAK = OLDTRK+1,NEWTRK
        CALL MSCINT(ITRAK)
        CALL MUIFW2(ITRAK)
        CALL MUIFW3(ITRAK)
        CALL MUIFW4(ITRAK)
      ENDDO
C
      RETURN
      END
