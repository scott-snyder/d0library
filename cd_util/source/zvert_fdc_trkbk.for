      SUBROUTINE ZVERT_FDC_TRKBK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book histograms for FDC vertex finding.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  22-MAY-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INTEGER LISAE ,GZISAE
      LOGICAL FIRST
C
      SAVE FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
C
        CALL HBOOK1(503,'R(Trk) (cm), FDC',
     &    50,0.,10.,0.0)
C
        CALL HBOOK1(510,'Z-Vert (cm), FDC',
     &  100,-100.,100.,0.0)
        CALL HBOOK1(511,'Num tracks in vertex, FDC',
     &  50,0.0,50.,0.0)
        CALL HBOOK1(512,'Z(Trk) - Z(Vrt) (cm), FDC',
     &  100,-20.,20.,0.0)
        CALL HBOOK1(516,'Num iterations, FDC',
     &  50,0.0,50.,0.0)
C
        CALL HBOOK2(514,'Z(Trk) - Z(Vrt) vs nused, FDC',
     &  50,  0.,50.,
     &  40,-20.,20.,0.0)
        CALL HBOOK2(515,'Error, Z(Vrt),  vs nused, FDC',
     &  50,  0., 50.,
     &  20,  0., 10.,0.0)
C
        LISAE = GZISAE()
        IF (LISAE .GT. 0) THEN
C
          CALL HBOOK2(550,'(Z(FDC)-Z(isa)) vs Nused',
     &    50,  0., 50.,
     &    40,-20.,20.,0.0)
C
          CALL HBOOK2(551,'(Z(FDC)-Z(isa)) vs Zerror',
     &    40,0.,10.,
     &    40,-20.,20.,0.0)
C
          CALL HBOOK2(552,'(Z(FDC)-Z(isa)) vs Z(ISA)',
     &    25,-100.,100.,
     &    40,-20.,20.,0.0)
C
        ENDIF
C
      END IF
C
  999 RETURN
      END
