      SUBROUTINE PZTRAKS_RPHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw central detector tracks (ZTRKs) in 
C-           R-Phi view. For Collider data, track is drawn from point of
C-           closest approach to beam to the outer most tracking chamber.
C-           For Cosmic ray data, track is drawn all the way through all
C-           relevant chambers.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   5-AUG-1991   Robert E. Avery 
C-              (Loosely based on PZTRK_3D, by Qizhong Li-Demarteau)
C-   Updated  27-MAR-1992   Robert E. Avery  Implement color by particle type,
C-       Only draw matched tracks.
C-   Updated   9-JUL-1992   Robert E. Avery  Change name of rcp parameter 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZTRK.LINK'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
C
      INTEGER GZZTRH
      INTEGER IER 
      INTEGER LZTRH, LZTRK, LZFIT
      INTEGER ID_FDC,ID_CDC, ID_VTX 
      INTEGER STATUS 
      INTEGER PFPHICHK,RSIGN
C
      REAL    PHI,XC,YC
      REAL    THETA,ZC
      REAL    XPOS1, YPOS1, ZPOS1
      REAL    XPOS2, YPOS2, ZPOS2
C
      LOGICAL DRAW_TRACKS 
      LOGICAL EZERROR
      LOGICAL BTEST
      CHARACTER*4 ZFIT_CLR
C
      DATA    ZFIT_CLR /'RED'/
C----------------------------------------------------------------------
C
C ****  Pick PIXIE RCP bank
C
      CALL EZPICK('PX_ZTRAKSDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG( 'PIXIE','PZTRAKS_RZ',
     &               'Cannot find PX_ZTRAKSDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('ZTRAKS DRAW ZTRKS',DRAW_TRACKS)
      CALL PUGETA('ZTRAKS COLR ZFIT',ZFIT_CLR)
      CALL EZRSET
C
      IF ( .NOT. DRAW_TRACKS ) GOTO 999
      CALL PATHRS
      LZTRH = GZZTRH()
      IF (LZTRH.EQ.0) GOTO 999
C
      CALL PUOPEN
      CALL JLSTYL(2)
C
C  Loop over ZTRK's
C
      LZTRK = LQ(LZTRH-IZZTRK)               
      DO WHILE (LZTRK.NE.0)
        LZFIT = LQ(LZTRK-IZZFIT)
        IF (LZFIT .NE. 0) THEN
C
          ID_VTX = IQ(LZTRK+2)
          ID_CDC = IQ(LZTRK+3)
          ID_FDC = IQ(LZTRK+4)
C
C Only plot matched tracks          
          IF (   ( (ID_VTX.EQ.0) .AND. (ID_CDC.EQ.0) )
     &      .OR. ( (ID_VTX.EQ.0) .AND. (ID_FDC.EQ.0) )
     &      .OR. ( (ID_CDC.EQ.0) .AND. (ID_FDC.EQ.0) ) ) THEN
            GOTO 100                    
          ENDIF
C
          PHI = Q(LZFIT+10)
          XC = Q(LZFIT+11)
          YC = Q(LZFIT+12)
          THETA = Q(LZFIT+13)
          ZC = Q(LZFIT+15)
C
C Set color for tracks in roads depending on particle type
C
          STATUS = IQ(LZTRK)
          CALL PXCOLR(ZFIT_CLR)
          IF (BTEST(STATUS,MUROAD))  CALL PXCOLR('GRE')
          IF (BTEST(STATUS,ELROAD))  CALL PXCOLR('RED')
          IF (BTEST(STATUS,TAUROAD)) CALL PXCOLR('CYA')
          IF (BTEST(STATUS,5))       CALL PXCOLR('YEL')
C
C  Get first and last point
C
          CALL PZFIT_POINTS( ID_CDC,ID_FDC,
     &                       PHI,XC,YC,THETA,ZC,
     &                       XPOS1,YPOS1,ZPOS1,
     &                       XPOS2,YPOS2,ZPOS2)
C
C  Draw track:
C
          CALL JMOVE(XPOS1, YPOS1)
          CALL JDRAW(XPOS2, YPOS2)
C
        ENDIF 
  100   CONTINUE
        LZTRK = LQ(LZTRK)                 ! pointer to next bank 'ZTRK' 
      ENDDO
      CALL JLSTYL(0)
      CALL JRCLOS
C
  999 CONTINUE
      RETURN
      END
