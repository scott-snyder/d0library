      SUBROUTINE FRHITS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
C----------------------------------------------------------------------
C
C    Purpose and Methods : Main routine for hitfinding in FDC sectors
C                          along a road.
C
C    Input : ZVTX = Z position of vertex for road
C            PHIMIN,PHIMAX,THEMIN,THEMAX = road parameters
C
C-   Created  ??-FEB-1989   Daria Zieminska
C-   Updated  17-APR-1989   Jeffrey Bantly
C-   Updated  23-JUL-1990   Jeffrey Bantly  book banks only if hits,
C-                                          add delay lines hits
C-   Updated  24-JAN-1991   Jeffrey Bantly  correct hit counting
C-   Updated  22-MAR-1991   Jeffrey Bantly  add ZVTX for accurate roads,
C-                                          clean up PARAMS,RCP
C-   Updated  29-AUG-1991   Robert E. Avery  Build compressed hit bank.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated  25-MAY-1992   Robert E. Avery  Remove reference to SHIFTT.
C-   Updated  25-OCT-1993   Robert E. Avery  Many changes.
C-     Restructured to allow reconstruction from compressed hits banks.
C-     This routine is now used to do FULL fdc hitfinding also.
C-     Expand FHIT banks (if they exist) to form FXSC banks.
C-     Definition of IQ(FDCH+10) has changed, now
C-              IQ(FDCH+10) = Number of SW0 hits with associated DL pulses.
C-                              (used to be the number of DL pulese).
C-     Also allow building of FCHT banks (similar to L2 compressed hits).
C-
C-   Updated  17-AUG-1994   Susan K. Blessing  Remove call to FLFSEC
C-    (done in FDROAD), replace ON array with call to FSECTOR_IN_ROAD.
C-   Updated  19-AUG-1994   Susan K. Blessing  Add call to EVNTID so that
C-    information can be passed to FHITFL.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
C Input:
C
      REAL ZVTX
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX
C Local:
C
      INTEGER HALF,UNIT,QUAD,SECTOR,STAT,IER
      INTEGER LFXSC, LFXDA, LFDUN
      INTEGER LFDCH, LFHLF, LFTQD
      INTEGER GZFXSC, GZFXDA, GZFDUN
      INTEGER GZFDCH, GZFHLF, GZFTQD
      INTEGER LFTRH, GZFTRH
      INTEGER LFHIT ,GZFHIT
      INTEGER NHITTOT, NHITDATOT, NDLTOT, NHIT, NHITDA, NDL
      INTEGER NHITHA, NHITUN, NHITQD
      INTEGER MAX_QUAD(0:1), MAX_SECT(0:1)
      INTEGER RUN,ID,RUNSAV,IDSAV
C
      LOGICAL DONE
      LOGICAL FSECTOR_IN_ROAD
      LOGICAL HITS_EXIST
      LOGICAL FULL_HITS
      LOGICAL FTPREQ
      LOGICAL BUILD_FCHT
      LOGICAL BUILD_FHIT
      LOGICAL BUILD_FULL_FHIT
      LOGICAL FIRST
      LOGICAL NEW_EVENT
C
      CHARACTER*4 PATH
C
      SAVE FIRST
      SAVE MAX_QUAD, MAX_SECT
      DATA MAX_QUAD / MXQUAD, 0 /
      DATA MAX_SECT / MXSECT, MXSECP /
      DATA BUILD_FCHT /.TRUE./
      DATA BUILD_FHIT /.TRUE./
      DATA BUILD_FULL_FHIT /.TRUE./
      DATA FIRST /.TRUE./
      DATA RUNSAV,IDSAV/-1,-1/
C
C----------------------------------------------------------------------
C
C  If PATH='GEAN', should be calling FRHITS_GEAN instead.
C
      CALL PATHGT(PATH)
      IF ( PATH .EQ. 'GEAN' ) GOTO 999
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('BUILD_FCHT',BUILD_FCHT,IER)
        CALL EZGET('BUILD_FHIT',BUILD_FHIT,IER)
        CALL EZGET('BUILD_FULL_FHIT',BUILD_FULL_FHIT,IER)
        CALL EZRSET
      ENDIF
C
      NEW_EVENT = .FALSE.
      CALL EVNTID(RUN,ID)
      IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
        NEW_EVENT = .TRUE.
      END IF
C
C Check if full compressed hits exist:
C
      LFHIT = GZFHIT()
      IF ( LFHIT.GT.0 ) THEN
        HITS_EXIST = BTEST(IQ(LFHIT),IDONE).AND.IQ(LFHIT+1).EQ.1
      ELSE
        HITS_EXIST = .FALSE.
      ENDIF
C
C  Find sectors on the road
C
      IF ( PHIMIN.NE.PHIMAX ) THEN
        FULL_HITS=.FALSE.
      ELSE
        FULL_HITS=.TRUE.
      ENDIF
C
C  Fill the 'hits' and 'data' banks one sector at a time
C
      LFDCH = GZFDCH()
      IF (LFDCH.GT.0) THEN
        NDLTOT = IQ(LFDCH+10)           ! # SW w. associated DL
      ELSE
        NDLTOT = 0
      ENDIF
C
      NHITTOT  = 0                        ! # SW hits in FDC in FxSC banks
      NHITDATOT = 0                       ! # pulses (SW+DL) in FxDA banks
      DO HALF = 0, MXHALF
        NHITHA  = 0                      ! # hits in half
        DO UNIT = 0, MXUNIT
          NHITUN  = 0                    ! # hits in theta or phi
          LFDUN=GZFDUN(HALF,UNIT)
          IF (LFDUN.LE.0) THEN
            IF( UNIT .LE. 0 ) THEN
              CALL BKFTHE(HALF,LFDUN)
            ELSE
              CALL BKFPHI(HALF,LFDUN)
            ENDIF
          ENDIF
C
          DO QUAD =  0,MAX_QUAD(UNIT)
            NHITQD  = 0                ! # hits in quad
            DO SECTOR =  0, MAX_SECT(UNIT)
C
              NHIT  = 0                ! # hits in sector
              CALL FHITCHK(HALF,UNIT,QUAD,SECTOR,2,DONE)
              IF ( FULL_HITS.OR.
     &          FSECTOR_IN_ROAD(HALF,UNIT,QUAD,SECTOR) ) THEN
                IF (.NOT.DONE) THEN
                  IF ( HITS_EXIST ) THEN
C
C Expand compressed hits bank for this sector
C
                    CALL FHIT_EXPAND(HALF,UNIT,QUAD,SECTOR,NHIT,NDL)
                    DONE = .TRUE.
                    NHITDA = 0
                  ELSE
C
C Do hit finding for this sector
C
                    CALL FSECHT(HALF,UNIT,QUAD,SECTOR,
     &                NHIT,NHITDA,NDL)
                    DONE = .TRUE.
                  ENDIF
                  CALL FHITCHK(HALF,UNIT,QUAD,SECTOR,1,DONE)
C
                  NHITDATOT = NHITDATOT + NHITDA
                  NDLTOT = NDLTOT + NDL
                ENDIF
              END IF
C
              LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)
              IF (LFXSC.GT.0) THEN
                IF ( DONE ) THEN
                  NHIT  = IQ(LFXSC+1)
                ENDIF
                IF ( FSECTOR_IN_ROAD(HALF,UNIT,QUAD,SECTOR) ) THEN
                  STAT=IQ(LFXSC)
                  IQ(LFXSC)=IBSET(STAT,ION)    ! Mark as on a road.
                END IF
              END IF
C
              NHITQD  = NHITQD + NHIT
            ENDDO                     ! end SECTOR loop
            IF( UNIT .LE. 0 ) THEN
              LFTQD = GZFTQD(HALF,QUAD)
              IF (LFTQD.GT.0) IQ(LFTQD+1) = NHITQD ! # hits in quad now
            ENDIF
            NHITUN  = NHITUN + NHITQD
          END DO                               ! end QUAD loop
          LFDUN = GZFDUN(HALF,UNIT)
          IF (LFDUN.GT.0) IQ(LFDUN+1) = NHITUN  ! # hits in UNIT now

          NHITHA  = NHITHA + NHITUN
        END DO                      ! end UNIT loop
        LFHLF = GZFHLF(HALF)
        IF (LFHLF.GT.0) IQ(LFHLF+1) = NHITHA   ! # hits in half now
        NHITTOT  = NHITTOT + NHITHA
      END DO                         ! end HALF loop
C
C Update number of hits in banks.
C
      LFDCH = GZFDCH()
      IF (LFDCH.GT.0) IQ(LFDCH+1)  = NHITTOT    ! # hits in FDC now
      IF (LFDCH.GT.0) IQ(LFDCH+10) = NDLTOT     ! # DL hits in FDC now
      LFTRH = GZFTRH()
      IF (LFTRH.GT.0) IQ(LFTRH+7)  = NHITTOT    ! Track bank.
C
C ****  Fill the compressed banks (if they don't yet exist)
C
      IF ( .NOT.HITS_EXIST  ) THEN
        DO HALF = 0, MXHALF
          DO UNIT = 0, MXUNIT
            DO QUAD =  0,MAX_QUAD(UNIT)
              DO SECTOR =  0, MAX_SECT(UNIT)
                IF ( (BUILD_FULL_FHIT.AND.FULL_HITS) .OR.
     &            BUILD_FHIT.AND.FSECTOR_IN_ROAD(HALF,UNIT,QUAD,SECTOR))
     &            THEN
                  CALL FHITFL(HALF,UNIT,QUAD,SECTOR,NEW_EVENT)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF ( FULL_HITS ) THEN
C
C Mark full compressed hits done.
C
          IF ( BUILD_FULL_FHIT ) THEN
            LFHIT = GZFHIT()
            IF ( LFHIT.GT.0 ) THEN
              IQ(LFHIT) = IBSET(IQ(LFHIT),IDONE)
            ENDIF
          ENDIF
C
C Build level2 style hit banks from FxDA banks:
C
          IF ( BUILD_FCHT ) THEN
            CALL FCHTFL(NHITDATOT)
          ENDIF
        ENDIF
      ENDIF
C
C--------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
