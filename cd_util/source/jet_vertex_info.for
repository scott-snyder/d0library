      SUBROUTINE JET_VERTEX_INFO(NJETS,VERT_ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines which tracks belong to which jets.
C-
C-
C-   Inputs  :   none.
C-   Outputs :   NJETS:     Number of Jets in this event
C-               VERT_ID    Vertex this jet has been associated with
C-
C-   Created  20-Oct-93     Brad Abbott
C-   Modfied   5-Jul-95     Adam Lyon - Modified to find tracks in all jets.
C-   Updated  21-JUL-1995   Srini Rajagopalan - Created based on routine
C-                                              JET_TRACKS from Adam Lyon.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER MAX_TRACKS,MAX_JETS,MAX_VERT
      PARAMETER (MAX_TRACKS = 100)
      PARAMETER (MAX_JETS = 30)
      PARAMETER (MAX_VERT = 3)
C
      INTEGER GZDTRK, LDTRK
      INTEGER LJETS,GZJETS
      INTEGER LVERT,GZVERT
      INTEGER LVERH,GZVERH
C
      INTEGER II, I, K, ITRK, ICONE, IER
      INTEGER NJETS, NVERT
      INTEGER TRACK_COUNTER
      INTEGER OLD_RUN_NUM,RUN_NUM,RUNNO
      INTEGER EVENT_NUM,EVONUM,OLD_EVENT_NUM
C
      INTEGER TVERT(MAX_VERT),MAXT
      INTEGER VERT_ID(MAX_JETS)
C
      REAL INFO(3,MAX_VERT)
      REAL ZVER(MAX_VERT),DZVER(MAX_VERT)
      REAL TRACK_DETA(MAX_TRACKS),TRACK_DPHI(MAX_TRACKS)
      REAL TRACK_IMPACT(MAX_TRACKS)
      REAL ZIMPACT,XYIMPACT,FJTRK(MAX_JETS)
      REAL TTHETA,TPHI,DIR(3),VTX(3)
      REAL JET_ET_CUT
C
      REAL ETAJT(MAX_JETS),PHIJT(MAX_JETS)
      REAL DET_ETA(MAX_JETS),DET_PHI(MAX_JETS)
C
      REAL DELTA_PHI,DIFF_PHI
      REAL PETA_TO_DETA,DR,M
      REAL CHIRZ,CHIXY,NHITS,NZ
      REAL RO,TRACK_RADIUS,ETAC,PHIC
      REAL BEAMX,BEAMY,XO,YO
      REAL Z_DIR, ETA_DIR
C
      REAL TEMPLATE(5,4)
      DATA TEMPLATE /
     &      1.,6.,0.7,0.,0.,                    ! CONE R=0.7
     &      1.,6.,0.5,0.,0.,                    ! CONE R=0.5
     &      1.,6.,0.3,0.,0.,                    ! CONE R=0.3
     &      2.,7.,2.,8.,2. /                    ! NN 2x2
C
      LOGICAL FIRST, OK
      DATA FIRST/.TRUE./
C
C---------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        OLD_EVENT_NUM=-999
        OLD_RUN_NUM=-999
C
C Read cone size from jet_tracks
C
        CALL EZPICK('VERTEX_FIX_RCP')
        CALL EZGET('JET_VERTEX_CONE',TRACK_RADIUS, IER)
        IF (IER.EQ.0) CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        IF (IER.NE.0)
     &     CALL ERRMSG('Bad VERTEX_FIX_RCP','JET_VERTEX_INFO',
     &                 'Required variables not found','F')
C
        IF (TRACK_RADIUS.EQ.0.7) THEN
          ICONE = 1
        ELSE IF (TRACK_RADIUS.EQ.0.5) THEN
          ICONE = 2
        ELSE IF (TRACK_RADIUS.EQ.0.3) THEN
          ICONE = 3
        ELSE
          CALL ERRMSG('BAD CONE','JET_VERTEX_INFO',
     &        'bad jet_vtx_cone size in RCP - default to 0.3','W')
          TRACK_RADIUS = 0.3
          ICONE = 3
        ENDIF
        CALL EZRSET
      ENDIF
C
      CALL VZERO(VERT_ID(1),MAX_JETS)
C
C Get vertex info
C
      CALL VERTEX_INFO( 10, NVERT, INFO, OK)
      IF ( .NOT. OK ) NVERT=0
      DO I = 1, MAX_VERT
        ZVER(I) = INFO(1,I)
        DZVER(I)= INFO(2,I)
      ENDDO
C
C Get jet eta and phi and convert to detector eta for this jet
C
      CALL SET_CAPH('CONE_JET',TEMPLATE(1,ICONE),IER)
      NJETS = 0
      LJETS = GZJETS()
      DO WHILE (LJETS.GT.0)
C
C select jets...
C
        IF (Q(LJETS+6).LT.JET_ET_CUT) GO TO 50
C
        NJETS = NJETS + 1
        IF (NJETS.GT.MAX_JETS) THEN
          CALL ERRMSG('Too Many Jets','VERTEX_INFO',
     &                'Not Using all available jets','W')
          NJETS = NJETS - 1
          GO TO 200
        ENDIF
        ETAJT(NJETS)=Q(LJETS+9)
        PHIJT(NJETS)=Q(LJETS+8)
        DET_ETA(NJETS)=PETA_TO_DETA(ETAJT(NJETS),ZVER(1))
        DET_PHI(NJETS)=PHIJT(NJETS)
   50   LJETS = LQ(LJETS)
      ENDDO
  200 CONTINUE
      CALL RESET_CAPH
C
C  Determine if same event
C
      EVENT_NUM=EVONUM()
      RUN_NUM=RUNNO()
      IF((EVENT_NUM.NE.OLD_EVENT_NUM).OR.
     &    (RUN_NUM.NE.OLD_RUN_NUM)) THEN   ! GET TRACKS ONLY ONCE PER EVENT
        OLD_EVENT_NUM=EVENT_NUM
        OLD_RUN_NUM=RUN_NUM
        TRACK_COUNTER=0                 ! NUMBER OF TRACKS IN EVENT
C
C Get Vertex/MI info
C
        LVERH=GZVERH()
        IF(LVERH.LE.0) GOTO 999
        LVERT=LQ(LVERH-1)
        IF(LVERT.LE.0) GOTO 999
        BEAMX=Q(LVERT+3)
        BEAMY=Q(LVERT+4)
C
C Loop over CDC Tracks
C
        LDTRK = GZDTRK(0)
        DO WHILE (LDTRK.GT.0)
          TTHETA = Q(LDTRK+9)
          TPHI = Q(LDTRK+6)
          VTX(1)=Q(LDTRK+7)    ! XO
          VTX(2)=Q(LDTRK+8)    ! YO
          VTX(3)=Q(LDTRK+11)   ! ZO
          RO=    Q(LDTRK+10)
C
C Throw away "bad" tracks
C
          IF (TTHETA .LT. 0.0001) GOTO 100
          NHITS = IQ(LDTRK+2)
          NZ = IQ(LDTRK+5)
          IF (NHITS.LE.2..OR.NZ.LE.2.) GOTO 100
          CHIRZ = Q(LDTRK+12)/(Nhits-2)
          CHIXY = Q(LDTRK+13)/(Nz-2)
          IF(CHIRZ.GT.100..OR.CHIXY.GT.100.) GOTO 100
          YO=VTX(2)-BEAMY
          XO=VTX(1)-BEAMX
          XYIMPACT=(YO-XO*TAN(TPHI))*COS(TPHI)
          IF(ABS(XYIMPACT).GT.2.5) GOTO 100
C
C Direction cosines
C
          DIR(1) = SIN(TTHETA)*COS(TPHI)
          DIR(2) = SIN(TTHETA)*SIN(TPHI)
          DIR(3) = COS(TTHETA)
C
C Get detector eta and phi for cdc tracks
C
          ETA_DIR   = -ALOG( TAN( TTHETA/2.0 ) )
          IF ( ABS(SIN( TTHETA )) .LT. .001 ) THEN
            Z_DIR   = VTX(3)
          ELSE
            Z_DIR   = VTX(3) - SQRT(VTX(2)**2+VTX(1)**2)/TAN( TTHETA )
          ENDIF
          ETAC =  PETA_TO_DETA( ETA_DIR, Z_DIR )
          PHIC =  TPHI
          M=1/TAN(TTHETA)
          TRACK_COUNTER=TRACK_COUNTER+1
          TRACK_DETA(TRACK_COUNTER)=ETAC
          TRACK_DPHI(TRACK_COUNTER)=PHIC
          TRACK_IMPACT(TRACK_COUNTER)=VTX(3)-RO*M
C
          IF(TRACK_COUNTER.GE.MAX_TRACKS) THEN
            CALL ERRMSG('Too many tracks','JET_VERTEX_INFO',
     &                  'Not using all available tracks','W')
            GOTO 300
          ENDIF
C
  100     LDTRK=LQ(LDTRK)
        ENDDO
C
      ENDIF
  300 CONTINUE
C
C Determine tracks that are within each jet
C
      DO II = 1,NJETS                       ! loop over jets
        ITRK = 0                            ! num. of tracks in jet
        CALL VZERO(TVERT(1),MAX_VERT)
C
C loop over all tracks and associate them to this jet
C
        DO I = 1,TRACK_COUNTER
          DELTA_PHI=DIFF_PHI(TRACK_DPHI(I),DET_PHI(II))
          DR=SQRT((TRACK_DETA(I)-DET_ETA(II))**2+(DELTA_PHI)**2)
C
          IF (DR.LE.TRACK_RADIUS) THEN
            ITRK = ITRK + 1                 ! This track in jet
C
C find which vertex this track points to based on min impact
C TVERT = number of tracks pointing to each vertex
C
            ZIMPACT = 9999.
            LVERT = GZVERT(1)
            DO WHILE (LVERT.GT.0)
              IF (ABS(TRACK_IMPACT(I)-Q(LVERT+5)).LE.ZIMPACT) THEN
                ZIMPACT = ABS(TRACK_IMPACT(I)-Q(LVERT+5))
                K = IQ(LVERT-5)
              ENDIF
              LVERT = LQ(LVERT)
            ENDDO
            IF (ZIMPACT.NE.9999.) TVERT(K) = TVERT(K) + 1
C
          ENDIF
        ENDDO                               ! end loop over tracks
C
C Determine which vertex the jet points
C VERT_ID = vertex which the jet points to
C FJTRK = Fraction of tracks in this jet pointing to the said vertex
C
        MAXT = 0
        DO K = 1,NVERT
          IF (TVERT(K).GT.MAXT .AND. TVERT(K).GT.0) THEN
            MAXT = TVERT(K)
            VERT_ID(II) = K                   ! jet II points to vertex K
            FJTRK(II) = FLOAT(TVERT(K))/FLOAT(ITRK)
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN
      END
