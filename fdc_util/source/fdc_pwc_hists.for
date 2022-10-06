      FUNCTION FDC_PWC_HISTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill histograms for PWC analysis of FDC
C-   data. This version makes tight cuts on track quality.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-JUN-1990   Robert E. Avery
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_PWC_HISTS
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IADD                      ! PACKED ADDRESS
      INTEGER ID                        ! HISTOGRAM ID
      INTEGER IER                       ! ERROR FOR EZGET
      INTEGER IPTR,DPTR                 ! POINTERS TO HITS IN SC, DA BANKS
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
      INTEGER MODULE,LAYER              ! FOR TRACKING COORD, =HALF*3+LAYER
      INTEGER SEG                  ! NUM OF SEGMENTS IN A PHI OR THETA
      INTEGER LFDTH
      INTEGER LFXSC,LFXDA         ! POINTERS TO FSEG, FxSC, FxDA BANKS
      INTEGER LFTRH,GZFTRH
      INTEGER GZFXSC,GZFXDA,GZFSEG      ! TO GET ABOVE POINTERS
      INTEGER LFDCT,GZFDCT
      INTEGER NHIT,HIT                  ! NUMBER OF HITS ON SEGMENT/TRACK
      INTEGER NSEN                      ! NUMBER OF SENSE WIRES IN A SECTOR
      INTEGER NTRACK                    ! NUMBER OF FDC TRACKS
      INTEGER NHTRK,HTRK                ! NUMBER OF HITS ON TRACK, COUNTER
      INTEGER UBIT                      ! USED/UNUSED CHANNEL
      INTEGER XSECT_HIT 
      INTEGER LADDER(0:2)
      INTEGER MINHIT
      INTEGER LR
C
      REAL QTRAK(26),QHTRK(3,34)        ! TRACK INFORMATION
      REAL XPOS,YPOS,ZPOS               ! POSITION ON CHAMBER
      REAL Z0(2)
      REAL Z_OFFSET
      REAL X_FDC,Y_FDC,Z_FDC         ! POSITION OF FDC REL. TO PWC1
      REAL X_BEAM ,Y_BEAM
      REAL X_PWC_0,Y_PWC_0           ! POSITION OF PWC TRACK, PWC COORDS.
      REAL DX_PWC_0,DY_PWC_0         ! SLOPE OF PWC TRACK, PWC COORDS.
      REAL X_PWC,Y_PWC               ! POSITION OF PWC TRACK
      REAL DX_PWC,DY_PWC             ! SLOPE OF PWC TRACK
      REAL X_ROT ,Y_ROT
      REAL DX_ROT ,DY_ROT
      REAL X_SLOPE,X_DRIFT,Y_DL
      REAL DRIFT_TIME
      REAL X_RESIDUAL, ANGLE_RESIDUAL, DL_RESIDUAL
      REAL Y_RESIDUAL
      REAL X_ANGLE
      REAL Y_ANGLE
      REAL X_COS
      REAL Y_COS
      REAL STAGGER,FSTAGR
      REAL TRACK_CUT
      REAL    PH_SW0,PH_HV,PH_SIG
      REAL    DT_SW0,DT_HV,DT_SIG
      REAL    DELVEL,DELLEN 
      REAL    DL_DIFF 
      REAL    DL_LEN 
C
      LOGICAL DL_FOUND
      LOGICAL OK  
      LOGICAL FDEXST
      LOGICAL FIRST
      LOGICAL FDC_PWC_TRACK
C
      INTEGER ICONT(62)
      REAL CONT(62)                     ! CONTENTS OF FSGx BANKS
      EQUIVALENCE (CONT,ICONT)
C
      DATA TRACK_CUT /1.0/           ! CM (TRACK - PWC RESIDUAL)
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      FDC_PWC_HISTS = .TRUE.
      IF (FIRST) THEN
C
C Get center of Phi (and FDC) in z:
C
        CALL GTFALH(0,1,0,0,7,X_FDC,Y_FDC,Z_FDC)
        Z_OFFSET =  Z_FDC/2.
        CALL GTFALH(0,1,0,0,8,X_FDC,Y_FDC,Z_FDC)
        Z_OFFSET =  Z_OFFSET + Z_FDC/2.
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_rarr('Z0',Z0,IER)
        CALL EZRSET
        CALL EZPICK('FDC_RCP')
        CALL EZGET('X_FDC',X_FDC,IER)
        CALL EZGET('Y_FDC',Y_FDC,IER)
        CALL EZGET('X_ANGLE',X_ANGLE,IER)
        CALL EZGET('Y_ANGLE',Y_ANGLE,IER)
        CALL EZGET_i('MINHIT',MINHIT,IER)
        CALL EZGET('TRACK_CUT',TRACK_CUT,IER)
        CALL EZRSET
C
        X_COS = COS(X_ANGLE) 
        Y_COS = COS(Y_ANGLE) 
        FIRST = .FALSE.
      END IF
      CALL PATHST('RECO')
      HALF = 0                          ! ALWAYS =0 FOR TB (FOR PWC
C                                       ! TRACK TO MAKE SENSE)
C
      CALL HF1(1100,0.,1.)
      IF (
     &  .NOT. FDC_PWC_TRACK(
     &          X_PWC_0,Y_PWC_0,DX_PWC_0,DY_PWC_0,
     &          X_PWC,Y_PWC,DX_PWC,DY_PWC)
     &                          ) THEN
        GOTO 999
      ENDIF
      CALL HF1(1100,1.,1.)
C
C HISTOGRAMS INVOLVING COMPLETE TRACKS
C GET NUMBER OF FDC TRACKS

      LFTRH = GZFTRH()
      NTRACK = IQ(LFTRH+2)
      IF (NTRACK .NE. 1) THEN
        GOTO 999           ! only allow 1 real track per event
      END IF
      CALL HF1(1100,2.,1.)
C
      LFDCT=GZFDCT(1)
      CALL GTFDCT(1,QTRAK,QHTRK,LADDER)
      NHTRK=IQ(LFDCT+2)
      IF ( NHIT.LT.MINHIT ) THEN
        GOTO 999           ! with enough hits
      ENDIF
      CALL HF1(1100,3.,1.)
C
C  FDC TRACK IN BEAM COORDINATES
      HALF = 0
      XPOS = X_COS * (Q(LFDCT+4) + Q(LFDCT+7)*(Z_OFFSET-Z0(HALF+1)))
     &       + X_FDC
      YPOS = Y_COS * (Q(LFDCT+5) + Q(LFDCT+8)*(Z_OFFSET-Z0(HALF+1)))
     &       + Y_FDC
C
      CALL HF2(1110,X_PWC_0,XPOS,1.)
      CALL HF2(1120,Y_PWC_0,YPOS,1.)
      CALL HF2(1130,DX_PWC,Q(LFDCT+7),1.)
      CALL HF2(1140,DY_PWC,Q(LFDCT+8),1.)
C
C  PWC CENTER IN FDC COORDINATES
C
      X_BEAM = 
     &      X_COS * (Q(LFDCT+4) + Q(LFDCT+7)*(Z_OFFSET-Z0(HALF+1)))
     &      - X_PWC_0
      Y_BEAM = 
     &      Y_COS * (Q(LFDCT+5) + Q(LFDCT+8)*(Z_OFFSET-Z0(HALF+1)))
     &      - Y_PWC_0
C  PWC ANGLE IN FDC COORDINATES
      X_ANGLE = Q(LFDCT+7) - DX_PWC_0
      Y_ANGLE = Q(LFDCT+8) - DY_PWC_0
C
      CALL HF1(1150,X_BEAM,1.)
      CALL HF1(1160,Y_BEAM,1.)
      CALL HF1(1170,X_ANGLE,1.)
      CALL HF1(1180,Y_ANGLE,1.)
C
      X_RESIDUAL = XPOS - X_PWC_0
      Y_RESIDUAL = YPOS - Y_PWC_0
      CALL HF1(1250,X_RESIDUAL ,1.)
      CALL HF1(1260,Y_RESIDUAL ,1.)
C
C Clean, well measured track:
C
      IF (  ABS(X_RESIDUAL) .GT. TRACK_CUT ) THEN
        GOTO 999
      ENDIF
      IF (  ABS(Y_RESIDUAL) .GT. TRACK_CUT ) THEN
        GOTO 999
      ENDIF
      CALL HF1(1100,4.,1.)
C
C HISTOGRAMS INVOLVING HITS ON SEGMENTS
C
      DO LAYER = 0, 2
        IF (LADDER(LAYER).NE.0) THEN
          IF (LAYER.LE.1) THEN
            NSEN = NBTSEN
          ELSE
            NSEN = NBPSEN
          END IF
C
          MODULE = 3*HALF + LAYER
          CALL GTFSEG(MODULE,LADDER(LAYER),CONT)
          XSECT_HIT = ICONT(1)/1000
          IADD = ICONT(2)
          NHIT = ICONT(3)
          CALL FCODER(IADD,HALF,UNIT,QDRT,SCTR,WIRE,UBIT,1)
          IF (FDEXST(HALF,UNIT,QDRT,SCTR)) THEN
C
            CALL FDC_CELL(
     &              HALF,UNIT,QDRT,SCTR,0,
     &              X_PWC,Y_PWC ,DX_PWC ,DY_PWC,
     &              X_ROT,Y_ROT ,DX_ROT ,DY_ROT)
C
            CALL FDC_SEGMENT(MODULE,LADDER(LAYER),
     &          X_SLOPE,X_DRIFT,Y_DL,DL_FOUND,IER)
C
            X_RESIDUAL = X_DRIFT - X_ROT
            ANGLE_RESIDUAL = X_SLOPE - DX_ROT
            DL_RESIDUAL = Y_DL - Y_ROT
C
            ID = 100000*HALF + 10000*UNIT + 1000*QDRT + 100*SCTR
            CALL HF1(62000000+ID,X_RESIDUAL,1.)
            CALL HF1(63000000+ID,ANGLE_RESIDUAL,1.)
            CALL HF1(70000000+ID,DX_ROT,1.)
            IF ( DL_FOUND ) THEN
C
C  refind besdif:
              CALL FGET_DL_INFO(MODULE,LADDER(LAYER),
     &          DT_SW0,DT_HV,DT_SIG,
     &          PH_SW0,PH_HV,PH_SIG,
     &          DELLEN,DELVEL, DL_FOUND)
              DL_LEN = DT_HV + DT_SIG - 2*DT_SW0
              DL_DIFF = DL_LEN - DELLEN / DELVEL 
C
              CALL HF1(64000000+ID,DL_RESIDUAL,1.)
              CALL HF2(65000000+ID,PH_SW0,DL_RESIDUAL,1.)
              CALL HF2(66000000+ID,Y_ROT,Y_DL,1.)
              CALL HF1(67000000+ID,DL_DIFF,1.)
              CALL HF2(68000000+ID,PH_SW0,DL_DIFF,1.)
            ENDIF
C
C GET POINTER TO RAW DATA HIT BANK, FTDA OR FPDA
C
            LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
            LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
            IF ( (LFXDA .EQ. 0 ) .OR. (LFXSC .EQ. 0 ) )THEN
              CALL ERRMSG('NOLFXDA','FDC_HISTS',
     &                ' NO hit bank for Segment hit','W')
              GOTO 999
            ENDIF
            DO HIT = 1, NHIT
              IF ( (XSECT_HIT.NE.0) .AND.
     &             (HIT .EQ. ABS(XSECT_HIT) ) )THEN
                SCTR = SCTR + SIGN(1,XSECT_HIT)
                LFXSC = GZFXSC(HALF,UNIT,QDRT,SCTR)
                LFXDA = GZFXDA(HALF,UNIT,QDRT,SCTR)
              ENDIF
C
              WIRE = INT(CONT(3+HIT)/2.)
              LR=INT(CONT(3+HIT))-2*WIRE
C
              ID = 100000*HALF + 10000*UNIT + 1000*QDRT 
     &          + 100*SCTR + WIRE
              IPTR = LFXSC + CONT(3+HIT+NSEN)        ! POINTER in FXSC
              DPTR = LFXDA + IQ(IPTR+10)             ! POINTER in FXDA 
C
              DRIFT_TIME = Q(DPTR+2)
              STAGGER=FSTAGR(HALF,UNIT,QDRT,SCTR,WIRE)
              X_DRIFT = Q(IPTR+2+LR)-STAGGER
C
              CALL FDC_CELL(
     &              HALF,UNIT,QDRT,SCTR,WIRE,
     &              X_PWC,Y_PWC ,DX_PWC ,DY_PWC,
     &              X_ROT,Y_ROT ,DX_ROT ,DY_ROT)

              X_RESIDUAL = X_DRIFT - X_ROT

              CALL HF2(60000000+ID,X_ROT,DRIFT_TIME ,1.)
              CALL HF2(61000000+ID,X_ROT,DRIFT_TIME ,1.)
              CALL HF1(71000000+ID,X_RESIDUAL,1.)
              CALL HF2(72000000+ID,X_ROT,X_DRIFT,1.)
            END DO
          ENDIF
        END IF
      END DO
  999 RETURN
      END
