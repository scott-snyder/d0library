      SUBROUTINE L2_CD_MATCH_PARAMETERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute RADII to each LAYER, WIRE
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-NOV-91   D. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:L2TRAK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C For error message routine
C
      CHARACTER*80 NOWERRMESS           ! Error message
      CHARACTER*16 NOWSMESS             ! short description
C
      INTEGER ERR, GZSL2H, GZL2DRFT, GZL2DTMH, GZL2DPDH
      INTEGER ILAYER, IWIRE, LSL2H
C----------------------------------------------------------------------
      INTEGER BIMAP(0:255)
      REAL DLTZR1, DLTZR2, PHITOL
      REAL WZ, X0, Y0, Z0, ZTOL
      LOGICAL L2TRAK_INIT

      COMMON /TRAK_TABLE/ BIMAP
      COMMON /L2TRKINIT/ L2TRAK_INIT
      COMMON /L2VERTEX/ X0, Y0, Z0
      COMMON /DELAYS/ DLTZR1, DLTZR2, WZ, ZTOL
      COMMON /NARROW_PHI/ PHItol
C----------------------------------------------------------------------
C
      L2TRAK_INIT = .TRUE.
C
      LSL2H = GZSL2H()
      IF (LSL2H .LE. 0) THEN
        NOWERRMESS = ' Cant find SL2H bank'
        NOWSMESS = 'L2_TRK_PARAMS'
        GOTO 900
      ENDIF
C
      L2DRFT = GZL2DRFT()
C      L2DRFT = LC( LC( LC(LSL2H-9) -4)  -3)
C                         L2CDC    DGEH  DRFT
      L2DTMH = GZL2DTMH()
C      L2DTMH = LC( LC(LSL2H-9) -3)
C                     L2CDC     DTMH
      L2DPDH = GZL2DPDH()
C      L2DPDH = LC( LC(LSL2H-9) -1)
C                     L2CDC     DPDH
C
      WIDSEC = C(L2DRFT + 9) * RADIAN
      ICEN =  C(L2DRFT + 12) * RADIAN   ! Cell center, Layer 0, Sector 0
      TANW = TAN(WIDSEC)
C
      DO ILAYER = 0,3
        DO IWIRE = 0,6
          RADIUS(IWIRE,ILAYER)=C(L2DRFT+11+ILAYER*2)+C(L2DRFT+19+IWIRE)
        ENDDO
      ENDDO
C
      CALL EZPICK('L2TRAK_RCP')
C
      CALL EZGET('ZROAD', WZ, ERR)
      CALL EZGET('ZTOL', ZTOL, ERR)
      CALL EZGET('PHITOL', PHITOL, ERR)
C
      CALL EZGET_l('COSMICS',COS_FLAG,ERR)
      CALL EZGET_l('DELAY',DEL_FLAG,ERR)
      CALL EZGET_l('FDC',FDC_FLAG,ERR)
      CALL EZGET_l('CDC',CDC_FLAG,ERR)
C
      CALL EZGET('TRGOFF',TRGOFF,ERR) ! Should return 0 for MC data
C                                     !       585.0 for COSMIC data
C                                     !      -230.0 for collider run Ia
      CALL EZGET_l('MONTE_CARLO',MC_FLAG,ERR)
C
      CALL EZGET('VTX_X0', X0, ERR)
      CALL EZGET('VTX_Y0', Y0, ERR)
C
      CALL EZGET('CDC_EDGE', CDC_EDGE, ERR)
      CALL EZGET('FDC_EDGE', FDC_EDGE, ERR)
      CALL EZGET_i('WIRE_MIN', WIRE_MIN, ERR)
      CALL EZGET_i('MIN_DEL', MIN_DEL, ERR)
      CALL EZGET_i('TOO_MANY', TOO_MANY, ERR)
      CALL EZGET('DRFTCUT', DRFTCUT, ERR)
      CALL EZGET('THR1', THR1, ERR)
      CALL EZGET('THR2', THR2, ERR)
      CALL EZGET_i('IPED', IPED, ERR)
C
      CALL EZRSET
C
      CALL EZPICK('L2CDHT_RCP')       ! Open the RCP from Chris' hitfind
C
      CALL EZGET_l('TRGFLG',TRGFLG,ERR) ! For Trigger time correction
      CALL EZGET('DLTZR1',DLTZR1,ERR) ! offset for delay line (left side)
      CALL EZGET('DLTZR2',DLTZR2,ERR) ! offset for delay line (right side)
C
      CALL EZGET_iarr('TABLE', BIMAP, ERR)
C
      CALL EZRSET
      GOTO 999                        ! parameter stage complete
C
  900 CONTINUE
      CALL ERRMSG(NOWSMESS,'L2_CD_MATCH_PARAMETERS',NOWERRMESS,'F')
      L2TRAK_INIT = .FALSE.
C
  999 RETURN
      END
