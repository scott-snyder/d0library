      SUBROUTINE JET_UNDZSP_FACTOR( CONE_USED, LUM, MITOOL,
     &  ETA, DO_AREA_SCALE, RECO_AREA, PUE_E, PUE_ET,
     &  ZSP_E, ZSP_ET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the amount (in GeV) of underlying event
C-      and zerosuppression/noise + offset from additional interactions
C-      for jets of cone_size=CONE_USED both in energy and transverse
C-      energy and as a function of detector eta, luminosity and mitool.
C-
C-   Inputs  : CONE_USED  [R]  : Conesize of jet
C-             LUM        [R]  : luminosity
C-             MITOOL     [I]  : Mitool run1 value
C-             ETA        [R]  : Detector eta of jet
C-             DO_AREA_SCALE [L]: Do area scaling for s/m jets
C-             RECO_AREA   [R] : True area of jet from RECO
C-   Outputs : PUE_E   [R]  : Phys Underlying event energy (minbias) (GeV)
C-             PUE_ET  [R]  : Phys Underlying event transverse energy (GeV)
C-             ZSP_E      [R]  : Noise/ZSP + Added interactions energy (GeV)
C-             ZSP_ET     [R]  : Noise/ZSP + Added interactions ET (GeV)
C-
C- ENTRY JET_UNDZSP_ERROR( PUE_E_ERR, PUE_ET_ERR,
C-                           ZSP_E_ERR, ZSP_ET_ERR
C-                           ZSP_E_ERR_SYS, ZSP_ET_ERR_SYS)
C-      To return the errors on the previous call to JET_UNDZSP_FACTOR
C-
C-   Created  11-AUG-1996   Bob Hirosky -- new cafix 51 version
C-                          based on JET_UNDZSP_FACTOR
C-   Updated  26-JUN-1997   Bob Hirosky -- bug fix on ZSP_E systematic error
C-   Updated  17-JUL-1997   Bob Hirosky -- add 630 Noise/ZSP parametrization
C-   Updated  28-JUL-1997   Bob Hirosky -- fix lum_offset bug, fix ZSP dens
C-                                      -- to constant value past eta=3.9
C-   Updated   7-AUG-1997   Bob Hirosky -- add USE_630_MODEL switch
C-   Updated   4-SEP-1997   Bob Hirosky -- fix EZGET for ZSP_LUM array
C-   Updated  22-MAY-1998   Bob Hirosky -- add new 630 combined UND/ZSP error
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL SMALL
      PARAMETER( SMALL = .0001)
      REAL CONE_USED, ETA
      LOGICAL DO_AREA_SCALE
      REAL LUM,RECO_AREA
      INTEGER MITOOL
      REAL E1, E2, E3, E4, E5, E6
      REAL ETA_SLICE, THE_SLICE, DPHI, SINTHETA
      REAL FACTOR
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER I,J
      INTEGER LUMID,MITID
C
      INTEGER  ICONE
      INTEGER  NPARS,NLUM,NMI,NCONE
      PARAMETER (NPARS=8,NLUM=5,NMI=3,NCONE=4)
      INTEGER PAR_OFFSET, LUM_OFFSET, CONE_OFFSET
      INTEGER CONE_LUM_OFFSET
      LOGICAL USE_LUM_AND_MI
      SAVE USE_LUM_AND_MI
      INTEGER MC_RCP, LUMBINS
      SAVE MC_RCP, LUMBINS
      REAL MC_LUMIN
      SAVE MC_LUMIN
      REAL LUM_LIST(NLUM)
      SAVE LUM_LIST
      LOGICAL USE_MC_LUM, USE_630_MODEL
      SAVE USE_MC_LUM, USE_630_MODEL
      REAL ULUM
      REAL ETA_LIMIT             ! last point in ZSP dens. fit
      PARAMETER (ETA_LIMIT=3.9)  ! hold density constant past this eta
C
C ****  parameters for noise+additional event offset
C
      REAL ZSP_LUM(NPARS*NLUM*NCONE)
      REAL ZSP_LUM_MI1(NPARS*NLUM*NCONE)
      REAL ZSP_LUM_MI3(NPARS*NLUM*NCONE)
      REAL ZSP_DENS1(NPARS*NLUM*NCONE)
      REAL ZSP_DENS2(NPARS*NLUM*NCONE)
      SAVE ZSP_DENS1, ZSP_DENS2
C
      REAL ZSP_DENS_ERROR,ZSP_DENS_SYS,PUE_DENS_ERROR
      SAVE ZSP_DENS_ERROR,ZSP_DENS_SYS,PUE_DENS_ERROR
C
      REAL AL(NPARS), AH(NPARS)
C
C ****  parameters for Physics underlying event offset
C
      INTEGER MAX_ETA_BINS
      PARAMETER (MAX_ETA_BINS=20)
      REAL PUE_ET_DENSITY(MAX_ETA_BINS),ZSPUE_DENS_ERROR(MAX_ETA_BINS)
      SAVE PUE_ET_DENSITY,ZSPUE_DENS_ERROR
C
      REAL X, SLOPE, DELTA_LUM
      REAL PUE_E, PUE_ET
      INTEGER IETA, PUE_BIN
      REAL PUE_ERR, PUE_ET_ERR
      SAVE PUE_ERR, PUE_ET_ERR
      REAL ZSP_ET, ZSP_E, ZSP_ET_H, ZSP_E_H
      REAL ZSP_ET_ERR, ZSP_ERR
      SAVE ZSP_ET_ERR, ZSP_ERR
      REAL ZSP_ET_ERR_SYS, ZSP_ERR_SYS
      SAVE ZSP_ET_ERR_SYS, ZSP_ERR_SYS
      REAL AREA, OFFSET_DENSITY, OFFSET_DENSITY_L, OFFSET_DENSITY_H
C---------------------------------------------------------------------
C
C: Read values for RCP
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP('QCD_JET_CORRECTION_RCP', IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_OFFSET_FACTOR',
     &        'Cant read in RCP file','F')
          IER = -1
          GOTO 900
        ENDIF
        CALL EZPICK('QCD_JET_CORRECTION_RCP')
        CALL EZERR( IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_OFFSET_FACTOR',
     &        'Cant find bank ','F')
          IER = -1
          GOTO 900
        ENDIF
        CALL ezget_i('MC_RCP',mc_rcp, ier )
        IF ( ier .NE. 0 ) THEN
          CALL errmsg('RCP error','JET_UNDZSP_FACTOR',
     &      'Read error:MC_RCP ','F')
        ENDIF
        IF (mc_rcp.EQ.1) CALL ezget('MC_LUMIN',mc_lumin, ier )

C
        IF (ier.EQ.0) CALL EZGET_l('USE_LUM_AND_MI',USE_LUM_AND_MI,IER)
        IF (ier.EQ.0) CALL ezgeta_i('LUM_LIST',0,0,0,LUMBINS,IER)
        IF (ier.EQ.0) CALL ezgeta('LUM_LIST',1,LUMBINS,1,LUM_LIST,IER)
C
        IF (ier.EQ.0) CALL EZGET_l('USE_630_MODEL',USE_630_MODEL,IER)
C
        USE_MC_LUM = .FALSE.
        IF ((mc_rcp.EQ.1).AND.(mc_lumin.LT.-0.5)) then
          CALL VZERO(ZSP_DENS1,NPARS*NLUM*NCONE)  ! zero noise,etc for mc data
          CALL VZERO(ZSP_DENS2,NPARS*NLUM*NCONE)  ! w/o overlap
          CALL errmsg('QUIET MC SAMPLE','JET_UNDZSP_FACTOR',
     &      'NO OVERLAP TO SUBTRACT','W')
        ELSE
          IF (mc_rcp.EQ.1) THEN
            CALL errmsg('NOISEY MC SAMPLE',
     &        'JET_UNDZSP_FACTOR','SUBTRACTING OVERLAP FROM MC','W')
            USE_MC_LUM = .TRUE.
          ENDIF
          IF (.NOT.USE_LUM_AND_MI) THEN
            IF (ier.EQ.0) CALL EZGETA('ZSP_LUM',1,NPARS*LUMBINS*NCONE,
     &        1,ZSP_DENS1, IER )
          ELSE
            IF (ier.EQ.0) CALL EZGETA('ZSP_LUM_MI1',1,
     &        NPARS*LUMBINS*NCONE,1,ZSP_DENS1, IER )
            IF (ier.EQ.0) CALL EZGETA('ZSP_LUM_MI3',1,
     &        NPARS*LUMBINS*NCONE,1,ZSP_DENS2, IER )
          ENDIF
        ENDIF
C
        IF (ier.EQ.0) CALL EZGET('ZSP_DENS_ERROR',ZSP_DENS_ERROR,IER)
        IF (ier.EQ.0) CALL EZGET('ZSP_DENS_SYS',ZSP_DENS_SYS,IER)
        IF (ier.EQ.0) CALL EZGET_rarr('PUE_ET_DENSITY',PUE_ET_DENSITY,
     &       IER )
        IF (ier.EQ.0) CALL EZGET('PUE_DENS_ERROR',PUE_DENS_ERROR,IER)
        IF ((ier.EQ.0).AND.(USE_630_MODEL)) THEN
          CALL EZGET_rarr('ZSPUE_DENS_ERROR',ZSPUE_DENS_ERROR, IER )
        ENDIF
C
        CALL EZRSET
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','JET_OFFSET_FACTOR',
     &        'Read error:abort ','F')
          IER = -1
          GOTO 900
        ENDIF
C
      ENDIF !(FIRST)
C
C: Initialize for offset calculation
C
      ZSP_E   = 0.0
      ZSP_ET  = 0.0
      ZSP_E_H   = 0.0
      ZSP_ET_H  = 0.0
      ZSP_ET_ERR = 0.0
      ZSP_ERR = 0.0
      ZSP_ET_ERR_SYS = 0.0
      ZSP_ERR_SYS = 0.0
      PUE_E   = 0.0
      PUE_ET  = 0.0
      PUE_ET_ERR = 0.0
      PUE_ERR = 0.0
      AREA    = 0.0
C
C ****  choose parametrization of zsp/add event ! start w/ lum
C
      IF (USE_MC_LUM) THEN
        ULUM = MC_LUMIN
      ELSE
        ULUM = LUM
      ENDIF
C
      LUM_OFFSET = 1
      DO i = 2, lumbins-1
        IF (ULUM.GE.LUM_LIST(I)) THEN
          LUM_OFFSET = I
        ENDIF
      ENDDO
C
C ****  choose cone
C
      ICONE = 4                         ! 1.0 cone
      IF (CONE_USED.LT.0.9) ICONE = 3   ! 0.7 cone
      IF (CONE_USED.LT.0.6) ICONE = 2   ! 0.5 cone
      IF (CONE_USED.LT.0.4) ICONE = 1   ! 0.3 cone
      CONE_OFFSET = NPARS*LUMBINS*(ICONE-1)
C
C ****  choose MITOOL parametrization
C
      CONE_LUM_OFFSET = CONE_OFFSET + (LUM_OFFSET-1)
      IF (.NOT. USE_LUM_AND_MI .OR. MITOOL.LT.3) THEN
        DO I=1,NPARS
          AL(I) = ZSP_DENS1( CONE_LUM_OFFSET + (I-1)*LUMBINS + 1 )
          AH(I) = ZSP_DENS1( CONE_LUM_OFFSET + 1 + (I-1)*LUMBINS + 1 )
        ENDDO
      ELSE
        DO I=1,NPARS
          AL(I) = ZSP_DENS2( CONE_LUM_OFFSET + (I-1)*LUMBINS + 1 )
          AH(I) = ZSP_DENS2( CONE_LUM_OFFSET + 1 + (I-1)*LUMBINS + 1 )
        ENDDO
      ENDIF
C
C: Use .099 instead of .1 to avoid round off problems that would
C: stop the loop prematurely
      DO ETA_SLICE = ETA - CONE_USED +.05, ETA + CONE_USED -.05, .099
        SINTHETA  = 1./COSH(ETA_SLICE)
        DPHI     = 2*SQRT( CONE_USED**2 - ABS(ETA_SLICE - ETA)**2 )
        FACTOR   = DPHI*.1
        AREA     = AREA + FACTOR
C
        X = min(ETA_LIMIT,ABS(ETA_SLICE))
C
C ****  offset parametrization function
C
        IF (USE_630_MODEL) THEN
          OFFSET_DENSITY_L = AL(1)/(1+AL(2)*(10*x)**AL(3)) +
     &      AL(4)/(1+AL(5)*(3*x)**AL(6))
          OFFSET_DENSITY_H = AH(1)/(1+AH(2)*(10*x)**AH(3)) +
     &      AH(4)/(1+AH(5)*(3*x)**AH(6))
        ELSE
          OFFSET_DENSITY_L = AL(1) + AL(2)*X
     &      + AL(3)*SIN(AL(4)*X-AL(5)) + AL(6)*SIN(AL(7)*X-AL(8))
          OFFSET_DENSITY_H = AH(1) + AH(2)*X
     &      + AH(3)*SIN(AH(4)*X-AH(5)) + AH(6)*SIN(AH(7)*X-AH(8))
        ENDIF
C
C ****  Interpolate zsp/AddEvt to input luminosity
C
        SLOPE = ( OFFSET_DENSITY_H-OFFSET_DENSITY_L )
     &    / ( LUM_LIST(LUM_OFFSET+1) - LUM_LIST(LUM_OFFSET) )
        DELTA_LUM = LUM - LUM_LIST(LUM_OFFSET)
        OFFSET_DENSITY = SLOPE*DELTA_LUM + OFFSET_DENSITY_L
c
        ZSP_E = ZSP_E + FACTOR*OFFSET_DENSITY/SINTHETA
        ZSP_ET= ZSP_ET+ FACTOR*OFFSET_DENSITY
c
C- systematic error from occupancy correction
c
        ZSP_E_H = ZSP_E_H + FACTOR*(OFFSET_DENSITY+ZSP_DENS_SYS)
     &    /SINTHETA
        ZSP_ET_H= ZSP_ET_H + FACTOR*(OFFSET_DENSITY+ZSP_DENS_SYS)
C
        CALL CALETA_INV_QCD(X,IETA,IER)
        OFFSET_DENSITY = 0.0
        PUE_BIN = (ABS(IETA)+1)/2
        IF (PUE_BIN.GT.16) PUE_BIN = MIN(20,20-(36-ieta))
        IF (IER.EQ.0) OFFSET_DENSITY = PUE_ET_DENSITY(ABS(PUE_BIN))
        PUE_E = PUE_E + FACTOR*OFFSET_DENSITY/SINTHETA
        PUE_ET= PUE_ET+ FACTOR*OFFSET_DENSITY
        IF (USE_630_MODEL) THEN
          IF (IER.EQ.0) OFFSET_DENSITY = ZSPUE_DENS_ERROR(ABS(PUE_BIN))
          ZSP_ERR = ZSP_ERR + FACTOR*OFFSET_DENSITY/SINTHETA
          ZSP_ET_ERR = ZSP_ET_ERR + FACTOR*OFFSET_DENSITY
        ENDIF
      ENDDO
C
C: Do AREA correction for s/m Jets
C
      IF (DO_AREA_SCALE) THEN
        PUE_ET = PUE_ET * RECO_AREA / AREA
        PUE_E  = PUE_E  * RECO_AREA / AREA
        ZSP_ET = ZSP_ET * RECO_AREA / AREA
        ZSP_E  = ZSP_E  * RECO_AREA / AREA
        ZSP_ET_ERR = ZSP_ET_ERR * RECO_AREA / AREA
        ZSP_ERR = ZSP_ERR * RECO_AREA / AREA
        ZSP_ET_H = ZSP_ET_H * RECO_AREA / AREA
        ZSP_E_H  = ZSP_E_H  * RECO_AREA / AREA
      ENDIF
C
C: Determine errors for entry point
C
      PUE_ET_ERR  = PUE_ET * PUE_DENS_ERROR
      PUE_ERR     = PUE_E  * PUE_DENS_ERROR
      IF (.NOT. USE_630_MODEL) THEN
        ZSP_ET_ERR  = ZSP_ET * ZSP_DENS_ERROR
        ZSP_ERR     = ZSP_E  * ZSP_DENS_ERROR
      ENDIF
      ZSP_ET_ERR_SYS = ZSP_ET_H - ZSP_ET
      ZSP_ERR_SYS = ZSP_E_H - ZSP_E

C
      IF ((mc_rcp.EQ.1).AND.(mc_lumin.LT.-0.5)) then ! no noise/pileup in MC
        ZSP_ET = 0.0
        ZSP_ET_ERR = 0.0
        ZSP_E = 0.0
        ZSP_ERR = 0.0
        ZSP_ET_ERR_SYS = 0.0
        ZSP_ERR_SYS = 0.0
      ENDIF

      GOTO 999
C
C: RCP error
C

  900 PUE_ET = -999.
      PUE_E  = -999.

  999 RETURN

C*********************************************************
C ENTRY JET_OFFSET_ERROR to report errors of the above
C WARNING: This entry is not valid unless preceded by a call to
C          JET_OFFSET_FACTOR!
C*********************************************************

      ENTRY JET_UNDZSP_ERROR(e1, e2, e3, e4, e5, e6)
      E1 = PUE_ERR
      E2 = PUE_ET_ERR
      E3 = ZSP_ERR
      E4 = ZSP_ET_ERR
      E5 = ZSP_ERR_SYS
      E6 = ZSP_ET_ERR_SYS
      RETURN
C
      END
