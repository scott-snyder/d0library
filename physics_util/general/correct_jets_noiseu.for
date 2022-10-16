      SUBROUTINE CORRECT_JETS_NOISEU(LJETS, INDET_ETA, INCONE_SIZE,
     &  UE_E, UE_ET, ZSP_E, ZSP_ET )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the noise and underlying event
c-                         contribution for this event.
C-
C-   Inputs  :  [I] LJETS - pointer to JETS bank. If 0, will use ETA
C-                          and CONE_SIZE to return the number.
C-              [R] DET_ETA   - detector eta of jet (not needed if LJETS .GT. 0)
C-              [R] CONE_SIZE - effective area of jet. (not needed if LJETS
C-                              .GT. 0 )
C-
C-   Outputs :  [R] UE_E     - underlying event contribution to E
C-              [R] UE_ET    - underlying event contribution to ET
C-              [R] ZSP_E    - noise contribution to E
C-              [R] ZSP_ET   - noise contribution to ET
C-   Controls:
C-
C-   Created   8-MAY-1995   Richard V. Astur
C-   Updated   7-FEB-1997   Bob Hirosky  -- update for cafix51
C-                                      use new noise+add_evt, and ULE params
C-                                      add jet area scaling feature
C-                          for reco12.20(1) - convert offset
C-                          information to jet area (only possible at
C-                          dst/sta level), for D0FIX - use jet area,
C-                          else assume all jets are round as before
C-
C-                                      add entry noiseu_set_mi_lum
C-                                          for mitool/lum input in ntuple
C-                                          processing
C-                                      allow rcp luminosity setting for MC
C-   Updated   7-AUG-1997   Bob Hirosky -- update for 630 GeV data, restrict
C-                                         input lums > 0.0
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      REAL DEFAULT_CONE_SIZE, INCONE_SIZE
      PARAMETER( DEFAULT_CONE_SIZE = .3 )
      EXTERNAL  PETA_TO_DETA
      REAL      PETA_TO_DETA
      REAL  UE_E, ZSP_ET, ZSP_E
      REAL UE_ET, DET_ETA, CONE_SIZE, INDET_ETA
      REAL ZV(3)
      INTEGER MC_RCP
      SAVE MC_RCP
      LOGICAL USE_630_MODEL, IS_630_DATA
      SAVE USE_630_MODEL
      REAL MC_LUMIN
      SAVE MC_LUMIN
      REAL UNDER_DENSITY_V_4_2, UNDER_ICD_DENSITY_V_4_2
      SAVE UNDER_DENSITY_V_4_2, UNDER_ICD_DENSITY_V_4_2
      LOGICAL USE_LUM_AND_MI, USE_RECO_AREA
      SAVE USE_LUM_AND_MI, USE_RECO_AREA
      LOGICAL DO_AREA_SCALE
      INTEGER JET_FLAG, VERSION
      REAL UE_ET_IC, RECO_AREA, RECO_AREAW
      INTEGER MITOOL,  MULTIPLE_INTERACTION_TOOL_RUN1
      INTEGER MULTIPLE_INTERACTION_TOOL_630, run_number, qcdrunno
      REAL ILUM, LUM(2), AGE(2)
      INTEGER LUMERR
      INTEGER LJETS, LCAPH, IER, IERTOT
      LOGICAL DONE, COR_DONE, ARA_DONE, UND_DONE, FIRST
      DATA FIRST /.TRUE./
C
      INTEGER inmitool, user_mitool
      SAVE user_mitool
      REAL inlum, user_lum
      SAVE user_lum
      LOGICAL use_user_mi_lum
      SAVE use_user_mi_lum
      DATA use_user_mi_lum /.false./
C----------------------------------------------------------------------
      IF (FIRST) THEN  ! read in old energy density constants
        IER = 0
        FIRST = .FALSE.
        CALL INRCP('QCD_JET_CORRECTION_RCP', IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','CORRECT_JETS_OFFSET',
     &        'Cant read in RCP file','F')
          IER = -1
          GOTO 999
        ENDIF
        CALL EZPICK('QCD_JET_CORRECTION_RCP')
        CALL EZERR( IER )
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('RCP error','CORRECT_JETS_NOISEU',
     &        'Cant find bank ','F')
          IER = -1
          GOTO 999
        ENDIF
        IERTOT = 0
C
C: What TYPE of RCP?
C
        CALL ezget_i('MC_RCP',mc_rcp, ier )
        iertot = iertot + abs(ier)
        IF ( iertot .NE. 0 ) THEN
          CALL errmsg('RCP error','CORRECT_JETS_NOISEU',
     &      'Read error:MC_RCP ','F')
        ENDIF
        MC_LUMIN = -1.0
        IF (MC_RCP.EQ.1) THEN
          CALL EZGET('MC_LUMIN',MC_LUMIN,IER)
          IERTOT = IERTOT + ABS(IER)
        ENDIF
        CALL EZGET_l('USE_LUM_AND_MI',USE_LUM_AND_MI,IER)
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET_l('USE_RECO_AREA',USE_RECO_AREA,IER)
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('UNDER_DENSITY_V_4_2',UNDER_DENSITY_V_4_2, IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET('UNDER_ICD_DENSITY_V_4_2',UNDER_ICD_DENSITY_V_4_2,
     &    IER )
        IERTOT = IERTOT + ABS(IER)
        CALL EZGET_l('USE_630_MODEL',USE_630_MODEL,IER)
        IERTOT = IERTOT + ABS(IER)
        CALL EZRSET
        IF ( IERTOT .NE. 0 ) THEN
          CALL ERRMSG('RCP error','CORRECT_JETS_OFFSET',
     &      'Read error:abort ','F')
          IER = -1
          GOTO 999
        ENDIF
      ENDIF !(first)
C
      ZSP_E  = 0.0
      ZSP_ET = 0.0
      DO_AREA_SCALE = .FALSE.
C
      CONE_SIZE = INCONE_SIZE
      DET_ETA   = INDET_ETA
      IF ( CONE_SIZE .LE. 0. ) CONE_SIZE  = DEFAULT_CONE_SIZE
C
C: If LJETS .GT. 0, we can check to see if the noise/ue/area was calculated
C: online. Also, we can get the physics ETA and cone size from this.
C
      IF ( LJETS .GT. 0 ) THEN
C
C: Get vertex to find detector eta
C
        DET_ETA       = Q( LJETS + 9 )            ! physics eta
        CALL VERTEX_INFO(1, IER , ZV, DONE )
        IF ( .NOT. DONE ) ZV(1) = 0.0
        DET_ETA = PETA_TO_DETA( DET_ETA, ZV(1) )
C
C: Find conesize / algorithm
C
        LCAPH     = LQ( LJETS + 1 )
        CONE_SIZE = DEFAULT_CONE_SIZE
        IF ( IQ(LCAPH+4) .EQ. 2 ) THEN        ! cone jet, might have area info
          CONE_SIZE = ABS(Q( LCAPH+6))        ! for -0.7, D0 cone
C
C: Is jet isolated?
C
          JET_FLAG = IQ( LJETS + 15 )
C
C: Get Version of jet's bank
C
          VERSION = IQ( LJETS + 1 )
C
          IF ((USE_RECO_AREA).AND.(JET_FLAG.NE.0)
     &                                       .AND.(VERSION.GE.7))  THEN
C
C: Are noise/ue words already filled?  Get jet area.
C
            RECO_AREA  = 0.0
            RECO_AREAW = 0.0
            CALL JETS_BANK_CORRECTED(LJETS, 'COR', COR_DONE , IER )
            COR_DONE = ( COR_DONE .AND. ( IER .EQ. 0 ) )
            CALL JETS_BANK_CORRECTED(LJETS, 'ARA', ARA_DONE , IER )
            ARA_DONE = ( ARA_DONE .AND. ( IER .EQ. 0 ) )
            CALL JETS_BANK_CORRECTED(LJETS, 'UND', UND_DONE , IER )
            UND_DONE = ( UND_DONE .AND. ( IER .EQ. 0 ) )
C
            IF ((.NOT.COR_DONE) .AND. (UND_DONE)) THEN ! RECO 12.20(1)
              UE_ET = Q( LJETS + 31)                 ! convert ENERGY to area
              UE_ET_IC  = Q( LJETS + 38)
              RECO_AREA = (UE_ET-UE_ET_IC)/UNDER_DENSITY_V_4_2
              IF (RECO_AREA.LE.0.0) THEN
                CALL errmsg('RECO area error','CORRECT_JETS_NOISEU',
     &              'Jet Area .LE. 0.0 ignored','W')
              ELSE
                DO_AREA_SCALE = .TRUE.
              ENDIF
            ELSEIF((.NOT.COR_DONE) .AND. (ARA_DONE))  THEN ! D0FIX Data
              RECO_AREA = Q( LJETS + 31)
              RECO_AREAW = Q( LJETS + 30)
              IF (RECO_AREA.LE.0.0) THEN
                CALL errmsg('D0FIX area error','CORRECT_JETS_NOISEU',
     &              'Jet Area .LE. 0.0 ignored','W')
              ELSE
                DO_AREA_SCALE = .TRUE.
              ENDIF
            ENDIF
          ENDIF       !((USE_RECO_AREA).AND.(JET_FLAG.NE.0).AND.(VERSION.GE.7))
        ENDIF         !( IQ(LCAPH+4) .EQ. 2 )
      ENDIF           !(LJETS.GT.0)
C
C ****  Get Lum and MI TOOL values here or use user_override
C
      IF (.NOT. use_user_mi_lum) THEN
        MITOOL = 0
        IF (USE_LUM_AND_MI) THEN       ! assume mitool = 1 for not enough info
          run_number = qcdrunno()
          IS_630_data = (run_number.GE.94874).AND.(run_number.LE.95389)
          IF ( IS_630_DATA ) THEN
            MITOOL = MAX( 1 , MULTIPLE_INTERACTION_TOOL_630() )
            IF (.NOT. USE_630_MODEL) CALL errmsg('Check Noise model',
     &        'CORRECT_JETS_NOISEU','inconsistent with run number','W')
          ELSE
            MITOOL = MAX( 1 , MULTIPLE_INTERACTION_TOOL_RUN1() )
            IF (USE_630_MODEL) CALL errmsg('Check Noise model',
     &        'CORRECT_JETS_NOISEU','inconsistent with run number','W')
          ENDIF
        ENDIF
        LUMERR = -1
        IF (MC_RCP.EQ.0) CALL GETLUM( 0, LUM, AGE, LUMERR )
C
C ****  for now set lum = 0.0 for MC data!
C
        IF ( lumerr .NE. 0 ) THEN
          IF (MC_RCP.EQ.0) CALL errmsg('getlum error',
     &      'CORRECT_JETS_NOISEU',
     &      'Assume low luminosity','W')
          lum(1)  = 0.
          lum(2)  = 0.
          age(2)  = 1.0
          age(1)  = 0.0
        ENDIF
        ILUM =   AGE(1)*(LUM(2)-LUM(1))/(AGE(2)+AGE(1)) + LUM(1)
      ELSE
        MITOOL = 0
        IF (USE_LUM_AND_MI) MITOOL = user_mitool
        ILUM = user_lum
      ENDIF
      IF (MC_RCP.EQ.1) ILUM = MC_LUMIN
C
C: Call new routine UE = und evt + noise energy
C
      CALL JET_UNDZSP_FACTOR( CONE_SIZE, ILUM, MITOOL, DET_ETA,
     &    DO_AREA_SCALE, RECO_AREA, UE_E, UE_ET, ZSP_E, ZSP_ET)

  999 RETURN
C
C ****  entry to enter mitool value and luminosity for ntuple processing
C
      ENTRY noiseu_set_mi_lum( inmitool, inlum )
      user_mitool = MAX(1,inmitool)
      user_lum = max(inlum,0.0)
      use_user_mi_lum = .true.
      RETURN

      END
