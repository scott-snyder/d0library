C DEC/CMS REPLACEMENT HISTORY, Element QCD_FILTER_BIT_INIT.FOR
C *7    30-MAR-1995 17:42:12 MEENA "Richard V. Astur: Bug fix: array initialization overwrite"
C *6    19-OCT-1994 19:51:45 FRAME "Richard V. Astur: Add new QCD filter names"
C *5    15-AUG-1994 00:20:35 MEENA "Richard V. Astur: Add new QCD filter names"
C *4    19-JUL-1994 21:45:03 MEENA "Richard V. Astur: Add new QCD filter names"
C *3     2-APR-1994 14:33:56 MEENA "Richard V. Astur: Addition of JET_30 to qcd triggers"
C *2    25-FEB-1994 18:57:07 MEENA "Richard V. Astur: QCD portion of udst"
C *1    24-FEB-1994 18:21:18 MEENA "Richard V. Astur: QCD portion of udst"
C DEC/CMS REPLACEMENT HISTORY, Element QCD_FILTER_BIT_INIT.FOR
      SUBROUTINE QCD_FILTER_BIT_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get list of filter bits and names for this run,
C-                         and set up arrays to map between these bits and
C-                         the official set of QCD bits. Do once per run.
C-
C-   Controls: Get run number from header.
C-
C-   Created   5-DEC-1992   Richard V. Astur
C-   Updated   17-SEP-1996  V.Daniel Elvira - Add JET_GAP_DPU2
C-   Updated   17-SEP-1996  Terry C. Heuring - Add LNR triggers 
C-            
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IER, RUNNO, OLD_RUNNO, OLD_EVENT
      SAVE OLD_RUNNO, OLD_EVENT
      INCLUDE 'D0$INC:QCD_BIT_NAMES.INC'
      INCLUDE 'D0$INC:QCD_BIT.INC'
C: D0 Names
      INTEGER NTRIG, TRIGBIT(32), NFILT, FILTBIT(128),TRIG_FILT(128)
      CHARACTER*64 TRIGNAME(32), FILTNAME(128)
      CHARACTER*64 NAME, NAME1
C:
      INTEGER I,J, L, L1, LL, LL1, IQCD_BIT
      LOGICAL FIRST, RUN1B, LNR
      SAVE IER, FIRST, RUN1B
      DATA OLD_RUNNO/ -30 /
      DATA FIRST /.TRUE./

C----------------------------------------------------------------------
C
C: One time initialization of arrays
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.

        RUN1B = .TRUE.

        IF ( RUNNO() .LE. 69000 ) RUN1B = .FALSE.

        LNR = .FALSE.

        IF(( RUNNO() .GE. 94874 ) .AND. ( RUNNO() .LE. 95389))  
     &    LNR = .TRUE.

        IF ( .NOT. RUN1B) THEN

        IQCD_BIT = 0
        QCD_BIT_NAME( IQCD_BIT ) = 'OTHER'
        QCD_L2_NJETS( IQCD_BIT ) = 0
        QCD_L2_THRESH( IQCD_BIT )= 0.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MIN'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 20.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 2
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_LOW'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 30.0
        QCD_L2_END(IQCD_BIT )    = .TRUE.

        IQCD_BIT = 3
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_MED'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 50.0
        QCD_L2_END(IQCD_BIT )    = .TRUE.

        IQCD_BIT = 4
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_LOW'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 30.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 5
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MEDIUM'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 50.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 6
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_HIGH'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 85.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 7
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_3_HIGH'
        QCD_L2_NJETS( IQCD_BIT ) = 3
        QCD_L2_THRESH( IQCD_BIT )= 30.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 8
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MAX'
        QCD_L2_NJETS( IQCD_BIT ) = 1
        QCD_L2_THRESH( IQCD_BIT )= 115.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 9
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MULTI'
        QCD_L2_NJETS( IQCD_BIT ) = 5
        QCD_L2_THRESH( IQCD_BIT )= 10.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 10
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MULTI_X'
        QCD_L2_NJETS( IQCD_BIT ) = 5
        QCD_L2_THRESH( IQCD_BIT )= 15.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        IQCD_BIT = 11
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MED'
        QCD_L2_NJETS( IQCD_BIT ) = 2
        QCD_L2_THRESH( IQCD_BIT )= 25.0
        QCD_L2_END(IQCD_BIT )    = .TRUE.

        IQCD_BIT = 12
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_4_MED'
        QCD_L2_NJETS( IQCD_BIT ) = 4
        QCD_L2_THRESH( IQCD_BIT )= 20.0
        QCD_L2_END(IQCD_BIT )    = .FALSE.

        N_QCD_BITS = 13
        ELSE

C
C: RUN 1B
C
        IQCD_BIT                 = 0
        QCD_BIT_NAME( IQCD_BIT ) = 'MU_1_EM_JET'  ! 0
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MIN'      ! 1
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_FAR_END'  ! 2
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_50'       ! 3
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_85'       ! 4
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MAX'      ! 5
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_LOW'  ! 6
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_HIGH' ! 7
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'ZERO_BIAS_QNT'  ! 8
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MIN_BIAS_QNT' ! 9
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MINBIAS_MI_QNT'  ! 10
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_10_QNT'   ! 11
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_INV_QNT'  ! 12
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_30_QNT'   ! 13
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'ALWAYS_SET1'  ! 14
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_50_QNT'   ! 15
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_50_NMI_QNT' ! 16
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_15_QNT'   ! 17
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_25_QNT'   ! 18
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_FAR_END_QNT'  ! 19
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_85_QNT'       ! 20
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_115_QNT'      ! 21
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_ASM_QGP'  ! 22  available in the future
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MIN_QGP'  ! 23  available in the future
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MNO_QGP'  ! 24  available in the future
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_15_NOL0'      ! 25
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_30_NOL0'      ! 26
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MAXA'         ! 27
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_SDH'     ! 28 
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MIN_BIAS_QGP'     ! 29
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MINBIAS_MI_QGP'   ! 30
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_LHE'      ! 31
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_LME'      ! 32
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_HHE'      ! 33
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_HME'      ! 34
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MHE'      ! 35
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_DPU'      ! 36  
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_DPU2'     ! 37  
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_NMI'      ! 38
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_30'           ! 39
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_20_BADF'           ! 40
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_20_BADS'           ! 41
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12'           ! 42
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET2_12'           ! 43
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_12'           ! 44
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET2_END_12'           ! 45
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'ALWAYS_SET2'           ! 46
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET2_END_12B'           ! 47
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_140'           ! 48
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_160'           ! 49
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_20_NOL0'           ! 50
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_SAME'           ! 51
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_FAR_SUP'           ! 52
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_FAR_MAX'           ! 53
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_20_BADF_QNT'           ! 54
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_20_BADS_QNT'           ! 55
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12_QNT'           ! 56
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET2_12_QNT'           ! 57
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_END_12_QNT'           ! 58
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET2_END_12_QNT'           ! 59
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_SD'           ! 60
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_POM'           ! 61
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_VLO'           ! 62
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_VHI'           ! 63
        IQCD_BIT                 = IQCD_BIT + 1
        N_QCD_BITS = 64
        ENDIF
C
C: LNR: 630 GeV running
C
        IF( LNR ) THEN
        IQCD_BIT                 = 0
        QCD_BIT_NAME( IQCD_BIT ) = 'EM2_EIS2_HI-LNR'      ! 1
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MINBIAS_D0_DET-LNR'   ! 2
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_10-LNR'       ! 3
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_10H-LNR'      ! 4
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_10MI-LNR'     ! 5
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_10HMI-LNR'    ! 6
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MNS-LNR'      ! 7
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_PLS-LNR'      ! 8
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_MID-LNR'      ! 9
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_15_NOL0-LNR'      ! 10
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'ALWAYS_SET1'          ! 11
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_SD-LNR'       ! 12
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_GAP_SD12-LNR'     ! 13
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'MINBIAS_CDC_CAL-LNR'  ! 14
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_30-LNR'           ! 15
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_30_QMP-LNR'       ! 16
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_50-LNR'           ! 17
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12-LNR'           ! 18
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12_GM-LNR'        ! 19
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12_QMP-LNR'       ! 20
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_12B-LNR'          ! 21
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MID_12B-LNR'      ! 22
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_MNS_12B-LNR'      ! 23
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_PLS_12B-LNR'      ! 24
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_10-LNR'           ! 25
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'JET_10_QMP-LNR'       ! 26
        IQCD_BIT                 = IQCD_BIT + 1
        QCD_BIT_NAME( IQCD_BIT ) = 'ZERO_BIAS-LNR'        ! 27
        IQCD_BIT                 = IQCD_BIT + 1
        N_QCD_BITS = 28
        ENDIF
      ENDIF
C: Init arrays at start of new run
      IF ( OLD_RUNNO .NE. RUNNO() ) THEN
        OLD_RUNNO = RUNNO()
c        CALL DBO_GET_TRIGGERS
c     &    (OLD_RUNNO,NTRIG,TRIGBIT,TRIGNAME,NFILT,FILTBIT,FILTNAME,IER)
C: Init arrays
        DO I = 0,127
          D0_TO_QCD(I) = -1
        ENDDO
        DO I = 0, QCD_BIT_MAX-1
          QCD_TO_D0(I)  = -1
        ENDDO
C: Set old event number
        OLD_EVENT = -123
      ENDIF

C: Skip if we have done this once already this event
      IF ( OLD_EVENT .EQ. IQ( LHEAD + 9 ) ) RETURN
      OLD_EVENT = IQ( LHEAD + 9 )

      CALL GTTSUM( NTRIG, TRIGBIT, TRIGNAME, NFILT, FILTBIT, FILTNAME
     &    )

      DO I = 1, NFILT
        IF ( D0_TO_QCD( FILTBIT(I) ) .EQ. -1) THEN
          D0_TO_QCD( FILTBIT(I) ) = -2               ! We looked at it
          CALL QCD_NAME_TO_BIT( FILTNAME(I), IQCD_BIT )
          IF ( IQCD_BIT .GE. 0 ) QCD_TO_D0( IQCD_BIT ) = FILTBIT(I)
          D0_TO_QCD( FILTBIT(I) ) = IQCD_BIT
        ENDIF
      ENDDO
  999 RETURN
      END
