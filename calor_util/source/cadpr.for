      SUBROUTINE CADPR(LDET,LCRATE,LADC,LBLS,LROTOW,LDEPTH
     &,IBOX,IPRBRD,IROTOW,IDEPTH,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert from ADC scheme to PREAMP scheme
C-
C-   Inputs  : LDET   detector config as defined in CAL_PULSE_LIST.PARAMS
C-             LCRATE adc crate
C-             LADC   adc board
C-             LBLS   bls board
C-             LROTOW read out tower on bls board
C-             LDEPTH depth in bls read out tower
C-   Outputs : IBOX   preamp box - 0 to 13
C-             IPRBRD preamp board - 0 to 95
C-             IROTOW read out tower on preamp board
C-             IDEPTH depth in preamp board read out tower
C-   Controls: ICOND  0 = valid   1 = is attached to no preamp
C-              2 = ICD thus not from a preamp  20 = bad inputs
C-
C-   Created   7-DEC-1988   James Kourlas  NYUHEP::KOURLAS
C-   Modified  7-DEC-1989   James Kourlas for new ADC crate numbers
C-   Modified  16-JAN-1990  James Kourlas for test beam and quadrant test
C-   Modified  14-FEB-1990  James Kourlas for new CC north sector ordering
C-   Modified  14-MAY-1990  James Kourlas for ICOND=1 and ICOND=2
C-   Updated   14-FEB-1992  Joan Guida    for ICD
C-   Modified  15-OCT-1992  James Kourlas for ICOND=1 correction
C-   Modified  21-NOV-1992  Joan Guida    for missing main-ring channels
C-
C-   See D0 Note 774 for documentation. A copy is in
C-                                   D0$DOCS:CALORIMETER_ADDRESSING.MEM
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_PULSE_LIST.PARAMS'
      INCLUDE 'D0$PARAMS:TB90L1_BLS_TO_PRE.DEF'
      INCLUDE 'D0$PARAMS:TB90L2_BLS_TO_PRE.DEF'
C                         passed variables:
      INTEGER LDET, LCRATE, LADC, LBLS, LROTOW, LDEPTH
      INTEGER IBOX, IPRBRD, IROTOW, IDEPTH
      INTEGER ICOND
C                         local variables:
      INTEGER LOGBLS    ! logical bls number 0-95
      INTEGER IMOD      ! LOGBLS mod 6 for CC  or  LOGBLS mod 12 for EC
      INTEGER ISECT     ! sector number modulo 16 for CC or modulo 8 for EC
      INTEGER NCRATE    ! logical adc crate 0-11
      INTEGER JCRATE    ! ncrate mod 6
      INTEGER IFLAG     ! temp flag
C
C                 >>> SECTOR POSITIONS IN PREAMP BOXES <<<
C       In each preamp box there are four columns of 24 preamp boards.
C       Every six preamp boards handles a sector. After the cables leave the
C       preamp box they are arranged in order of sector.
C       FRONT VIEW OF PAIRS OF PREAMP BOXES AS THEY SIT ON THE SIDE OF CRYOSTAT.
C       EACH NUMBER IS A SECTOR LOCATION IN WHICH 6 PREAMP BOARDS RESIDE:
C                  1                 1            (preamp board 1 - upper left)
C                ________________   _______________
C                |   8  0  7 15 |   | 23 31 24 16 |
C                |  10  2  5 13 |   | 21 29 26 18 |   EAST CC PREAMP BOXES
C                |  12  4  3 11 |   | 19 27 28 20 |       8  and  9
C                |  14  6  1  9 |   | 17 25 30 22 |     ( SE and NE )
C                |______________|   |_____________|
C
C                ________________   _______________
C                |   9  1  6 14 |   | 22 30 25 17 |
C                |  11  3  4 12 |   | 20 28 27 19 |   WEST CC PREAMP BOXES
C                |  13  5  2 10 |   | 18 26 29 21 |       2  and  3
C                |  15  7  0  8 |   | 16 24 31 23 |     ( NW and SW )
C                |______________|   |_____________|
C
C            for EC preamp boxes, the left half on each box has low etas:
C                ________________   _______________
C                |  12  8 11 15 |   |  4  0  3  7 |
C                |  13  9 10 14 |   |  5  1  2  6 |   TWO EC PREAMP BOXES
C                |  14 10  9 13 |   |  6  2  1  5 |   (0 & 1   or 6 & 7 )
C                |  15 11  8 12 |   |  7  3  0  4 |   note: they are
C                |______________|   |_____________|     equal modulo 8
C
C                ________________   _______________
C                |  27 31 28 24 |   | 19 23 20 16 |
C                |  26 30 29 25 |   | 18 22 21 17 |   TWO EC PREAMP BOXES
C                |  25 29 30 26 |   | 17 21 22 18 |   (4 & 5   or 10 & 11 )
C                |  24 28 31 27 |   | 16 20 23 19 |   note: they are
C                |______________|   |_____________|     equal module 8
C
C
C     The arrays LSSET below express the location of sectors in the
C     preamp box.
C     The following arrays take LOGBLS/6 as an argument. (i.e. the input
C     is sector group) The output is sector position in the preamp box
C
      INTEGER LSSET1(0:15), LSSET2(0:15), LSSET3(0:15), LSSET4(0:15)
C
C
C     For Preamp boxes 0, 1, 6, 7, 8 the order number is:
C
      DATA LSSET1 / 4,11, 5,10, 6, 9, 7, 8,   0,15, 1,14, 2,13, 3,12 /
C
C
C     For Preamp boxes 3, 4, 5, 10, 11 the order number is:
C
      DATA LSSET2 / 3,12, 2,13, 1,14, 0,15,   7, 8, 6, 9, 5,10, 4,11 /
C
C     For Preamp box   2 the order number is:
C
      DATA LSSET3 / 11,4, 10,5, 9, 6, 8, 7,  15,0, 14,1, 13,2, 12,3 /
C
C
C     For Preamp box   9 the order number is:
C
      DATA LSSET4 /  12,3, 13,2, 14,1, 15,0,  8, 7, 9, 6, 10,5, 11,4 /
C
C
C                 >>> ADC CRATE TO PREAMP BOX MAPPING <<<
C       Find the ADC CRATE associated with a given PREAMP BOX.
C       Preamp Box numbering starts from the north west preamp box and goes
C       counter clockwise as viewed from above. Box numbers: 0 to 11
C       see page 7 and 11 of D0 note 774 - which should show:
C
C
C                               north
C        NW                                                   NE
C                      ADC37  P00 | P11 ADC47                        <-  ECN
C                      ADC27  P01 | P10 ADC57                        <-  ECN
C                      ADC7   P02 | P09 ADC17                        <-  CC
C                      ADC18  P03 | P08 ADC8                         <-  CC
C                      ADC58  P04 | P07 ADC28                        <-  ECS
C                      ADC48  P05 | P06 ADC38                        <-  ECS
C        SW                                                   SE
C                               south
C
      INTEGER IPRBOX(0:11)      ! look up preamp box number for adc crate
      DATA IPRBOX /  2, 9, 1, 0, 11, 10,   8, 3, 7, 6, 5, 4 /
C
C
C
C
C
C
C              >>> ADC CRATES FOR MERGE CROSSOVERS CHANNELS <<<
C       Massless gap adc mapping - CC massless gaps crossover to EC adc crates
C       For each CC there are two EC adc crates to send massless gap signals
C       depending on sector.
C       In addition, EC non-massless gaps for eta 7-11 crossover to cc adc
C       crates.
C               From page 7 of D0 note 774:
C
C           ADC Crate            Cable Number
C                            H         S      E
C            7+0=7           1       0-15    0-11
C            +10=17          1       16-31   0-11
C            +20=27          1       0-7     12-44, MG, ICD
C            +30=37          1       8-15    12-44, MG, ICD
C            +40=47          1       16-23   12-44, MG, ICD
C            +50=57          1       24-31   12-44, MG, ICD
C                            H         S      E
C             8+0=8          0       0-15    0-11
C             +10=18         0       16-31   0-11
C             +20=28         0       0-7     12-44, MG, ICD
C             +30=38         0       8-15    12-44, MG, ICD
C             +40=48         0       16-23   12-44, MG, ICD
C             +50=58         0       24-31   12-44, MG, ICD
C
      INTEGER XBOX(0:11,0:1)   ! look up crossover premap box for adc crate
      DATA XBOX /              ! arguments: IBOX, LOGBLS/48
C                               ! for low sectors (0-7 or 16-23) LOGBLS/48=0
     &  2, 2, 1,  5, 3, 3,   8, 8, 7,  11, 9, 9,
C                               ! for hi sectors (8-15 or 24-31) LOGBLS/48=1
     &  2, 2, 0,  4, 3, 3,   8, 8, 6,  10, 9, 9 /
C
C
C        >>>>>  CC ADC SIGNALS FROM EC PREAMP BOARD FOR ETA 7-11 <<<<
C
C    In a CC ADC crate, certain channels come from EC Preamp Boxes.
C    The first preamp board of every six boards on the left half
C    of the EC Preamp Boxes have signals for eta 7-11 that will
C    be sent to CC ADC crates. The following array is part of the
C    inverse transformation. It gives the read_out_tower in the
C    EC Preamp Board.
C
C    gives tower in EC preamp card for eta 7-11
C    look up by read_out_tower in BLS crate, CC bls board mod 6
C
      INTEGER ECBRDT(0:3,3:5)   ! ECBRDT( LROTOW, MOD(LOGBLS,6) )
      DATA ECBRDT /
     & 0, 0, 0, 0,              ! board 3 has EC signals from tower 0
     & 2, 2, 0, 0,              ! board 4 has EC signals from tower 2 & 0
     & 3, 3, 1, 1/              ! board 5 has EC signals from tower 3 & 1
C
C
C    depth in EC preamp card
C    look up by depth, read_out_tower, CC bls board mod 6
C    if >= 0 then it is the depth in the EC eta 7-11 board
C    else it comes from CC preamp board
C
      INTEGER ECBRD(0:11,0:3,3:5)       ! ECBRD(LDEPTH,LROTOW,MOD(LOGBLS,6))
      DATA ECBRD /
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-2,-2,        ! board 3   eta 6 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-2,-2,        ! board 3   eta 6 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1,-2,        ! board 3   eta 7 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 3,-2,        ! board 3   eta 7 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2,-2,        ! board 4   eta 8 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1, 4, 5,-2,        ! board 4   eta 8 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1, 5, 6,-2,-2,        ! board 4   eta 9 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1, 8, 9,-2,-2,        ! board 4   eta 9 phi 1
     &  -1,-1,-1,-1,-1,-1,-1, 0, 2, 3, 4,-2,        ! board 5   eta 10
     &  -1,-1,-1,-1,-1,-1,-1, 5, 7, 8, 9,-2,        ! board 5   eta 10
     &  -1,-1,-1,-1, 1, 2, 3, 4, 5,-2,-2,-2,        ! board 5   eta 11
     &  -1,-1,-1,-1, 7, 8, 9,10,11,-2,-2,-2/        ! board 5   eta 11
C
C
C
C
C              <<<<<   SPECIAL BLS  #11 & #12  IN EC CRATE >>>>>
C
C
C       EFFECTIVE BLS 11 OUTPUT         EFFECTIVE BLS 12 OUTPUT
C
C 0             E32_EM1                 empty
C 1             E32_EM2                 empty
C 2             E32_EM3                 empty
C 3             E32_EM4                 empty
C 4             empty                   empty
C 5             empty                   empty
C 6             empty                   empty
C 7             E32_IFH1                empty
C 8             E32_IFH2                empty
C 9             E32_IFH3                E44_IFH3
C 10            E32_IFH4                E44_IFH4
C 11            E32_ICH                 E44_ICH
C
C 0             E34_EM1                 CC  MG:F0-E7
C 1             E34_EM2                 EC OH0:F0-E7
C 2             E34_EM3                 CC  MG:F1-E7
C 3             E34_EM4                 EC OH0:F1-E7
C 4             empty                   CC  MG:F0-E9
C 5             empty                   EC OH0:F0-E9
C 6             empty                   CC  MG:F1-E9
C 7             E34_IFH1                EC OH0:F1-E9
C 8             E34_IFH2                CC  MG:F0-E11
C 9             E34_IFH3                EC MH0:F0-E11
C 10            E34_IFH4                CC  MG:F1-E11
C 11            E34_ICH                 EC MH0:F1-E11
C
C 0             E37_EM1                 ICD
C 1             E37_EM2                 ICD
C 2             E37_EM3                 ICD
C 3             E37_EM4                 ICD
C 4             empty                   ICD
C 5             empty                   ICD
C 6             empty                   ICD
C 7             E37_IFH1                ICD
C 8             E37_IFH2                ICD
C 9             E37_IFH3                ICD
C 10            E37_IFH4                ICD
C 11            E37_ICH                 ICD
C
C 0             empty                   CC  MG:F0-E8
C 1             empty                   EC OH0:F0-E8
C 2             empty                   CC  MG:F1-E8
C 3             empty                   EC OH0:F1-E8
C 4             empty                   CC  MG:F0-E10
C 5             empty                   EC OH0:F0-E10
C 6             empty                   CC  MG:F1-E10
C 7             E41_IFH1                EC OH0:F1-E10
C 8             E41_IFH2                EC MH0:F0-E12
C 9             E41_IFH3                EC MH0:F1-E12
C 10            E41_IFH4                empty
C 11            E41_ICH                 empty
C
C
      INTEGER BLS12(4,0:11,0:3)         ! type, depth, tower
C        cyro, board, tower, depth
C                               cryo: 0-not used  1-CC  2-EC 3-ICD
      DATA BLS12 /
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  0, 0,  0, 0,
     &  2, 0,  3, 2,                    ! E44_IFH3
     &  2, 0,  3, 3,                    ! E44_IFH4
     &  2, 0,  3, 4,                    ! E44_ICH
C
     &  1,  3, 2, 10,                   ! CC  MG:F0-E7
     &  2,-11, 0,  0,                   ! EC OH0:F0-E7
     &  1,  3, 3, 10,                   ! CC  MG:F1-E7
     &  2,-11, 0,  2,                   ! EC OH0:F1-E7
     &  1,  4, 2,  8,                   ! CC  MG:F0-E9
     &  2,-11, 0,  4,                   ! EC OH0:F0-E9
     &  1,  4, 3,  8,                   ! CC  MG:F1-E9
     &  2,-11, 0,  7,                   ! EC OH0:F1-E9
     &  1,  5, 2,  4,                   ! CC  MG:F0-E11
     &  2,-11, 1,  0,                   ! EC MH0:F0-E11
     &  1,  5, 3,  4,                   ! CC  MG:F1-E11
     &  2,-11, 1,  6,                   ! EC MH0:F1-E11
C
     &  3,  0, 0,  1,                   ! ICD
     &  3,  0, 0,  1,
     &  3,  0, 0,  2,
     &  3,  0, 0,  2,
     &  3,  0, 0,  3,
     &  3,  0, 0,  3,
     &  3,  0, 1,  4,
     &  3,  0, 1,  4,
     &  3,  0, 1,  5,
     &  3,  0, 1,  5,
     &  3,  0, 1,  6,
     &  3,  0, 1,  6,
C
     &  1,  4, 0,  9,                   ! CC  MG:F0-8
     &  2,-11, 2,  0,                   ! EC OH0:F0-E8
     &  1,  4, 1,  9,                   ! CC  MG:F1-E8
     &  2,-11, 2,  3,                   ! EC OH0:F1-E8
     &  1,  5, 0,  7,                   ! CC  MG:F0-E10
     &  2,-11, 3,  1,                   ! EC OH0:F0-E10
     &  1,  5, 1,  7,                   ! CC  MG:F1-E10
     &  2,-11, 3,  6,                   ! EC OH0:F1-E10
     &  2,-11, 3, 10,                   ! EC MH0:F0-E12
     &  2,-11, 3, 11,                   ! EC MH0:F1-E12
     &  0,  0, 0,  0,
     &  0,  0, 0,  0/
C
C----------------------------------------------------------------------
C
C
      ICOND = 0                         ! innocent until proven otherwise
C
      IF( LADC.LT.0 .OR. LBLS.LT.0 .OR. LROTOW.LT.0 .OR. LDEPTH.LT.0
     & .OR. LADC.GE.12 .OR. LBLS.GE.8 .OR. LROTOW.GE.4
     & .OR. LDEPTH.GE.12 ) THEN
        ICOND = 20                      ! guilty - bad inputs
        GOTO 999                        ! return
      ENDIF
C
C     TEST BEAM KLUDGE
C
      IF( LDET.EQ.PP_TB90L1 ) THEN
        IBOX = PP_TB90L1
        IPRBRD = TB_PRE( LBLS, LADC )
        IDEPTH = LDEPTH
        IROTOW = LROTOW
        IF( IPRBRD.LE.17 .OR.  IPRBRD.GE.78 .OR.
     &     (IPRBRD.GE.36 .AND. IPRBRD.LE.59) ) THEN
          IF(LROTOW.LE.1) IROTOW = IROTOW + 2
          IF(LROTOW.GT.1) IROTOW = IROTOW - 2
        ENDIF
        GOTO 999
      ELSEIF( LDET.EQ.PP_TB90L2 ) THEN
        IBOX = PP_TB90L2
        IPRBRD = TB_PREL2( LBLS, LADC )
        IDEPTH = LDEPTH
        IROTOW = LROTOW
        IF( IPRBRD.GE.78 ) THEN
          IF(LROTOW.LE.1) IROTOW = IROTOW + 2
          IF(LROTOW.GT.1) IROTOW = IROTOW - 2
        ENDIF
        GOTO 999
      ELSEIF( LDET.EQ.PP_TB90L3 ) THEN
        IBOX = PP_TB90L3
        ICOND=20
        GOTO 999
      ENDIF
C
C     map ADC crate number to the interval 0-11 for the detector
C     indicate others (test beam, 5000 channel test) by -1
C
      IF( MOD(LCRATE,10) .EQ. 7 ) THEN          ! north half of detector
        NCRATE = LCRATE / 10
      ELSEIF( MOD(LCRATE,10) .EQ. 8 ) THEN
        NCRATE = LCRATE / 10 + 6                ! south half of detector
      ELSE
        NCRATE = -1                             ! not a detector crate
      ENDIF
C
C       ---------------------- UNMERGED ------------------------
C
C     Default (unmerged) mapping
C
C     give an unappropriate crate, you will force an unmerged pattern
C
      IBOX = 14
      IF( LDET .LT. 12 .AND. NCRATE .GE. 0  .AND.  NCRATE .LE. 11 )
     &  IBOX = IPRBOX(NCRATE)
C
C
      IDEPTH = LDEPTH
      IROTOW = LROTOW
      LOGBLS = 8 * LADC + LBLS
C
C       ---------------------- CC EXCEPTIONS ------------------------
C
      IF( IBOX.EQ.2 .OR. IBOX.EQ.3 .OR. IBOX.EQ.8 .OR. IBOX.EQ.9 ) THEN
C
C       The merge pattern repeats every six bls boards
C
        IMOD = MOD( LOGBLS, 6 )
C
        IF( IMOD .GE. 3 ) THEN                          ! from merge boards
          IFLAG = ECBRD(LDEPTH, LROTOW, IMOD )          ! CC or EC origin?
          IF( IFLAG .GE. 0 ) THEN                       ! EC preamp box channel
            IBOX = XBOX(IBOX,LOGBLS/48)                 ! crossover EC box
            ISECT  = LOGBLS / 6                         ! sector number mod 16
            LOGBLS = 12 * ISECT
            LOGBLS = MOD( LOGBLS, 96)
            IROTOW = ECBRDT(LROTOW,IMOD)
            IDEPTH = IFLAG
          ELSEIF( IFLAG .EQ. -2 ) THEN
            ICOND = 1
          ENDIF
        ELSE
          IF( LDEPTH .EQ. 11 ) ICOND = 1
        ENDIF
C
C       ---------------------- EC EXCEPTIONS ------------------------
C
      ELSEIF ( IBOX .LT. 12 ) THEN
        IMOD = MOD( LOGBLS, 12)
        IF( IMOD .EQ. 11 ) THEN                 ! special BLS
          IFLAG  = BLS12(1,LDEPTH,LROTOW)       ! 1=CC 2=EC 0=nowhere
          IROTOW = BLS12(3,LDEPTH,LROTOW)
          IDEPTH = BLS12(4,LDEPTH,LROTOW)
          IF ( IFLAG .EQ. 1 ) THEN
            IBOX = XBOX(IBOX,1)
            ISECT = LOGBLS / 12
            LOGBLS = 6 * ISECT + BLS12(2,LDEPTH,LROTOW)
            JCRATE = MOD(NCRATE,6)
            IF( JCRATE.EQ.3 .OR. JCRATE.EQ.5 ) LOGBLS = LOGBLS + 48
          ELSEIF( IFLAG .EQ. 2 ) THEN
            LOGBLS = LOGBLS + BLS12(2,LDEPTH,LROTOW)
          ELSEIF( IFLAG .EQ. 3 ) THEN                     ! ICD
            ICOND=2
            IF (MOD(LCRATE,10) .EQ. 7) IBOX = 12             ! north half
            IF (MOD(LCRATE,10) .EQ. 8) IBOX = 13             ! south half
            ISECT = (LCRATE/10 - 2)*8 + LOGBLS/12
          ELSE
            ICOND = 1
          ENDIF
        ELSE
          LOGBLS = LOGBLS + 1
        ENDIF
C
        IF( IMOD .EQ. 0 ) THEN
          IF( LROTOW .LE. 1 .AND. LDEPTH .LE. 5 ) ICOND = 1
          IF( LROTOW .GE. 2 .AND. LDEPTH .LE. 2 ) ICOND = 1
        ELSEIF( IMOD .EQ. 2 ) THEN
          IF( LROTOW .GE. 2 .AND. LDEPTH .EQ. 9 ) ICOND = 1
        ELSEIF( IMOD .EQ. 3 ) THEN
          IF( LROTOW .LE. 1 .AND. LDEPTH .EQ. 10 ) ICOND = 1
        ELSEIF( IMOD .GE. 7 .AND. IMOD .LE. 9 ) THEN
          IF( LDEPTH .GE. 3 .AND. LDEPTH .LE. 5 ) ICOND = 1
        ELSEIF( IMOD .EQ. 10 ) THEN
          IF( LDEPTH .GE. 4 .AND. LDEPTH .LE. 6 ) ICOND = 1
          IF( LROTOW .EQ. 3 .AND. LDEPTH .LE. 6 ) ICOND = 1
        ENDIF
C
      ENDIF
C
      IF( LDET.EQ.PP_QUAD ) THEN
        IBOX = PP_QUAD
        IPRBRD = 6 * LSSET2( LOGBLS/6 ) + MOD(LOGBLS,6)
      ELSEIF( IBOX.EQ.2 ) THEN
        IPRBRD = 6 * LSSET3( LOGBLS/6 ) + MOD(LOGBLS,6)
      ELSEIF( IBOX.EQ.9 ) THEN
        IPRBRD = 6 * LSSET4( LOGBLS/6 ) + MOD(LOGBLS,6)
      ELSEIF( IBOX.EQ.12 .OR. IBOX.EQ.13 ) THEN
        IPRBRD = 2 * ISECT + MOD(LDEPTH,2)
      ELSEIF( MOD(IBOX,6) .LE. 2 ) THEN
        IPRBRD = 6 * LSSET1( LOGBLS/6 ) + MOD(LOGBLS,6)
      ELSEIF( LDET.EQ.PP_5KC ) THEN
        IBOX = PP_5KC
        IPRBRD = 6 * LSSET1( LOGBLS/6 ) + MOD(LOGBLS,6)
      ELSE
        IPRBRD = 6 * LSSET2( LOGBLS/6 ) + MOD(LOGBLS,6)
      ENDIF
C
C       -------------- MISSING EC MAIN RING CHANNELS ---------------
C
      IF (LCRATE.EQ.7 .AND. LADC.EQ.0) THEN
        IF (LBLS.EQ.3 .AND. LROTOW.EQ.3 .AND. LDEPTH.EQ.10) ICOND=1
        IF (LBLS.EQ.4 .AND. LROTOW.EQ.1 .AND. LDEPTH.EQ.10) ICOND=1
        IF (LBLS.EQ.4 .AND. LROTOW.EQ.3 .AND. LDEPTH.EQ. 9) ICOND=1
        IF (LBLS.EQ.5 .AND. LROTOW.EQ.1 .AND. LDEPTH.EQ.10) ICOND=1
      ENDIF
      IF (LCRATE.EQ.18 .AND. LADC.EQ.11) THEN
        IF (LBLS.EQ.5 .AND. LROTOW.EQ.2 .AND. LDEPTH.EQ.10) ICOND=1
        IF (LBLS.EQ.6 .AND. LROTOW.EQ.0 .AND. LDEPTH.EQ.10) ICOND=1
        IF (LBLS.EQ.6 .AND. LROTOW.EQ.2 .AND. LDEPTH.EQ. 9) ICOND=1
        IF (LBLS.EQ.7 .AND. LROTOW.EQ.0 .AND. LDEPTH.EQ.10) ICOND=1
      ENDIF
C
  999 RETURN
      END
