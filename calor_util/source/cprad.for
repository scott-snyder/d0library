      SUBROUTINE CPRAD(IBOX,IPRBRD,IROTOW,IDEPTH,LCRATE,LADC,LBLS
     &,LROTOW,LDEPTH,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert from PREAMP scheme to ADC scheme
C-
C-   Inputs  : IBOX preamp box - 0 to 11 THE detector or
C-                                    13 for 5000 channel test
C-                                    14 for quadrant test
C-                                    etc
C-                                    12, 13 ICD
C-                for IBOX >= 12 use defs in D0$params:cal_pulse_list.params
C-             IPRBRD preamp board number
C-             IROTOW read out tower on preamp board
C-             IDEPTH depth on preamp board
C-   Outputs : LCRATE adc crate
C-             LADC adc board
C-             LBLS bls board
C-             LROTOW bls read out tower
C-             LDEPTH depth on bls board
C-   Controls: ICOND  0 = valid    1 = preamp output does not reach a BLS
C-                   20 = invalid inputs
C-
C-   Created  23-NOV-1988   James Kourlas  NYUHEP::KOURLAS
C-   Modified  7-DEC-1989   James Kourlas for new ADC crate numbering
C-   Modified 16-JAN-1990   James Kourlas quadrant tests
C-   Modified 27-JAN-1990   James Kourlas for unique test beam mapping
C-   Modified 14-FEB-1990   James Kourlas for north CC sector changes
C-   Modified 15-MAY-1990   James Kourlas for ICOND=1 condition
C-   Updated  11-MAR-1992   Joan Guida  for ICD
C-   Modified 15-OCT-1992   James Kourlas for new comments
C-   Modified  21-NOV-1992  Joan Guida    for missing main-ring channels
C-   Updated   5-NOV-1993   Joan Guida    Fixed missing main-ring channels
C-
C-   Expected user of this code: The main use of this code is in
C-      the translation of the calorimetry test pulser pattern. This
C-      pattern is defined with reference to the preamp boxes and has
C-      to be filtered with the translation below. In addition, this
C-      program will be used in the process of hardware debugging.
C-
C-   See D0 Note 774 for documentation. A copy is in
C-                                   D0$DOCS:CALORIMETER_ADDRESSING.MEM
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_PULSE_LIST.PARAMS'
      INCLUDE 'D0$PARAMS:TB90L1_PRE_TO_BLS.DEF'
      INCLUDE 'D0$PARAMS:TB90L2_PRE_TO_BLS.DEF'
      INCLUDE 'D0$INC:ICD.INC'
C                         passed variables:
      INTEGER IBOX, IPRBRD, IROTOW, IDEPTH              ! inputs
      INTEGER LCRATE, LADC, LBLS, LROTOW, LDEPTH        ! outputs
      INTEGER ICOND                                     ! status flag
C                         local variables:
      INTEGER IPRMOD    ! preamp board number modulo 6 - 6 boards to a sector
      INTEGER LPRBRD    ! logical preamp board number - when ordered by sector
      INTEGER ISECT     ! sector number modulo 16 for CC or modulo 8 for EC
      INTEGER JBOX      ! IBOX MOD 6
C
C
C
C       INTRODUCTION TO SECTOR LABELING as refered to within.
C       As defined in d0 note 774, the calorimeter (CC and both ECs)
C       is regarded as two halves - north and south. Sector labeling,
C       when viewed from the interaction point, starts from the
C       top, then proceeds counter clockwise (see page 5 of d0 note 774).
C       Sector labeling starts from 0. Eta starts from 0 and is positive.
C
C       PREAMP BOARD LABELING - There are 96 preamp boards to a preamp
C       box. They are in four columns (they insert like FERMILAB cafeteria
C       trays). Preamp board 0 is in the upper left corner of the preamp
C       box (when viewed from the side that the preamp boards insert).
C       Boards 0-23 are in the first column. Boards 24-47 are in the second
C       column. Boards 48-71 are in the third column. Boards 72-95 are
C       in the fourth and right most column.
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
C     Notice that in EC Preamp boxes numbers are sector doubly degenerate.
C     The left side numbers refer to six preamp boards which handle low
C     eta signals from the left bank of the feedthru. The right half of the
C     crate handles hi eta signals from the right bank of the feedthru.
C
C     The arrays ISSET below express the order of signals when ordered by
C     sector and thus when cables are reordered at the back of the preamp box.
C
C     The following arrays take IPRBRD/6 as an argument. (i.e. the input
C     is "preamp box order for each sector set") The output is in sector
C     order.
C
      INTEGER ISSET1(0:15), ISSET2(0:15), ISSET3(0:15), ISSET4(0:15)
C
C     There are four types of reordering. One type reorders cables from
C     preamp boards like CC preamp box 8. First cables are taken from the
C     six preamp boards in the set marked "0" in the second column of
C     preamp boards, then from the set marked "1" in the third column, etc.
C     For each set of six preamp boards there is an order number.
C
C
C     For Preamp boxes 0, 1, 6, 7, 8 the order number is:
C
      DATA ISSET1 / 8, 10,12,14,  0, 2, 4, 6,  7, 5, 3, 1, 15,13,11, 9 /
C
C
C     For Preamp boxes 3, 4, 5, 10, 11 the order number is:
C
      DATA ISSET2 / 6,  4, 2, 0, 14,12,10, 8,  9,11,13,15,  1, 3, 5, 7 /
C
C     For Preamp box   2 the order number is:
C
      DATA ISSET3 / 9, 11,13,15,  1, 3, 5, 7,  6, 4, 2, 0, 14,12,10, 8 /
C
C
C     For Preamp box   9 the order number is:
C
      DATA ISSET4 / 7,  5, 3, 1, 15,13,11, 9,  8,10,12,14,  0, 2, 4, 6 /
C
C     usage: divide preamp board number by 6 then insert:
C       sector set = ISSET1( IPRBRD / 6 )
C       example: For preamp box 2, preamp board 9 ( 9/6=1 ) is in sector set 11.
C       This is the 12th sector (0=1st) in the quadrant handled by
C       the preamp box 2.
C
C
C
C
C
C                 >>> ADC CRATE TO PREAMP BOX MAPPING <<<
C       Find the ADC CRATE associated with a given PREAMP BOX.
C       Preamp Box numbering starts from the north west preamp box and goes
C       counter clockwise as viewed from above. Box numbers: 0 to 11
C       Call this program with IBOX=12 or 13 to force an unmerged pattern.
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
      INTEGER LPRADC(0:11)      ! look up adc crate number for preamp box
      DATA LPRADC /             ! for sector or eta see table below
     &  37,                     ! sector 8-15;  eta 12-44; north half
     &  27,                     ! sector 0-7;   eta 12-44; north half
     &  7,                      ! sector 0-15;  eta 0-11;  north half
     &  18,                     ! sector 16-31; eta 0-11;  south half
     &  58,                     ! sector 24-31; eta 12-44; south half
     &  48,                     ! sector 16-23; eta 12-44; south half
     &  38,                     ! sector 8-15;  eta 12-44; south half
     &  28,                     ! sector 0-7;   eta 12-44; south half
     &  8,                      ! sector 0-15;  eta 0-11;  south half
     &  17,                     ! sector 16-31; eta 0-11;  north half
     &  57,                     ! sector 24-31; eta 12-44; north half
     &  47/                     ! sector 16-23; eta 12-44; north half
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
      INTEGER MGADC(0:11,0:1)   ! look up crossover adc crate for preamp box
      DATA MGADC /              ! arguments: IBOX,  LPRBRD/48
C                               ! for low sectors (0-7 or 16-23) LPRBRD/48=0
     &  7,7,27, 48,18,18, 8,8,28, 47,17,17,
C                               ! for hi sectors (8-15 or 24-31) LPRBRD/48=1
     &  7,7,37, 58,18,18, 8,8,38, 57,17,17/
C
C                 >>> CC EXCEPTION TABLES <<<
C     In every CC sector (6 preamp boards) there are massless gap
C     channels that must be sent to EC bls. Also, there are CC preamps
C     which have no physical signal but are thrown away and replaced
C     by hadron channels from EC cables for eta 7-11
C     for CC preamps:
C     CCEXCP is a flag for massless gap and lost channels
C     It is a function of DEPTH, TIME_SLICE, IPRBRD mod 6
C                          where TIME_SLICE = IROTOW / 2
C
C               EXCEPTION KIND:         ! by depth, eta type, preamp board
      INTEGER CCEXCP(0:11,0:1,3:5)      ! eta type means even or odd eta
      DATA CCEXCP /                     ! 3=massless gap 1=goes nowhere
     &  0,0,0,0,0,0,0,0,0,0,1,1,        ! preamp board 3   eta 6
     &  0,0,0,0,0,0,0,0,0,0,3,1,        ! preamp board 3   eta 7
     &  0,0,0,0,0,0,0,0,0,3,1,1,        ! preamp board 4   eta 8
     &  0,0,0,0,0,0,0,0,3,1,1,1,        ! preamp board 4   eta 9
     &  0,0,0,0,0,0,0,3,1,1,1,1,        ! preamp board 5   eta 10
     &  0,0,0,0,3,1,1,1,1,1,1,1/        ! preamp board 5   eta 11
C
C               DEPTH REPLACEMENT FOR MASSLESS GAPS
C     Massless gaps from CC are send to EC special massless gaps BLS. They
C     are put into the depth location as given in the array below.
C     CCMGAP is a function of IROTOW, IPRBRD mod 6
C
      INTEGER CCMGAP(0:3,3:5)           ! by tower, preamp board
      DATA CCMGAP /                     !
     &  0, 0, 0, 2,                     ! preamp board 3
     &  0, 2, 4, 6,                     ! preamp board 4
     &  4, 6, 8,10/                     ! preamp board 5
C
C
C
C
C
C
C                 >>> MERGE TRANSLATION FROM EC TO CC <<<
C       ETA 7-11 FROM EC   see page 29 of D0 note 774
C       Eta 7-11 from EC comes from the first preamp board of each sector
C       on the left side of the preamp box. The signals from this board
C       have to be distributed as follows: a) massless gaps must be sent
C       to the massless gap BLS in an EC BLS crate. b) non-massless gaps
C       must be sent to a CC crate. c) some signals are not used.
C       All depths and towers change - they are listed below.
C
C       LECMRG is a function of "type of data", DEPTH, IROTOW
C       where "type of data" means LROTOW, LDEPTH, BLS mod 12
C
      INTEGER LECMRG(3,0:11,0:3)
      DATA LECMRG /
C       first preamp card in EC sector - tower, depth, BLS index
C                                       first tower on card - eta 7,9
C       a   b   c   --->   a = read out tower; b = depth; c = BLS index
     &  1,  1, 11,      ! OH0:F0-E7     massless gap - note bls 11
     &  2, 10,  3,      ! OH1:F0-E7     eta 7 => bls 3 in CC
     &  1,  3, 11,      ! OH0:F1-E7     massless gap
     &  3, 10,  3,      ! OH1:F1-E7
     &  1,  5, 11,      ! OH0:F0-E9     massless gap
     &  2,  8,  4,      ! OH1:F0-E9
     &  2,  9,  4,      ! OH2:F0-E9
     &  1,  7, 11,      ! OH0:F1-E9     massless gap
     &  3,  8,  4,      ! OH1:F1-E9
     &  3,  9,  4,      ! OH2:F1-E9
     &  0,  0, -1,      !               no module connection - mark by -1
     &  0,  0, -1,      !               no module connection
C                                       second tower on card - eta 11
     &  1,  9, 11,      ! MH0:F0-E11    massless gap
     &  2,  4,  5,      ! MH1:F0-E11
     &  2,  5,  5,      ! MH2:F0-E11
     &  2,  6,  5,      ! OH1:F0-E11
     &  2,  7,  5,      ! OH2:F0-E11
     &  2,  8,  5,      ! OH3:F0-E11
     &  1, 11, 11,      ! MH0:F1-E11    massless gap
     &  3,  4,  5,      ! MH1:F1-E11
     &  3,  5,  5,      ! MH2:F1-E11
     &  3,  6,  5,      ! OH1:F1-E11
     &  3,  7,  5,      ! OH2:F1-E11
     &  3,  8,  5,      ! OH3:F1-E11
C                                       third tower on card - eta 8
     &  3,  1, 11,      ! OH0:F0-E8     massless gap
     &  0,  9,  4,      ! OH1:F0-E8
     &  0, 10,  4,      ! OH2:F0-E8
     &  3,  3, 11,      ! OH0:F1-E8     massless gap
     &  1,  9,  4,      ! OH1:F1-E8
     &  1, 10,  4,      ! OH2:F1-E8
     &  0,  0, -1,      !               no module connection
     &  0,  0, -1,      !               no module connection
     &  0,  0, -1,      !               no module connection
     &  0,  0, -1,      !               no module connection
     &  0,  0, -1,      !               no module connection
     &  0,  0, -1,      !               no module connection
C                                       forth tower on card - eta 10 and eta 12
     &  0,  7,  5,      ! MH1:F0-E10
     &  3,  5, 11,      ! OH0:F0-E10    massless gap
     &  0,  8,  5,      ! OH1:F0-E10
     &  0,  9,  5,      ! OH2:F0-E10
     &  0, 10,  5,      ! OH3:F0-E10
     &  1,  7,  5,      ! MH1:F1-E10
     &  3,  7, 11,      ! OH0:F1-E10    massless gap
     &  1,  8,  5,      ! OH1:F1-E10
     &  1,  9,  5,      ! OH2:F1-E10
     &  1, 10,  5,      ! OH3:F1-E10
     &  3,  8, 11,      ! MH0:F0-E12    massless gap
     &  3,  9, 11/      ! MH0:F1-E12    massless gap
C
C
C
C----------------------------------------------------------------------
C
      ICOND = 0                         ! innocent until proven otherwise
C
      IF( IPRBRD.GE.96 .OR. IROTOW.GE.4 .OR. IDEPTH.GE.12 ) THEN
        ICOND = 20                      ! guilty - bad inputs
        GOTO 999                        ! return
      ENDIF
C
C       -------------------- TEST BEAM KLUDGE ------------------
C
      IF( IBOX.EQ.PP_TB90L1 ) THEN      ! first load of test beam 1990
	LCRATE = 7
        LADC = TB_BLS(2,IPRBRD)
        LBLS = TB_BLS(3,IPRBRD)
        LROTOW = IROTOW
        LDEPTH = IDEPTH
        IF( IPRBRD.LE.17 .OR.  IPRBRD.GE.78 .OR.
     &     (IPRBRD.GE.36 .AND. IPRBRD.LE.59) ) THEN
          IF(IROTOW.LE.1) LROTOW = LROTOW + 2
          IF(IROTOW.GT.1) LROTOW = LROTOW - 2
        ENDIF
        LCRATE = 7
        GOTO 999
      ELSEIF( IBOX.EQ.PP_TB90L2 ) THEN  ! second load of test beam 1990
        LCRATE = 87
        LADC = TB_BLSL2(2,IPRBRD)
        LBLS = TB_BLSL2(3,IPRBRD)
        LROTOW = IROTOW
        IF( IPRBRD.GE.78 ) THEN
          IF(IROTOW.LE.1) LROTOW = LROTOW + 2
          IF(IROTOW.GT.1) LROTOW = LROTOW - 2
        ENDIF
        LDEPTH = IDEPTH
        GOTO 999
      ELSEIF( IBOX.EQ.PP_TB90L3 ) THEN  ! third load of test beam 1990
        ICOND = 20
        GOTO 999
      ENDIF
C
C       -------------------- UNMERGED DEFAULTS ------------------
C
C     Find ADC crate number from Preamp Box number:
C
      IF( IBOX.GE.0 .AND. IBOX.LT.12 ) LCRATE = LPRADC( IBOX )
C
C     Find the order of the preamp board when ordered by sector: LPRBRD
C
C     for boxes 0 1 6 7 8 and 5000 CHANNEL TEST use sector order ISSET1
C
      IF ( LICD ) THEN
        LPRBRD = 12 * (MOD(IPRBRD,16)/2+1) - 1
        LCRATE = (IPRBRD/16+2)*10 + IBOX-5
      ELSEIF( IBOX.EQ.PP_QUAD ) THEN
        LCRATE = 77
        LPRBRD = 6 * ISSET2( IPRBRD/6 ) + MOD(IPRBRD,6)
      ELSEIF( IBOX.EQ.2 ) THEN
        LPRBRD = 6 * ISSET3( IPRBRD/6 ) + MOD(IPRBRD,6)
      ELSEIF( IBOX.EQ.9 ) THEN
        LPRBRD = 6 * ISSET4( IPRBRD/6 ) + MOD(IPRBRD,6)
      ELSEIF( MOD(IBOX,6) .LE. 2 ) THEN
        LPRBRD = 6 * ISSET1( IPRBRD/6 ) + MOD(IPRBRD,6)
      ELSEIF( IBOX.EQ.PP_5KC ) THEN
        LCRATE = 67
        LPRBRD = 6 * ISSET1( IPRBRD/6 ) + MOD(IPRBRD,6)
      ELSE
        LPRBRD = 6 * ISSET2( IPRBRD/6 ) + MOD(IPRBRD,6)
      ENDIF
C
C     If not reordering cables by sector (i.e. special test) insert:
C      LPRBRD = IPRBRD
C
C     LADC and LBLS will be finished at the end. They are calculated
C     from LPRBRD.
C
C     Default mapping is the identity mapping for tower and depth
C
      LROTOW = IROTOW
      LDEPTH = IDEPTH
C
C
C
C
C     Get ready for merge analysis
C
      IPRMOD = MOD( IPRBRD, 6 )         ! patterns repeats every 6 boards
C
C
C       -------------------- CC EXCEPTIONS --------------------------
C
      IF ( IBOX.EQ.2 .OR. IBOX.EQ.3 .OR. IBOX.EQ.8 .OR. IBOX.EQ.9 ) THEN
C
C       Find eta 7 to 11 with missing channels and massless gaps
C       Mark missing channels with ICOND = 1 from the array CCEXCP
C
        IF( IPRMOD .GE. 3 ) THEN                        ! goes to merge boards
          ICOND = CCEXCP( IDEPTH, IROTOW/2, IPRMOD )
          IF( ICOND .EQ. 3 ) THEN                       ! massless gap
C
C           For massless gaps find the DEPTH, READ_OUT_TOWER in the
C           LCRATE and the recalculate LPRBRD.
C
            LDEPTH = CCMGAP( IROTOW, IPRMOD )           ! depth in MG BLS
            LROTOW = 3                                  ! even eta tower or
            IF( IROTOW.GE. 2) LROTOW = 1                ! odd eta tower
C
C           Use the LPRBRD to find which of the 2 EC crates we need.
C
            LCRATE = MGADC(IBOX,LPRBRD/48)              ! crossover EC crate
            ISECT  = LPRBRD / 6                         ! sector number mod 16
C
C             the 16 sectors of CC go to two different EC crates
C             thus, we need to use sector number mod 8
C             The massless gap card is the last card of every 12 BLS cards
C
            LPRBRD = 12 * MOD( ISECT, 8) + 11           ! massless gap BLS 11
C
            ICOND = 0                                   ! declare good
C
          ENDIF
        ELSE                            ! still goes through merge execpt:
          IF( IDEPTH.EQ.11 ) ICOND = 1  ! does not get through merge.
        ENDIF
C
C
C       -------------------- EC EXCEPTIONS --------------------------
C
      ELSEIF( IBOX .LT. 12 ) THEN               ! the remaining preamp boxes
C
C       Preamp boards 0-47 handle low eta.  (D0 note 744 - page 26)
C
        LPRBRD = LPRBRD - 1                     ! we drop eta 7-11 card
C
        IF( IPRBRD .LT. 48 ) THEN               ! left bank of EC feedthru
C
C         This is the left bank of connectors on the EC feedthru. Starting
C         from the first (0=1st) preamp card, every sixth card will have
C         signals that go to the merge board. After the merge some of these
C         signals will come back to the EC signal stream inorder to reach
C         a massless gap BLS board. Other signals will go to CC BLSs depending
C         on their eta.
C
          IF( IPRMOD .EQ. 0 ) THEN              ! eta 7-11 goes to merge board
            LROTOW = LECMRG(1, IDEPTH, IROTOW)  ! read out tower on merge BLS
            LDEPTH = LECMRG(2, IDEPTH, IROTOW)  ! depth on merge BLS
            IF( LECMRG(3, IDEPTH, IROTOW) .LT. 0 ) THEN
              ICOND = 1                         ! goes nowhere
            ELSEIF( LECMRG(3, IDEPTH, IROTOW) .EQ. 11 ) THEN
              LPRBRD = LPRBRD + 12              ! massless gap card
            ELSE
              LCRATE = MGADC(IBOX,0)            ! to CC adc crate
              LPRBRD = LPRBRD + 1               ! restore to proper order
              ISECT  = LPRBRD / 12              ! yields sector number mod 8
C
C             Find LPRBRD in CC stream.
C
              LPRBRD = 6 * ISECT + LECMRG(3, IDEPTH, IROTOW)
C
C             Two EC crates go to one CC crate (LCRATE). One mixes with
C             the signals from preamp boards 0-47. The other mixes in with
C             signals from preamp boards 48-95.
C
              JBOX = MOD(IBOX,6)
              IF( JBOX .EQ. 0  .OR.  JBOX .EQ. 4 ) LPRBRD=LPRBRD+48
            ENDIF
          ENDIF
C
          IF( IPRMOD .EQ. 1 ) THEN
            IF( IDEPTH .LT. 6 .AND. IROTOW.LE.1 ) ICOND = 1
            IF( IDEPTH .LT. 3 .AND. IROTOW.GE.2 ) ICOND = 1
          ELSEIF( IPRMOD .EQ. 3 ) THEN
            IF( IDEPTH .EQ. 9 .AND. IROTOW.GE.2 ) ICOND = 1
          ELSEIF( IPRMOD .EQ. 4 ) THEN
            IF( IDEPTH .EQ. 10 .AND. IROTOW.LE.1 ) ICOND = 1
          ENDIF
C
C         On the right bank of the EC feedthru a single cable will have
C         signals for eta44. These are sent to special BLS.
C
        ELSE
          IF( IPRMOD .GE. 2 .AND.  IPRMOD .LE. 4 ) THEN
            IF( IDEPTH .GE. 3 .AND. IDEPTH .LE. 5 ) ICOND = 1
          ELSEIF( IPRMOD.EQ.5 .AND. IROTOW.NE.3 ) THEN
            IF( IDEPTH .GE. 4 .AND. IDEPTH .LE. 6 ) ICOND = 1
          ELSEIF( IPRMOD.EQ.5 .AND. IROTOW.EQ.3 ) THEN
            IF( IDEPTH.LT.5 .AND. IDEPTH.GT.1 ) THEN   ! eliminated pads 5-89
              LROTOW = 0
              LDEPTH = IDEPTH + 7                   ! shift to hadronic position
              LPRBRD = LPRBRD + 1                   ! massless gap card
            ELSEIF( IDEPTH.LT.7 ) THEN
              ICOND = 1
            ENDIF
          ENDIF
        ENDIF
C
      ENDIF
C
C       -------------------- ICD --------------------------
C
      IF (LICD) THEN
        LROTOW=2
        LDEPTH = 2*IDEPTH-2 + MOD(IPRBRD,2)
      ENDIF
C
C       --------------- ADC BLS MAPPING --------------------------
C
        LADC = LPRBRD/8
        LBLS = MOD( LPRBRD, 8)
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
C       -------------------- DONE --------------------------
C
  999 RETURN
      END
