      SUBROUTINE CPPAD(IBOX,IPPAT,NCMAX,NCELL,LCRATE,LADC,LBLS,LROTOW
     &,LDEPTH,IPLIST,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To compute a list of channels pulsed.
C-      The output is in ADC labeling scheme (i.e. CABRD)
C-
C-   Inputs  : IBOX preamp box - D0 detector labelled 0 to 11
C-             IPPAT pulser pattern number 0 - 31
C-             NCMAX maximun size of array provided by caller
C-   Outputs : NCELL number of array elements filled for the following:
C-             LCRATE list of adc crates - i.e. crate coordinate
C-             LADC   list of adc cards - i.e. adc coordinate
C-             LBLS   list of bls cards - i.e. bls coordinate
C-             LROTOW list of read out towers on bls - i.e. tower coordinate
C-             LDEPTH list of depths - i.e. depth coordinate
C-   Controls: IPLIST input: type of list as defined in CAL_PULSE_LIST.PARAMS
C-             ICOND output: 0 = valid
C-              5  = error from CPRAD, therefore, list is not complete
C-              10 = error from or no entries from a call to CPPPR.
C-              15 = no valid entries left after merge.
C-              20 = input array is not large enough.
C-
C-   Created  23-NOV-1988   James Kourlas  NYUHEP::KOURLAS
C-   Updated  11-MAR-1992   Joan Guida  for ICD
C-   Updated  24-MAR-1992   Joan Guida fixed for PP_ALL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_PULSE_LIST.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_PREAMP_NO.PARAMS'
      INCLUDE 'D0$INC:ICD.INC'
      INTEGER IBOX, IPPAT, NCMAX, NCELL, ICOND, IPLIST, ICONDR
C     Declare output list of channels. Each channel is described by
C     the five numbers needed for the ADC labeling scheme:
      INTEGER LCRATE(NCMAX), LADC(NCMAX), LBLS(NCMAX), LROTOW(NCMAX)
      INTEGER LDEPTH(NCMAX)
C     A call is made to get the pattern prior to merge. Variables needed:
      INTEGER NCMAXP,NCMAXPICD
      PARAMETER (NCMAXP=144)
      PARAMETER (NCMAXPICD=156)
      INTEGER MPRBRD(NCMAXPICD),MROTOW(NCMAXPICD),MDEPTH(NCMAXPICD)
      INTEGER NCELLR
      INTEGER I, J, MBOX, MBOX1
      LOGICAL LCC
C----------------------------------------------------------------------
C
      LICD = .FALSE.
      ICOND = 20
      IF( NCMAX .LT. NCMAXP ) GOTO 999          ! input array is too small
      I = NCMAXP * NPRBOX + 24
      IF( IPLIST .EQ. PP_ALL .AND. NCMAX .LT. I ) GOTO 999
C
C     Get the pulser pattern in terms of PREAMP box labeling scheme:
      CALL CPPPR(IPPAT,NCMAXP,NCELLR,MPRBRD,MROTOW,MDEPTH,ICONDR)
C
      ICOND = 10       ! errors from CPPPR should never happen. For protection:
      IF( ICONDR .NE. 0  .OR.  NCELLR .LE. 0  ) GOTO 999
C
C     Convert each entry to ADC labeling scheme:
C
C     Init:
      ICOND = 0
      J = 1
C
C     First, for all cases that require output for only one preamp box:
      IF( IPLIST .NE. PP_ALL  .AND.  IPLIST .NE. PP_QUART ) THEN
        MBOX = IBOX
C
        IF( IPLIST .GE. 12 ) MBOX=IPLIST
C
C       look up each channel on the pulse list and translate
        DO 140 I = 1, NCELLR
          CALL CPRAD(MBOX,MPRBRD(I),MROTOW(I),MDEPTH(I)
     &      ,LCRATE(J),LADC(J),LBLS(J),LROTOW(J),LDEPTH(J),ICONDR)
          IF( ICONDR .EQ. 0 ) J = J + 1         !count real channels
C         Check if list is complete
          IF( ICONDR .GE. 2 ) ICOND = 5
  140   CONTINUE
C
C     for all preamp boxes in the detector:
      ELSEIF( IPLIST .EQ. PP_ALL ) THEN
C
        CALL CPPPR_ICD(IPPAT,NCMAXPICD,NCELLR,MPRBRD,MROTOW,MDEPTH,
     &    ICONDR)
C
C       look up each channel on the pulse list and translate
        LICD=.FALSE.
        DO 240 MBOX=0,NPRBOX-1                            ! NO ICD
          DO 240 I = 1,NCMAXP
            CALL CPRAD(MBOX,MPRBRD(I),MROTOW(I),MDEPTH(I)
     &          ,LCRATE(J),LADC(J),LBLS(J),LROTOW(J),LDEPTH(J),ICONDR)
            IF( ICONDR .EQ. 0 ) J = J + 1       !count real channels
C           Check if list is complete
            IF( ICONDR .GE. 2 ) ICOND = 5
  240   CONTINUE
C
        DO 280 MBOX=NPRBOX,NPRBOX+1                        ! ICD ONLY
          LICD= MBOX.EQ.12 .OR. MBOX.EQ.13
          DO 280 I = NCMAXP+1,NCMAXPICD
            CALL CPRAD(MBOX,MPRBRD(I),MROTOW(I),MDEPTH(I)
     &          ,LCRATE(J),LADC(J),LBLS(J),LROTOW(J),LDEPTH(J),ICONDR)
            IF( ICONDR .EQ. 0 ) J = J + 1       !count real channels
C           Check if list is complete
            IF( ICONDR .GE. 2 ) ICOND = 5
  280   CONTINUE
C
C     for a quadrant of the detector:
      ELSEIF( IPLIST .EQ. PP_QUART ) THEN
C
        LCC = IBOX.EQ.2 .OR. IBOX.EQ.3 .OR. IBOX.EQ.8 .OR. IBOX.EQ.9
        IF (.NOT.LCC) THEN
          CALL CPPPR_ICD(IPPAT,NCMAXPICD, NCELLR,MPRBRD,MROTOW,MDEPTH,
     &        ICONDR)
        ENDIF
C
C       look up each channel on the pulse list and translate
        MBOX1 = IBOX - MOD(IBOX,3)
        DO 340 MBOX=MBOX1,MBOX1+2
          DO 340 I = 1, NCMAXP
            CALL CPRAD(MBOX,MPRBRD(I),MROTOW(I),MDEPTH(I)
     &          ,LCRATE(J),LADC(J),LBLS(J),LROTOW(J),LDEPTH(J),ICONDR)
            IF( ICONDR .EQ. 0 ) J = J + 1       !count real channels
C           Check if list is complete
            IF( ICONDR .GE. 2 ) ICOND = 5
  340   CONTINUE
C
        IF (.NOT.LCC) THEN
          LICD = .TRUE.
          MBOX=13                                       !ICDS
          IF (IBOX.LT.2 .OR. IBOX.GT.9) MBOX=12         !ICDN
          DO 440 I = NCMAXP+1,NCMAXPICD
            CALL CPRAD(MBOX,MPRBRD(I),MROTOW(I),MDEPTH(I)
     &          ,LCRATE(J),LADC(J),LBLS(J),LROTOW(J),LDEPTH(J),ICONDR)
            IF( ICONDR .EQ. 0 ) J = J + 1       !count real channels
C           Check if list is complete
            IF( ICONDR .GE. 2 ) ICOND = 5
  440     CONTINUE
          LICD = .FALSE.
        ENDIF
C
      ENDIF
C
C
      NCELL = J  - 1
C
      IF( NCELL .LE. 0 ) ICOND = 15     ! flag if no entries on list
C
  999 RETURN
      END
