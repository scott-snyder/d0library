      SUBROUTINE L2_CDEL(ROAD,N_INNER,N_OUTER,NUM_DEL,ADRESS,LENGTH,
     &  POINTR,L2CD,TRGR,RESULT_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpk delay lines LAYLST(N_xxxxx),SECLST(N_xxxxx)
C-                         and check for hits within z-road
C-
C-   Inputs  : ROAD
C-             N_INNER,N_OUTER positions within SECLST
C-             NUM_DEL ( LAYER,  WIRE  ) = number of hits within PHI road
C-             ADRESS( LAYER,  WIRE  , pulse# ) = HITADR of hit in PHI road
C-             LENGTH( LAYER,  WIRE  , pulse# ) = HITLEN
C-             POINTR( LAYER,  WIRE  , pulse# ) = POINT to data
C-                      0:inner 0:inside
C-                      1:outer 1:outside
C-   Outputs : RETURN_FLAG
C-   Controls:
C-
C-   Created  23-OCT-1991   D Claes
C-            18-FEB-1992   Further constricting road in z
C-            26-FEB-1992   Require BOTH sides of delay line within road
C-            22-SEP-1991   Final road constriction UP from bottom Z hit
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:CDCMAP.INC'
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
C----------------------------------------------------------------------
C
      INTEGER ADRESS(0:1,0:1,5)! LAYER, WIRE, pulse#     !  Delay Line Indices
      INTEGER BLYR,BWIR,CHNL,DEL,DELW,DPEDS,DPULSE,DWIRE ! I
      INTEGER END, ERR, HITLEN, HITADR                   ! N   BTM    0    L
      INTEGER I, IHIT, ILYR, ILAYER, IOPT, IPULS, IPULS2 ! N          1    R
      INTEGER ISCTR,IW,IWIR,IWIRE,JPEDS,JWIRE,J,JJ,K,KK  ! E   TOP    2    L
      INTEGER LENGTH(0:1,0:1,5)! LAYER, WIRE, pulse#     ! R          3    R
      INTEGER LABEL,LCHA,L2CD,LDPDL,LDTMD,LDTMW,LO_lmt   !  -------------
      INTEGER MASK16, N_DELAY, N_INNER                   ! O
      INTEGER NDHIT, Nhit(0:7), NHIT2(4), NLYR, N_OUTER  ! U   BTM    4    L
      INTEGER NDEL, NPULSE, NUM_DEL(0:1,0:1), POINT      ! T          5    R
      INTEGER POINTR(0:1,0:1,5)! LAYER, WIRE, pulse#     ! E   TOP    6    L
      INTEGER TLYR, TWIR, UP_LMT, WID, WINDX, WINDX2     ! R          7    R
      INTEGER WINDX3, WINDX4, ZCOUNT
C
      LOGICAL RESULT_FLAG
      LOGICAL TEST
C
      REAL BIN_TIME, B_max, B_min, Bb_max, Bb_min, BEG_PULS
      REAL DELLST(5), DEL_MAX, DEL_MIN, ETA, END_PULS
      REAL HITLST(5), L, m_max, M_min, mb_max, Mb_min, MAX, MIN
      REAL PHI, R, R_0, R_b, R_em3, RHIT(4)
      REAL ROAD(1:6,0:6,0:3) ! MAX:MIN,WIRE,LAYER roads
      REAL ROAD_MAX(0:1), ROAD_MIN(0:1), SGN, TDRIFT, TEMP
      REAL THETA, THETA_MIN, THETA_MAX, TRGR, TZCORR, TZERO
      REAL VELOCITY, WETA
      REAL Z1, Z2, Z_EM3_MAX, Z_EM3_MIN, Z_max, Z_min, Zavg
      REAL ZBavg, ZB_max, ZB_min, ZHIT(0:7,10), ZHIT2(4,10)
C
C     PARAMETER (MASK16 = 'FFFF'X)
      PARAMETER (MASK16 = 65535)
C
C----------------------------------------------------------------------
      REAL DLTZR1, DLTZR2,  WZ, ZTOL
      COMMON /DELAYS/ DLTZR1, DLTZR2, WZ, ZTOL
      COMMON /CDCROAD/ ETA, WETA, PHI, R_em3
C----------------------------------------------------------------------
      DATA TEST /.FALSE./           ! Do not skip road constrictions
C      DATA TEST /.TRUE./           ! Skip out after first hit counting pass
C----------------------------------------------------------------------
C
      BIN_TIME = 1000./106.
C
      N_DELAY = 0                       ! Count delay lines seeing hits
      ILAYER = 1
      ILYR  = LAYLST(N_OUTER)           ! Start with OUTER most layer
      ISCTR = SECLST(N_OUTER)
      NLYR  = N_OUTER
C
      DO I = 0,7
        NHIT(I) = 0
      ENDDO
C                                         ! Re-time hits on outer SW
  100 CONTINUE                            ! wires with full FADC unpacking
C
      LDTMW = LC(L2DTMH - 1 - ILYR)       ! Pointer to this layer's wire info
C
      LDPDL = LC(L2DPDH - 1 - ILYR)
      DO IWIRE = 0,1                      ! Loop over TOP/BTM SWs and perform
C
        JWIRE = LDTMW + (ISCTR*IC(LDTMW+4) + IWIRE*6)*IC(LDTMW+3) + 4
        JPEDS = LDPDL + (ISCTR*IC(LDPDL+4) + IWIRE*6)*IC(LDPDL+3) + 4
        IPED =  NINT( C(JPEDS+1) )
C
        NDEL = NUM_DEL(ILAYER,IWIRE)      ! Full FADC-unpacking-hitfinding
        IF (NDEL.EQ.0) THEN
          GOTO 800                        ! Try the next delay line (or layer)
        ENDIF
        NPULSE = 0
        DO IPULS = 1, NDEL                ! Loop over all SW clusters in road
          HITADR = ADRESS(ILAYER,IWIRE,IPULS)
          HITLEN = LENGTH(ILAYER,IWIRE,IPULS)
          POINT  = POINTR(ILAYER,IWIRE,IPULS)
C                                                              ! Returns timing
          CALL L2_HITFIND(1,HITADR,HITLEN,POINT,NPULSE,HITLST) ! of hits on the
C                                                              ! Sense Wire
          IF (NPULSE .GT. TOO_MANY) THEN
            GOTO 900                      ! Accept CDC z road as hit
          ENDIF
        ENDDO                             !(Loop over all SW clusters in road)
C
        NDHIT = 0                         ! A '1' will mark delay line as hit
C
        IF (NPULSE .GT. 0 ) THEN
          DO IHIT = 1,NPULSE              ! Loop over all SW hits in cluster
            TDRIFT = HITLST(IHIT) - C(JWIRE+1)  ! Subtract out Tzero
C                  ! Any trigger time or overall offset will cancel in
C                  ! DEL_MAX/MIN calculation below
C
C                                North (SW) side (-z side)
C                                         !   !
C Delay lines are denoted by wire numbers 7 8 9 10
C 7/8 - Delay line 1 (near SW0)             !    !
C 9/10- Delay line 2 (near SW6)      South (HV) side (+z side)
C
            IF (MC_FLAG) THEN             ! N/S readout reversed in MC
              SGN = -1
            ELSE
              SGN = 1
            ENDIF
            DO DELW = 7,8                 ! Read out left/right delay wires
              DEL = DELW + IWIRE*2
C
              CALL DCODER(LABEL,ILYR,ISCTR,DEL,0,2)   ! Encodes and outputs
C                                                     ! CDC channel address
              POINT = MAP(LABEL)                  ! Pointer to channel data
              LCHA = IAND(IQ(POINT), MASK16)
              IF (LCHA.LE.4) GO TO 500    ! Try other end or next delay line
C
              CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16)
              IF (CHNL.NE.LABEL) GO TO 500      ! Neccessary check since
              END = POINT - LCHA/4 + 1          ! MAP is never re-zeroed
C
              LDTMD = LC(L2DTMH - 5 - ILYR)     ! Pointer to this layer's
              IW = DEL - 7                      ! DELAY Line information
              IWIR = DELW-7
              WINDX = IW + ILAYER*4
              DWIRE = LDTMD + (ISCTR*IC(LDTMD+4) + IW)*IC(LDTMD+3) + 4
              DPEDS = LDPDL + (ISCTR*IC(LDPDL+4) +DEL)*IC(LDPDL+3) + 4
C
C       Now    VELOCITY = C(DWIRE+2)   and   Tzero = C(DWIRE+1)
C       To change delay distance to delay time use the relation
C              DISTANCE = (time - Tzero)*DELAY_VELOCITY
C       Delay velocities are set +/- for averaging both sides to get z
C       For LEVEL2 we will just check each side separately.
              VELOCITY = ABS( C(DWIRE+2) )
              TZERO    = C(DWIRE+1)
              IPED = NINT( C(DPEDS + 1) )
C
              IF (.NOT.MC_FLAG) THEN
                IF (DELW.EQ.7) THEN
                  TZERO = TZERO + DLTZR1
                ELSE
                  TZERO = TZERO + DLTZR2
                ENDIF
              ENDIF
C
C Guido informs me Tzeros are set w.r.t. z=0 centers of the delay lines
C
              IF (SGN.GT.0) THEN
                DEL_MIN = TDRIFT+(ROAD(5,IWIRE*6,ILYR))/VELOCITY + TZERO
                DEL_MAX = TDRIFT+(ROAD(6,IWIRE*6,ILYR))/VELOCITY + TZERO
              ELSE
                DEL_MIN = TDRIFT-(ROAD(6,IWIRE*6,ILYR))/VELOCITY + TZERO
                DEL_MAX = TDRIFT-(ROAD(5,IWIRE*6,ILYR))/VELOCITY + TZERO
              ENDIF
              SGN = -1*SGN
C                                                          ! Look at all hit
              DO WHILE (POINT.GT.END.AND.NHIT(WINDX).LT.5) ! clusters on this
C                                                          ! side of the delay
                POINT = POINT - 1               ! Bottom of NEXT hit's data
                HITLEN = IAND(IQ(POINT), MASK16)
                HITADR = IAND(ISHFT(IQ(POINT), -16), MASK16)! Timing bin
C                                                           ! of last datum
                IF (HITADR.GT.500) GOTO 500         ! Bad HITADR read out
C
                IF (TRGFLG) THEN
                  END_PULS = (HITADR * BIN_TIME)
                  BEG_PULS = ((HITADR-HITLEN+5)*BIN_TIME)
                ELSE                                    ! MC uses no offset
                  END_PULS = HITADR * BIN_TIME
                  BEG_PULS = (HITADR - HITLEN + 5) * BIN_TIME
                ENDIF
C
C       Check IF timing falls within ROAD
C       If NOT, skip to NEXT pulse!
C
                IF (END_PULS .LT. DEL_MIN) GOTO 500 ! Pulse comes too early
                IF (BEG_PULS .GT. DEL_MAX) GOTO 300 ! Pulse comes too late
C
C Enforce full FADC-unpacking-hitfinding for each delay line cluster in road
C
                DPULSE = 0
                CALL L2_HITFIND(2,HITADR,HITLEN,POINT,DPULSE,DELLST)
C                         NOTE: Will return POINT=POINT-(HITLEN/4)+1
C       Check IF leading edge of pulse falls within ROAD
C       If NOT, skip to NEXT pulse!
C
                IF (DPULSE.EQ.0) GOTO 400
                DO IPULS = 1, DPULSE                ! Loop over hits in cluster
C
                  BEG_PULS = DELLST(IPULS)
                  IF (BEG_PULS .LT. DEL_MIN) GOTO 200 ! Pulse comes too early
                  IF (BEG_PULS .GT. DEL_MAX) GOTO 200 ! Pulse comes too late
C
                  NHIT(WINDX) = NHIT(WINDX) + 1
                  IF (NHIT(WINDX).GT.10) NHIT(WINDX) = 10
                  ZHIT(WINDX,Nhit(WINDX))=(BEG_PULS-TDRIFT
     &              -C(DWIRE+1))*C(DWIRE+2)
C
                  NDHIT = 1
C
  200             CONTINUE
                ENDDO                     ! Next hit in this delay cluster
                GOTO 400        ! L2_HITFIND already reset POINT
  300           CONTINUE
                POINT = POINT - (HITLEN/4) + 1
  400           CONTINUE
              ENDDO             ! Next cluster on this delay line side
  500         CONTINUE
            ENDDO               ! Loop over left/right sides of delay lines
C
          ENDDO                 ! Loop over all hit clusters on driving SW
        ENDIF
        N_DELAY = N_DELAY + NDHIT
  800   CONTINUE
      ENDDO                     ! Next Delay Line (BOTTOM/TOP)
C
      DO I = 0,3
        IQ( L2CD + NTOP_L2CD + (NLYR-1)*NREP_L2CD + (6+I))
     &    = NHIT( ILAYER*4 + I )
      ENDDO
C
      IF (N_DELAY .LT. 1) GOTO 999          ! No delay hits in road thus far
C
      IF (ILAYER.EQ.1) THEN
        ILAYER = 0
        ILYR  = LAYLST(N_INNER)
        ISCTR = SECLST(N_INNER)
        NLYR  = N_INNER
        GOTO 100
      ENDIF
C
      IF (N_DELAY .LT. MIN_DEL) GOTO 999    ! No delay hits in road
C
C Possible hit along Z.  Reduce road and hit count again.
C
      N_DELAY = 0                           ! Reset delay line counter
C
      THETA_min = 2*ATAN(EXP(-1*(ETA-WETA)))
      THETA_max = 2*ATAN(EXP(-1*(ETA+WETA)))
      Z_EM3_min = R_em3/(TAN(THETA_min))
      Z_EM3_max = R_em3/(TAN(THETA_max))
C
      ILAYER = 1        ! Use only the OUTER most layer
      ILYR = LAYLST(N_OUTER)
      DO IWIRE = 1,0,-1    ! Loop over BOTTOM/TOP Delay lines
        R_0 = RADIUS(IWIRE*6, ILYR)
        IWIR = 0        !C*
        WINDX = ILAYER*4 + IWIRE*2 + IWIR   ! Look at both LEFT and RIGHT
        WINDX2 = WINDX + 1                  ! readout sides of delay line
        DO IPULS = 1, NHIT(WINDX)   ! Loop over all hits found in ROAD
          DO IPULS2 = 1, NHIT(WINDX2)
            IF (ABS(ZHIT(WINDX2,IPULS2)-ZHIT(WINDX,IPULS)).LT.ZTOL)
     &        THEN
              Zavg = (ZHIT(WINDX2,IPULS2)+ZHIT(WINDX,IPULS))/2.
C
              ZHIT2(1,1) = Zavg
              RHIT(1) = R_0
              NHIT2(1) = 1
              DO J=2,4
                NHIT2(J)=0
              ENDDO
C
              Z_min = Zavg - WZ        ! Need to add THETA, WZ
              Z_max = Zavg + WZ        ! to some COMMON block
C
C           Define a narrower road using EM3 tower and this hit
C           where max/min z edges given by z = m*R _ b
C
              m_max = (Z_EM3_min - Z_max)/(R_em3 - R_0)
              m_min = (Z_EM3_max - Z_min)/(R_em3 - R_0)
              B_max = Z_max - m_max*R_0
              B_min = Z_min - m_min*R_0
C
              N_DELAY = 1 ! Start counting with the hit used to define this road
              IF (IWIRE.EQ.1) THEN  ! Check the bottom delay of this outer layer
                R = RADIUS(0,ILYR)
                MIN = M_min*R + B_min - WZ
                TEMP = ROAD(5,0,ILYR) - WZ
                IF (TEMP.GT.MIN) MIN = TEMP
                MAX = M_max*R + B_max + WZ
                TEMP = ROAD(6,0,ILYR) + WZ
                IF (TEMP.LT.MAX) MAX = TEMP
C
                NDHIT = 0 ! Will count for hits seen BOTH from L and R
C
C
                DO J=0,1  ! Loop over LEFT/RIGHT readout of bottom DELAY line
                  WID = 4 + J
                  DO K = 1, NHIT(WID)
                    IF (ZHIT(WID,K).LT.MAX .AND. ZHIT(WID,K).GT.MIN)
     &                THEN
C                    NDHIT = NDHIT + 1     !C*
C                    IF (NDHIT.GE.2) THEN  !C*
                      N_DELAY = N_DELAY + 1
                      GOTO 820
C                    ENDIF                 !C*
C                    GOTO 810              !C*
                    ENDIF
                  ENDDO
  810             CONTINUE
                ENDDO
C
              ENDIF       ! If hits start in TOP delay line of OUTER layer
  820         CONTINUE
C
              DO I = 0,1
                ROAD_MIN(I) = 999.0
                ROAD_MAX(I) = 0.000
              ENDDO
C
              DO I = 0,1  ! Loop over TOP/BOTTOM of INNER layer
                R = RADIUS(I*6,LAYLST(N_INNER))
                MIN = M_min*R + B_min - WZ
                TEMP = ROAD(5,I*6,LAYLST(N_INNER)) - WZ
                IF (TEMP.GT.MIN) MIN = TEMP
                MAX = M_max*R + B_max + WZ
                TEMP = ROAD(6,I*6,LAYLST(N_INNER)) + WZ
                IF (TEMP.LT.MAX) MAX = TEMP
C
                ROAD_MIN(I) = MIN
                ROAD_MAX(I) = MAX
C
                NDHIT = 0 ! Will count for hits seen BOTH from L and R
                DO J=0,1  ! Loop over the LEFT/RIGHT readout
                  WID = I*2 + J
                  DO K = 1,  NHIT(WID)
                    IF (ZHIT(WID,K).LT.MAX .AND. ZHIT(WID,K).GT.MIN)
     &                THEN
C                    NDHIT = NDHIT + 1
C                    IF (NDHIT.GE.2) THEN
                      N_DELAY = N_DELAY + 1
                      GOTO 850
C                    ENDIF
C                    GOTO 840
                    ENDIF
                  ENDDO
  840             CONTINUE
                ENDDO
  850           CONTINUE
              ENDDO
C
              IF (TEST) GOTO 875                 ! Testing algorithm additions
C
              IF (N_DELAY.GE.MIN_DEL) THEN       ! Final road constriction
C                                            ! built up from below
                BLYR = LAYLST(N_INNER)           ! Use hits in innermost layer
                N_DELAY = 1
C
C                                            ! If IWIRE=0 MUST use ONLY
                DO BWIR = 0, IWIRE               ! bottom most delay line to
                  R_b = RADIUS(BWIR*6,BLYR)      ! count 3/4 delay lines hit
                  WINDX3 = BWIR*2
                  WINDX4 = WINDX3 + 1
                  DO I = 1, NHIT(WINDX3)
                    DO J = 1, NHIT(WINDX4)
                      IF (ABS(ZHIT(WINDX3,I)-ZHIT(WINDX4,J)).LT.ZTOL)
     &                  THEN
                        ZBavg = (ZHIT(WINDX3,I)+ZHIT(WINDX4,J))/2.
C
                        IF (ZBavg.GT.ROAD_MAX(BWIR) .OR.
     &                    ZBavg.LT.ROAD_MIN(BWIR)) GOTO 872
C
                        ZB_min = ZBavg - WZ
                        ZB_max = ZBavg + WZ
C
                        mb_max = (Z_EM3_min - ZB_max)/(R_em3 - R_b)
                        mb_min = (Z_EM3_min - ZB_min)/(R_em3 - R_b)
                        Bb_max = Zb_max - mb_max*R_b
                        Bb_min = Zb_min - mb_min*R_b
C
                        LO_lmt = 2 + 2*BWIR
                        UP_lmt = 2 + 2*IWIRE
C                                               ! Loop over interverning
                        N_DELAY = 2
C
                        DO K = LO_lmt, UP_lmt, 2    ! DELAY Lines
                          IF (K.EQ.2) THEN
                            R = RADIUS(6,BLYR)
                            Twir = 6
                            Tlyr = BLYR
                          ELSE
                            R = RADIUS(0,ILYR)
                            Twir = 0
                            Tlyr = ILYR
                          ENDIF
C
                          MIN = Mb_min*R + Bb_min - WZ
                          TEMP = ROAD(5,Twir,TLYR) - WZ
                          IF (TEMP.GT.MIN) MIN = TEMP
                          TEMP = M_min*R + B_min - WZ
                          IF (TEMP.GT.MIN) MIN = TEMP
C
                          MAX = Mb_max*R + Bb_max + WZ
                          TEMP = ROAD(6,Twir,TLYR) + WZ
                          IF (TEMP.LT.MAX) MAX = TEMP
                          TEMP = M_max*R + B_max + WZ
                          IF (TEMP.LT.MAX) MAX = TEMP
C
c                      NDHIT = 0 ! Will count for hits seen BOTH from L and R
C
                          DO JJ = 0,1 ! Loop over LEFT/RIGHT of DELAY lines
                            WID = K + JJ
                            DO KK = 1, NHIT(WID)
                              IF (ZHIT(WID,KK).LT.MAX .AND.
     &                          ZHIT(WID,KK).GT.MIN) THEN
                                N_DELAY = N_DELAY + 1
                                GOTO 870
                              ENDIF
                            ENDDO
  860                       CONTINUE
                          ENDDO
  870                     CONTINUE
                        ENDDO       ! Loop over intervening delay lines
                      ENDIF         ! If BTM delay line sees hits/both sides
  872                 CONTINUE
                    ENDDO     ! Loop over both sides of
                  ENDDO       ! BTM most delay line
C
                ENDDO
C
              ENDIF ! Intermediate check of N_DELAY >= MIN_DEL
C
  875         CONTINUE
              IF (N_DELAY.GE.MIN_DEL) GOTO 900 !Track found -> Jump out of loop
C
            ENDIF !If L/R pulses within ZTOL of each other
          ENDDO !Looping over hits from OPPOSITE side of delay
        ENDDO   ! Next hit on OUTER Delay Line within original ROAD
C
  880 CONTINUE
C
      ENDDO       ! Loop over TOP/BOTTOM Delay lines of OUTER most layer
C
C     Found no hit in z
C
      GOTO 999
C
C     Found hit in z -> TRACK IDENTIFIED!
C
  900 CONTINUE
      RESULT_FLAG = .TRUE.
  999 CONTINUE
      RETURN
      END
