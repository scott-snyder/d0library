      SUBROUTINE MUMSET_L2(NMOD,JMOD,NQUAD,JQUAD,NWAMSAM,SWAMSAM)
C----------------------------------------------------------------------
C-
C    TOM DIEHL July 25, 1992
C-
C-   Purpose and Methods : TO SET WHAT MODULES SHOULD HAVE their hits
C        converted from MUD1 to MUOH. also, decides which quadrants
C        should be looped over in tracking. See D0NOTE 1451 for 
C        a concise explanation of the trigger information.
C-
C-   Inputs  : CONTROL(4)
C-   Outputs : NMOD = number of modules
C              JMOD = module list
C              NQUAD = number of quadrants (some kind of Hedin notation)
C              JQUAD = quadrant list
C-
C  I set it up so that the 5 muon trigger sections are controlled 
C  by parts of the TRACK_IN_ROAD parameter. The parameter looks 
C  like: CFXXX_EFXXX_WSXXX_SXXX and the possible strings for XXX are: 
C  'ALL', 'L10', 'L15' or 'XXX' .  CF, EF, WS, and S stand for Central WAMUS,
C  End WAMUS, SAMUS_WAMUS and SAMUS muon triggers.  TRACK_IN_ROAD is 
C  parsed to pieces by MUON_L2_TIR_PARSE from MUON_L2 and passed via entry
C  point MUMSET_L2_PARAMS below.  There is a 24 character
C  limit on lengths of character strings set by COOR (that includes the
C  mandatory '' ).
C
C  WARNING: The event is set for LIFE on the first call to MUON_L2
C           tool at present except for pure SAMUS (see MUON_L2).
C    
C  Modified 08-Dec-1992 Diehl Removed WS PDTS from EF unpacking.
C                             WS controlled by CCTLAT2
C  Modified 04-Jan-1993 Diehl WAMSAM flags to control SAMUS hit unpacking
C                             and track processing
C                             for wamus-samus overlap triggers samus pdt.
C  Modified 25-Apr-1993 Diehl Wamus-Samus Overlap control word 'WSL1A'
C                             allowed for switching on SA-SB-WC tracking
C                             in MUON_L2.
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MUON_L2_PARAMS.INC'
      INCLUDE 'D0$INC:MUON_L2_PARAMS_C.INC'

      CHARACTER*5 CONTROL(4),TEMPCHAR(4)
      INTEGER PARAM_SET_NUMBER,PSN

      LOGICAL NWAMSAM,SWAMSAM
      INTEGER CCTTRACK(5)   

      INTEGER NMOD,JMOD(170),NQUAD,JQUAD(12),MOD,NPDT
      INTEGER IQUAD(12),MUQUAD,JQ(12),J,N,K,I,IERR
      INTEGER LMUOF,GZMUOF,NM,IM,LMUHT,GZMUHT

      INTEGER IREG(6),CRATEWORDS(5),TRIG_NUM(5),CRATE_ID(5),
     $ NOTC_CARDS(5),ACTOTC(5),VERSION(5),L15OTC(5),LONGTO(5),
     $ TABTYP(5),TRIGBITS(5),MERR(5),KERR(5),CCTLAT(5),OTCNUM(5,16),
     $ OTCSTAT(5,16),MGRSTAT(5),NOTCWD(5),KTABLE(5,130,2,4),ITFWD(2),
     $ TRAILWC(5),TRAILCR(5),TRAILTN(5),CCTLAT2(5)          ! GTTRGR2

      DATA JQ/6,5,7,8,10,9,11,12,1,2,3,4/

C                         LEVEL 1 DATA
C The PDTs are listed in rows of octants (CF) and quadrants (EF).
C In case you look close you will find some chambers are in more 
C than one quadrant or octant and that PDTs 250,251,253,255, and 
C 280,281,283,285 are not in the L1 trigger. 
C 
C The L1.0 part of the code isn't optimized for size. This part 
C could be written in 26 lines + data statements if no chambers
C were in more than one quadrant or octant and in something like 50
C lines if the goal was to minimize the size of the code. But the 
C object here is simply to minimize (within reason) the number of 
C commands for the purpose of speed.
C
      INTEGER PDTEFN(8,0:3),PDTEFS(8,0:3),PDTCFO(13,0:7)
      INTEGER PDTWSN(4,0:3),PDTWSS(4,0:3)
      INTEGER NEFQ(0:3),NCFO(0:7),NWSQ(0:3)

      DATA PDTEFN/061,067,160,161,260,261,270,271,
     $            062,064,162,163,262,263,272,273,
     $            064,065,164,165,264,265,274,275,
     $            065,067,166,167,266,267,276,277/
 
      DATA PDTEFS/091,097,190,191,290,291,300,301,
     $            092,094,192,193,292,293,302,303,
     $            094,095,194,195,294,295,304,305,
     $            095,097,196,197,296,297,306,307/
   
      DATA PDTWSN/150,260,261,000,
     $            153,263,262,000,
     $            153,165,264,265,
     $            150,166,267,266/

      DATA PDTWSS/180,290,291,000,
     $            183,293,292,000,
     $            183,195,294,295,
     $            180,196,297,296/

      DATA PDTCFO/010,020,030,100,110,120,130,140,200,210,220,230,240,
     $            011,021,031,101,111,121,131,141,201,211,221,231,241,
     $            012,022,032,102,112,122,132,142,202,212,222,232,242,
     $            013,023,033,103,113,123,133,143,203,213,223,233,243,
     $            013,023,033,104,114,124,134,144,204,214,224,234,244,
     $            015,025,035,105,115,135,145,205,215,235,245,000,000,
     $            016,026,036,106,116,136,146,206,216,236,246,000,000,
     $            010,020,030,107,117,127,137,147,207,217,227,237,247/

      DATA NCFO/5*13,2*11,13/,NEFQ/4*8/,NWSQ/3,3,4,4/
C----------------------------------------------------------------------
C                      Per Event Initialization
C----------------------------------------------------------------------
      NMOD=0             
      DO I=1,12
        IQUAD(I)=0
      ENDDO
C      DO J = 1,170
C        JMOD(J) = 0
C      ENDDO

      IF(TRACK_IN_ROAD(PSN).EQ.'OFFLINE') THEN
        NWAMSAM = .TRUE.
        SWAMSAM = .TRUE.
        LMUHT=GZMUHT(0)
        NMOD=IQ(LMUHT+3)
        LMUOF=GZMUOF(0)
        DO I=1,NMOD
          JMOD(I)=IQ(LMUOF+1+10*(I-1))
        ENDDO
        GOTO 756
      ENDIF

      IF(TRACK_IN_ROAD(PSN).EQ.'FALSE') THEN
        NWAMSAM = .FALSE.
        SWAMSAM = .FALSE.
        LMUHT=GZMUHT(0)
        NM=IQ(LMUHT+3)
        LMUOF=GZMUOF(0)
        DO I=1,NM
          IM=IQ(LMUOF+1+10*(I-1))
          IF(IM.GE.250.AND.IM.LE.255) THEN
            ELSE IF(IM.GE.280.AND.IM.LE.285) THEN
            ELSE IF(IM.GE.180.AND.IM.LE.185) THEN
            ELSE IF(IM.GE.150.AND.IM.LE.155) THEN
            ELSE IF(IM.GE.260.AND.IM.LE.267) THEN
            ELSE IF(IM.GE.290.AND.IM.LE.297) THEN
            ELSE
            NMOD=NMOD+1
            JMOD(NMOD)=IM
            IQUAD(MUQUAD(JMOD(NMOD)))=1
          ENDIF
        ENDDO
      GOTO 756
      ENDIF                         !IF TRACK_IN_ROAD IS FALSE


      CALL GTTRGR2(IREG,CRATEWORDS,TRIG_NUM,CRATE_ID,NOTC_CARDS,
     $  ACTOTC,VERSION,L15OTC,LONGTO,TABTYP,TRIGBITS,MERR,KERR,CCTLAT,
     $  CCTLAT2,OTCNUM,OTCSTAT,MGRSTAT,NOTCWD,KTABLE,ITFWD,TRAILWC,
     $  TRAILCR,TRAILTN)

C----------------------------------------------------------------------
C                CENTRAL MUON TRIGGERS 
C----------------------------------------------------------------------
      IF(CONTROL(1).EQ.'CFXXX') THEN
        CCTTRACK(1) = 0
      ELSEIF(CONTROL(1).EQ.'CFANY') THEN
        CCTTRACK(1) = 1044481            ! HEX FF001
      ELSEIF(CONTROL(1).EQ.'CFALL') THEN
        IF(BTEST(CCTLAT(1),0).OR.BTEST(CCTLAT(1),1)) THEN
          CCTTRACK(1) = 1044481          ! HEX FF001
        ELSE
          CCTTRACK(1) = 0
        ENDIF
      ELSEIF(CONTROL(1).EQ.'CFL10') THEN
        CCTTRACK(1) = CCTLAT(1)
      ELSEIF(CONTROL(1).EQ.'CFL15') THEN
        CCTTRACK(1) = 0
      ENDIF

      IF(BTEST(CCTTRACK(1),0).OR.BTEST(CCTTRACK(1),1)) THEN
        IF(BTEST(CCTTRACK(1),12)) THEN
          DO K = 1,NCFO(0)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,0)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),13)) THEN
          DO K = 1,NCFO(1)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,1)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),14)) THEN
          DO K = 1,NCFO(2)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,2)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),15)) THEN
          DO K = 1,NCFO(3)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,3)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),16)) THEN
          IF(BTEST(CCTTRACK(1),15)) THEN
            DO K = 4,NCFO(4)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTCFO(K,4)
            ENDDO
          ELSE
            DO K = 1,NCFO(4)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTCFO(K,4)
            ENDDO
          ENDIF
        ENDIF
        IF(BTEST(CCTTRACK(1),17)) THEN
          DO K = 1,NCFO(5)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,5)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),18)) THEN
          DO K = 1,NCFO(6)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTCFO(K,6)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(1),19)) THEN
          IF(BTEST(CCTTRACK(1),12)) THEN
            DO K = 4,NCFO(7)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTCFO(K,7)
            ENDDO
          ELSE
            DO K = 1,NCFO(7)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTCFO(K,7)
            ENDDO
          ENDIF
        ENDIF
      ENDIF                                   ! ENDIF for CCT(1,N) Latches  

C----------------------------------------------------------------------
C                North EF and South EF  MUON TRIGGERS SETUP
C----------------------------------------------------------------------
      IF(CONTROL(2).EQ.'EFXXX') THEN
        CCTTRACK(2) = 0
        CCTTRACK(3) = 0
      ELSEIF(CONTROL(2).EQ.'EFANY') THEN
        CCTTRACK(2) = 61441            ! HEX F001
        CCTTRACK(3) = 61441
      ELSEIF(CONTROL(2).EQ.'EFALL') THEN
        IF(BTEST(CCTLAT(2),0).OR.BTEST(CCTLAT(2),1)) THEN
          CCTTRACK(2) = 61441          ! HEX F001
        ELSE
          CCTTRACK(2) = 0
        ENDIF
        IF(BTEST(CCTLAT(3),0).OR.BTEST(CCTLAT(3),1)) THEN
          CCTTRACK(3) = 61441          ! HEX F001
        ELSE
          CCTTRACK(3) = 0
        ENDIF
      ELSEIF(CONTROL(2).EQ.'EFL10') THEN
        CCTTRACK(2) = CCTLAT(2)
        CCTTRACK(3) = CCTLAT(3)
      ELSEIF(CONTROL(1).EQ.'EFL15') THEN
        CCTTRACK(2) = 0
        CCTTRACK(3) = 0
      ENDIF

C----------------------------------------------------------------------
C                North EF MUON TRIGGERS 
C----------------------------------------------------------------------
      IF(BTEST(CCTTRACK(2),0).OR.BTEST(CCTTRACK(2),1)) THEN
        IF(BTEST(CCTTRACK(2),12)) THEN
          DO K = 1,NEFQ(0)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTEFN(K,0)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(2),13)) THEN
          DO K = 1,NEFQ(1)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTEFN(K,1)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(2),14)) THEN
          IF(BTEST(CCTTRACK(2),13)) THEN
            DO K = 1,NEFQ(2)
              IF(PDTEFN(K,2).NE.064.AND.PDTEFN(K,2).NE.153) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFN(K,2)
              ENDIF
            ENDDO
          ELSE 
            DO K = 1,NEFQ(2)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTEFN(K,2)
            ENDDO
          ENDIF
        ENDIF
        IF(BTEST(CCTTRACK(2),15)) THEN
          IF(BTEST(CCTTRACK(2),14).AND..NOT.BTEST(CCTTRACK(2),12))
     $      THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFN(K,3).NE.065) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFN(K,3)
              ENDIF
            ENDDO
          ELSEIF(BTEST(CCTTRACK(2),12).AND..NOT.BTEST(CCTTRACK(2),14))
     $     THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFN(K,3).NE.067.AND.PDTEFN(K,3).NE.150) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFN(K,3)
              ENDIF
            ENDDO
          ELSEIF(BTEST(CCTTRACK(2),12).AND.BTEST(CCTTRACK(2),14))
     $     THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFN(K,3).NE.065.AND.PDTEFN(K,3).NE.067.AND.
     $        PDTEFN(K,3).NE.150) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFN(K,3)
              ENDIF
            ENDDO
          ELSE
            DO K = 1,NEFQ(3)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTEFN(K,3)
            ENDDO
          ENDIF
        ENDIF
      ENDIF                                   !TRIGGER IN EFN
C----------------------------------------------------------------------
C                South EF MUON TRIGGERS WAMUS-ONLY
C----------------------------------------------------------------------
      IF(BTEST(CCTTRACK(3),0).OR.BTEST(CCTTRACK(3),1)) THEN
        IF(BTEST(CCTTRACK(3),12)) THEN
          DO K = 1,NEFQ(0)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTEFS(K,0)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(3),13)) THEN
          DO K = 1,NEFQ(1)
            NMOD = NMOD + 1
            JMOD(NMOD) = PDTEFS(K,1)
          ENDDO
        ENDIF
        IF(BTEST(CCTTRACK(3),14)) THEN
          IF(BTEST(CCTTRACK(3),13)) THEN
            DO K = 1,NEFQ(2)
              IF(PDTEFS(K,2).NE.094.AND.PDTEFS(K,2).NE.183) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFS(K,2)
              ENDIF
            ENDDO
          ELSE 
            DO K = 1,NEFQ(2)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTEFS(K,2)
            ENDDO
          ENDIF
        ENDIF

        IF(BTEST(CCTTRACK(3),15)) THEN
          IF(BTEST(CCTTRACK(3),14).AND..NOT.BTEST(CCTTRACK(3),12))
     $      THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFS(K,3).NE.095) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFS(K,3)
              ENDIF
            ENDDO
          ELSEIF(BTEST(CCTTRACK(3),12).AND..NOT.BTEST(CCTTRACK(3),14))
     $    THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFS(K,3).NE.097.AND.PDTEFS(K,3).NE.180) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFS(K,3)
              ENDIF
            ENDDO
          ELSEIF(BTEST(CCTTRACK(3),12).AND.BTEST(CCTTRACK(3),14)) THEN
            DO K = 1,NEFQ(3)
              IF(PDTEFS(K,3).NE.095.AND.PDTEFS(K,3).NE.097.AND.
     $        PDTEFS(K,3).NE.180) THEN
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTEFS(K,3)
              ENDIF
            ENDDO
          ELSE
            DO K = 1,NEFQ(3)
              NMOD = NMOD + 1
              JMOD(NMOD) = PDTEFS(K,3)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
C                North WAMUS-SAMUS MUON TRIGGERS 
C----------------------------------------------------------------------
      NWAMSAM = .FALSE.
      IF(CONTROL(3).EQ.'WSL10'.OR.CONTROL(3).EQ.'WSL1A') THEN
        IF(BTEST(CCTLAT2(2),0).OR.BTEST(CCTLAT2(2),1)) THEN
          NWAMSAM = .TRUE.
          IF(BTEST(CCTLAT2(2),12)) THEN
            IF(.NOT.BTEST(CCTTRACK(2),12)) THEN
              DO K = 1,NWSQ(0)                  ! 150 260 261
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSN(K,0)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(2),12)) THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 150
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(2),13)) THEN
            IF(.NOT.BTEST(CCTTRACK(2),13)) THEN
              DO K = 1,NWSQ(1)                  ! 153 262 263
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSN(K,1)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(2),13)) THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 153
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(2),14)) THEN
            IF(.NOT.BTEST(CCTTRACK(2),14).AND..NOT.BTEST(CCTLAT2(2),12))
     $        THEN
              DO K = 1,NWSQ(2)                  ! 153 165 264 265
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSN(K,2)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(2),14).AND..NOT.BTEST(CCTLAT2(2),12)) 
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 153
            ELSEIF(.NOT.BTEST(CCTTRACK(2),14).AND.BTEST(CCTLAT2(2),12))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 165
              NMOD = NMOD + 1 
              JMOD(NMOD) = 264
              NMOD = NMOD + 1 
              JMOD(NMOD) = 265
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(2),15)) THEN
            IF(.NOT.BTEST(CCTTRACK(2),15).AND..NOT.BTEST(CCTLAT2(2),13)) 
     $        THEN
              DO K = 1,NWSQ(3)
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSN(K,3)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(2),15).AND..NOT.BTEST(CCTLAT2(2),13))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 150
            ELSEIF(.NOT.BTEST(CCTTRACK(2),15).AND.BTEST(CCTLAT2(2),13))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 166
              NMOD = NMOD + 1 
              JMOD(NMOD) = 267
              NMOD = NMOD + 1 
              JMOD(NMOD) = 266
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
C                South WAMUS-SAMUS MUON TRIGGERS 
C----------------------------------------------------------------------
      SWAMSAM = .FALSE.
      IF(CONTROL(3).EQ.'WSL10'.OR.CONTROL(3).EQ.'WSL1A') THEN
        IF(BTEST(CCTLAT2(3),0).OR.BTEST(CCTLAT2(3),1)) THEN
          SWAMSAM = .TRUE.
          IF(BTEST(CCTLAT2(3),12)) THEN
            IF(.NOT.BTEST(CCTTRACK(3),12)) THEN
              DO K = 1,NWSQ(0)                  ! 180 290 291
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSS(K,0)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(3),12)) THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 180
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(3),13)) THEN
            IF(.NOT.BTEST(CCTTRACK(3),13)) THEN
              DO K = 1,NWSQ(1)                  ! 183 292 293
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSS(K,1)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(3),13)) THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 183
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(3),14)) THEN
            IF(.NOT.BTEST(CCTTRACK(3),14).AND..NOT.BTEST(CCTLAT2(3),12))
     $        THEN
              DO K = 1,NWSQ(2)                  ! 183 195 294 295
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSS(K,2)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(3),14).AND..NOT.BTEST(CCTLAT2(3),12))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 183
            ELSEIF(.NOT.BTEST(CCTTRACK(3),14).AND.BTEST(CCTLAT2(3),12))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 195
              NMOD = NMOD + 1 
              JMOD(NMOD) = 294
              NMOD = NMOD + 1 
              JMOD(NMOD) = 295
            ENDIF
          ENDIF

          IF(BTEST(CCTLAT2(3),15)) THEN
            IF(.NOT.BTEST(CCTTRACK(3),15).AND..NOT.BTEST(CCTLAT2(3),13))
     $        THEN
              DO K = 1,NWSQ(3)
                NMOD = NMOD + 1
                JMOD(NMOD) = PDTWSS(K,3)
              ENDDO
            ELSEIF(BTEST(CCTTRACK(3),15).AND..NOT.BTEST(CCTLAT2(3),13))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 180
            ELSEIF(.NOT.BTEST(CCTTRACK(3),15).AND.BTEST(CCTLAT2(3),13))
     $        THEN
              NMOD = NMOD + 1
              JMOD(NMOD) = 196
              NMOD = NMOD + 1 
              JMOD(NMOD) = 297
              NMOD = NMOD + 1 
              JMOD(NMOD) = 296
          ENDIF
          ENDIF
        ENDIF
      ENDIF     
C----------------------------------------------------------------------
C                PURE SAMUS MUON TRIGGERS 
C   THIS IS CONTROLLED BY SUBROUTINE SAMUS_L2_SIDE
C----------------------------------------------------------------------
C 
C          IF(BTEST(CCTLAT(4),0).OR.BTEST(CCTLAT(4),1).OR.
C     $      BTEST(CCTLAT(4),2)) THEN
C          ENDIF                  !SAMUS
C          IF(BTEST(CCTLAT(5),0).OR.BTEST(CCTLAT(5),1).OR.
C     $      BTEST(CCTLAT(5),2)) THEN
C          ENDIF                  !SAMUS
C----------------------------------------------------------------------
C                           Level 1.5
C----------------------------------------------------------------------
C          IF(TRACK_IN_ROAD(PSN).EQ.'L1.5') THEN 
C            DO I = 1,5         ! This section indicates chambers in trigger.
C            N = NOTCWD(I)
C            DO J = 1,N
C              DO K = 1,3
C                MOD = KTABLE(I,J,1,K)
C                CALL KTAB2MOD(MOD,NPDT,IERR)
CC               IF(IERR.NE.0) GOTO 1000
C                NMOD = NMOD + 1
C                JMOD(NMOD) = NPDT
C              ENDDO
C            ENDDO
C          ENDDO
C        ENDIF
C I STILL NEED TO ELIMINATE CHAMBERS LISTED MORE THAN ONCE ABOVE.
C FOR THE L1.5 SECTION
C----------------------------------------------------------------------
C                          Finish Event
C----------------------------------------------------------------------
 756  CONTINUE               ! JUMP POINT FOR TIR = FALSE  + OFFLINE
      NQUAD=0            
      DO I = 1,NMOD
        IQUAD(MUQUAD(JMOD(I))) = 1
      ENDDO
      DO I=1,12
        J=JQ(I)
        IF(IQUAD(J).EQ.1) THEN
          NQUAD=NQUAD+1
          JQUAD(NQUAD)=J
        ENDIF
      ENDDO

C----------------------------------------------------------------------
  999 RETURN
 1000 CONTINUE
      RETURN

      ENTRY MUMSET_L2_PARAMS(PARAM_SET_NUMBER,TEMPCHAR)
      PSN = PARAM_SET_NUMBER
      DO I = 1,4
        CONTROL(I) = TEMPCHAR(I)
      ENDDO
      RETURN

      END
