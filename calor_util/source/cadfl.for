      SUBROUTINE CADFL(CRATE,ADC,BLS,ROTOW,DEPTH,IETA,IPHI,ILYR,ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK AND FILL BANK CAD1,CAD2
C-                         - with fake raw calorimeter data.
C-
C-   Inputs  : CRATE - crate number
C              ADC   - ADC number in crate
C              BLS   - BLS in ADC
C              ROTOW   - READOUT tower in BLS
C-             DEPTH   - depth LAYER in reaout tower
C-             IETA    - ETA index determines CAD1 or CAD2
C-             IPHI    - needed to find gain if real data
C-             ILYR    - LYR detemrines gain used
C-             ENERGY  - ENERGY in cell given by CAEP caltowers
C-   Outputs : CAD bank in /ZEBCOM/
C-   Controls: MONTE_CARLO (logical based on record type)
C-
C-   Created  12-DEC 1988 BY A.P.White
C-   Updated   7-APR-1989   Chip Stewart  CREATES BOTH CAD BANKS (FOR COMPRESS)
C-   Updated  28-FEB-1990   Chip Stewart  New CAD bank format
C-   Updated  29-MAR-1990   Alan M. Jonckheere  Remove zero suppress - in MKCAD
C-   Updated  29-MAR-1990   Alan M. Jonckheere  Do overflows correctly
C-   Updated  18-MAR-1992   Chip Stewart  DATA STATEMENTS -> PARAMETERS
C-                               - Added NWA flags in CAD version
C-                               - Use CAD_GAIN_MC function
C-   Updated  27-AUG-1992   James T. Linnemann  restore functionality of
C-                                    handling real data
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAD1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAD2.LINK/LIST'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      REAL ENERGY,GAIN,CAD_GAIN,CGEV_GAIN
      INTEGER IOFF,I,SCALE,LMT,NMT
      INTEGER ICRATE,CARD,ICNUM,IC1,IC2,II,CRATE,ADC,BLS,ROTOW,DEPTH
      INTEGER NCRATE,IDET
      INTEGER ICHADR,IDAT,USER_INDEX,DATA_TYPE
      INTEGER LCAD,LCADR,LCRATE
      INTEGER NW_CRATE,NW_CRATE_HEAD,NW_CARD,NW_CAD
      INTEGER NWBLS,NWRTOW,NW_CARD_HEAD,NW_CRATE_TRAIL,NW_CAD_TRAIL
C
      INTEGER NNADC,ISYNC,ADR,ADBITS,LADC,LBLS,LEF,LDEP
      INTEGER IETA,IPHI,ILYR,ICOND,NCAD,BANK,USER,NTOSS,NW_BANK
      CHARACTER MSG*80
C
      LOGICAL OK,FIRST,MONTE_CARLO
C
C----------------------------------------------------------------------
C
      DATA FIRST          /.TRUE./
      PARAMETER (NW_CRATE_HEAD=5)           ! number of words in crate header
      PARAMETER (NW_CARD_HEAD=1)            ! number of words in card header
      PARAMETER (NW_CRATE_TRAIL=4)          ! number of words in crate trailer
      PARAMETER (ISYNC=2**16-1)             ! synch word least sig 16 bits
      PARAMETER (NW_CAD_TRAIL=16)           ! number of words in CAD trailer
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF ( LQ(LHEAD-IZCAD1).GT.0 .OR.
     &         LQ(LHEAD-IZCAD2).GT.0 ) THEN
          CALL ERRMSG('D0GEANT','CADFL',
     &         ' CAD BANK ALREADY EXISTS ON INPUT DATA','W')
        ENDIF
        MONTE_CARLO = IQ(LHEAD+1).GE.1000
        CALL STP_GSLINK('CADFL',NMT )
        STP_LSLINK(NMT) = 0
      ENDIF
      DO NCAD = 1, 2
        IF ( NCAD.EQ.1 ) THEN
          LCAD = LQ(LHEAD-IZCAD1)
        ELSE
          LCAD = LQ(LHEAD-IZCAD2)
        ENDIF
C
C--- IF CAD DOES NOT EXIST THEN BOOK IT.
        IF ( LCAD.EQ.0 ) THEN
C
C ****  SET UP BANK
          NW_CARD = NDEPTC*NEFC*NBLSC + NW_CARD_HEAD
          NW_CRATE = NW_CARD*NADCC + NW_CRATE_HEAD + NW_CRATE_TRAIL
          NW_CAD = NADCRC*NW_CRATE+NW_CAD_TRAIL
          IF ( NCAD.EQ.1 ) THEN
            CALL BKCAD1(LCAD)
            BANK = 7
          ELSE
            CALL BKCAD2(LCAD)
            BANK = 8
          ENDIF
C
C ****  CHECK FOR EMPTY CAD BANKS stored in ZEBSTP
C
          LMT = STP_LSLINK(NMT)
          IF (LMT.GT.0) THEN
            IF (NCAD.EQ.2) LMT = LC(LMT)
          END IF
          IF(LMT.GT.0) THEN
            CALL UCOPY(IC(LMT+1),IQ(LCAD+1),NW_CAD)
            GOTO 72
          END IF
C
C--- NOW SET UP THE "FIXED" PART OF CAD.
C--- LOOP OVER CRATES
          IOFF = 0
          DO ICRATE = 0,NADCRC-1
            IQ(LCAD+IOFF+1) = NW_CRATE_HEAD - 1   ! Header length (not inclusive)
            IQ(LCAD+IOFF+2) = ISYNC               ! SYNC word
C
C ****  Controller word
            IQ(LCAD+IOFF+3) = (ICRATE*10+BANK)*2**24 + ! CRATE NUMBER
     &         (NADCC-1)*2**16 +                  ! ADC CARD NUMBER
     &         2**3 +                             ! PED SUBTRACTED DATA
     &         1                                  ! DATA (NOT PEDS)
C
C ****  SOFTWARE VERSION NUMBER word
            SFTVSN = D0_SFTVSN                 !Current D0 MC SFTVSN
C
C ****  IS THIS GEANT TB or D0?
            USER_INDEX = 0
            DATA_TYPE = 0
            IF (MONTE_CARLO) THEN
              DATA_TYPE  = 2**29
              SFTVSN = D0_MC_SFTVSN                 !Current D0 MC SFTVS
            END IF
            IF (LOAD1) THEN
              USER_INDEX = 2**16              ! TB90 LOAD 1
              DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
            ELSE IF (LOAD2) THEN
              USER_INDEX = 2**17              ! TB90 LOAD 2
              DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
            END IF
            IF ( PLATE_GEOM) USER_INDEX = USER_INDEX + 2**18  ! Plate geometry
C
            IQ(LCAD+IOFF+4) = SFTVSN +            ! Version #
     &                        USER_INDEX  +       ! CALVSN:L1,L2,PLT,MIX,NOISE
     &                        DATA_TYPE           ! D0VSN: NWA/load1/load2
C
C ****  STATUS/VERTEX word
            IQ(LCAD+IOFF+5) = 0      ! Error indicators - no LV0 vertex yet
C
C ****  LOOP OVER ADC CARDS
            DO CARD = 1,NADCC
              IQ(LCAD+IOFF+(CARD-1)*NW_CARD + NW_CRATE_HEAD + 1)
     &          = NW_CARD-1                       !CARD HEADER
C ****  LOOP OVER ADDRESSES
              I = 0
              LADC = CARD -1
              DO LBLS = 0, NBLSC - 1
                DO LEF = 0, NEFC - 1
                  DO LDEP = 0, NDEPTC - 1
                    I = I + 1
                    CALL CADPAK (LADC,LBLS,LEF,LDEP,1,0,ADR)
                    CALL SBYT(ADR,ADBITS,17,16)
                    IQ(LCAD+IOFF+(CARD-1)*NW_CARD+NW_CRATE_HEAD+1+I)
     &                   = ADBITS
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
C
C ****  TRAILER - TOTAL WORD COUNT
            IQ(LCAD+IOFF+(NADCC-1)*NW_CARD+NW_CRATE_HEAD+I+2) =
     &          (NADCC-1)*NW_CARD+NW_CRATE_HEAD+I+1+NW_CRATE_TRAIL
C
C ****  EVENT NUMBER/CRATE NUMBER NOT YET DEFINED
            IQ(LCAD+IOFF+(NADCC-1)*NW_CARD+NW_CRATE_HEAD+I+3) = 0
C
C ****  TOKEN VALUE SET TO hex FFFFFFFF
            IQ(LCAD+IOFF+(NADCC-1)*NW_CARD+NW_CRATE_HEAD+I+4) = -1
C
C ****  CHECKSUM NOT DEFINED
            IQ(LCAD+IOFF+(NADCC-1)*NW_CARD+NW_CRATE_HEAD+I+5) = 0
C
            IOFF = IOFF + NW_CRATE
          ENDDO
          IOFF = IOFF + NW_CAD_TRAIL
          NW_BANK = IQ(LCAD-1)                ! 28000 FROM BKCAD1,BKCAD2
          NTOSS = NW_BANK - IOFF
          CALL MZPUSH(IXMAIN,LCAD,0,-NTOSS,'R')
          IF(NCAD.EQ.1) THEN
            CALL MZBOOK (IDVSTP,LMT,0,2,'CAMT',0,0,NW_CAD,2,0)
            STP_LSLINK(NMT) = LMT
            CALL UCOPY(IQ(LCAD+1),IC(LMT+1),NW_CAD)
          ELSE
            LMT = STP_LSLINK(NMT)
            IF(LMT.GT.0) THEN
              CALL MZBOOK (IDVSTP,LMT,LMT,0,'CAMT',0,0,NW_CAD,2,0)
              CALL UCOPY(IQ(LCAD+1),IC(LMT+1),NW_CAD)
            ELSE
              CALL ERRMSG('MKCAD','CADFL','LOST CAMT BANKS','W')
            END IF
          END IF
        ENDIF
   72   CONTINUE
      ENDDO
C
C ****  FIND GAIN - GeV to ADC counts
      IF (MONTE_CARLO) THEN
        GAIN = CAD_GAIN (IETA,ILYR)
      ELSE
        SCALE = 1     ! scale bit is on in packed address above
        GAIN = CGEV_GAIN(ILYR,IPHI,IETA,SCALE)
      ENDIF
C
C ****  CHECK FOR ZERO GAIN
C
      IF (GAIN.LE.0) THEN
        WRITE(MSG,19) IETA,ILYR,IPHI,GAIN
   19   FORMAT ('ETA',I5,' LYR',I3,' PHI ',I3,' GAIN',E7.1)
        CALL ERRMSG('ZERO_GAIN','CADFL',MSG,'W')
        GAIN = -1.E+37
      END IF
C
C--- Get appropriate CAD bank
      IF ( IETA.LT.0 ) THEN
        LCAD = LQ(LHEAD-IZCAD1)
        BANK = 7
      ELSE
        LCAD = LQ(LHEAD-IZCAD2)
        BANK = 8
      ENDIF
C
C--- DECIDE WHICH CRATE SECTION TO PUT DATA IN.
      ICRATE = CRATE/10
C
C--- CALCULATE RELATIVE ADDRESS IN CAD BANK
      LCADR = ICRATE*NW_CRATE +
     &   NW_CRATE_HEAD + ADC*NW_CARD + BLS*NEFC*NDEPTC +
     &   ROTOW*NDEPTC + DEPTH + NW_CARD_HEAD + 1
C
C **** PACK ADDRESS WORD
      ICHADR = ADC*2**11 + BLS*2**8 +
     &                 ROTOW*2**6 + DEPTH*4 + 2

C
C ****  ROUND ADC COUNTS (NOT TRUNCATE)
      IDAT = NINT(ENERGY/GAIN)
C
C ****  Check for overflow
      IF ( IDAT.GE.2**15 ) THEN
        WRITE(MSG,20) IETA,ILYR,IDAT
   20   FORMAT ('ADC OVERFLOW ETA',I5,' LYR',I5,' ',I9,' COUNTS')
        CALL ERRMSG('D0GEANT','CADFL',MSG,'W')
        IDAT = 2**15 - 1
      ENDIF
C
C ****  CHECK ADDRESS
      IF ( IQ(LCAD+LCADR).NE.ICHADR*2**16 ) THEN
        WRITE(MSG,21) IQ(LCAD+LCADR),ICHADR*2**16,ADC,BLS,ROTOW,DEPTH
   21   FORMAT (' BAD ADRRESS ',2Z15.1,4I5)
        CALL ERRMSG('D0GEANT','CADFL',MSG,'W')
      ENDIF
      IQ(LCAD+LCADR) = IQ(LCAD+LCADR) + IDAT
C
  999 RETURN
      END
