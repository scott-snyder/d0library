      SUBROUTINE NOI_CADFL(CRATE,ADC,BLS,ROTOW,DEPTH,IETA,ILYR,
     &                          ENERGY,NPRE,NOILEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK AND FILL BANK CAD1,CAD2 PRIME
C                          FOR PILEUP
C-                         SLIGHTLY MODIFIED VERSION OF CADFL
C-
C-   Inputs  : CRATE - crate number
C              ADC   - ADC number in crate
C              BLS   - BLS in ADC
C              ROTOW   - READOUT tower in BLS
C-             DEPTH   - depth LAYER in reaout tower
C-             IETA    - ETA index determines CAD1 or CAD2
C-             ILYR    - LYR detemrines gain used
C-             ENERGY  - ENERGY in cell given by CAEP caltowers
C-             NPRE    - NUMBER of previously exisiting CAD banks.
C-                       This call will create a set of banks pointed
C-                       to by LQ(LHEAD-IZCAD1).  If NPRE=1, then
C-                       there will also be a bank at
C-                       LQ(LQ(LHEAD-IZCAD1)).  If NPRE=2 then there
C-                       is also LQ(LQ(LQ(LHEAD-IZCAD1)))
C-             NOILEV  - version off noise added:
C-                       0,1,2 for none, trigger level1, and level 2
C-
C-   Outputs : CAD bank in /ZEBCOM/
C-
C-
C-   Created  31-JUL-1991   BY A.I. Mincer
C-   Modified 26-FEB-1992      A.I. Mincer
C-                              Handle multiple possibilities of
C-                              previous number of CAD banks
C-   Modified 21-MAY-1992      A.I.M. conform to new CADFL
C-   Modified 17-SEP-1993      Ian Adam 
C-     Add checks on LCAD>0 before using LQ(LCAD)
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAD1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAD2.LINK/LIST'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      REAL ENERGY,GAIN,CAD_GAIN
      INTEGER IOFF,I
      INTEGER ICRATE,CARD,ICNUM,IC1,IC2,II,CRATE,ADC,BLS,ROTOW,DEPTH
      INTEGER NCRATE,IDET
      INTEGER ICHADR,IDAT,USER_INDEX,DATA_TYPE
      INTEGER LCAD,LCADR,LCRATE
      INTEGER NW_CRATE,NW_CRATE_HEAD,NW_CARD
      INTEGER NWBLS,NWRTOW,NW_CARD_HEAD,NW_CRATE_TRAIL
C
      INTEGER NNADC,ISYNC,ADR,ADBITS,LADC,LBLS,LEF,LDEP
      INTEGER IETA,IPHI,ILYR,ICOND,NCAD,BANK,USER
      CHARACTER MSG*70
C
      LOGICAL OK,FIRST,FIRST1,FIRST2
      INTEGER NPRE,NOILEV,NOIVER
C
C----------------------------------------------------------------------
C
      DATA FIRST          /.TRUE./
      PARAMETER (NW_CRATE_HEAD=5)           ! number of words in crate header
      PARAMETER (NW_CARD_HEAD=1)            ! number of words in card header
      PARAMETER (NW_CRATE_TRAIL=4)          ! number of words in crate trailer
      PARAMETER (ISYNC=2**16-1)             ! synch word least sig 16 bits
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IF(NPRE.GT.0)THEN
          IF ( LQ(LHEAD-IZCAD1).LE.0 .OR.
     &           LQ(LHEAD-IZCAD2).LE.0 ) THEN
            CALL ERRMSG('NOISY','NOI_CADFL',
     &           ' ORIGINAL CAD BANKS DO NOT EXIST','W')
          ELSE
            IF(NPRE.GT.1)THEN
              IF ( LQ(LQ(LHEAD-IZCAD1)).LE.0 .OR.
     &               LQ(LQ(LHEAD-IZCAD2)).LE.0 ) THEN
                CALL ERRMSG('NOISY','NOI_CADFL',
     &               ' SECOND SET OF CAD BANKS DO NOT EXIST','W')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      DO NCAD = 1, 2
        IF ( NCAD.EQ.1 ) THEN
          LCAD = LQ(LHEAD-IZCAD1)
          IF((NPRE.GT.0).AND.(LCAD.GT.0)) LCAD=LQ(LCAD)
          IF((NPRE.GT.1).AND.(LCAD.GT.0)) LCAD=LQ(LCAD)
          IF(LCAD.GT.0)THEN
            FIRST1=.FALSE.
          ELSE
            FIRST1=.TRUE.
          ENDIF
          LCAD = LQ(LHEAD-IZCAD1)
        ELSE
          LCAD = LQ(LHEAD-IZCAD2)
          IF((NPRE.GT.0).AND.(LCAD.GT.0)) LCAD=LQ(LCAD)
          IF((NPRE.GT.1).AND.(LCAD.GT.0)) LCAD=LQ(LCAD)
          IF(LCAD.GT.0)THEN
            FIRST2=.FALSE.
          ELSE
            FIRST2=.TRUE.
          ENDIF
          LCAD = LQ(LHEAD-IZCAD2)
        ENDIF
C
C--- IF CAD DOES NOT EXIST THEN BOOK IT.
        IF ((FIRST1.AND.NCAD.EQ.1).OR.(FIRST2.AND.NCAD.EQ.2)) THEN
C
C ****  SET UP BANK
          NW_CRATE = (NDEPTC*NEFC*NBLSC + NW_CARD_HEAD)*NADCC +
     &      NW_CRATE_HEAD + NW_CRATE_TRAIL
          NW_CARD = NDEPTC*NEFC*NBLSC + NW_CARD_HEAD
          IF ( NCAD.EQ.1 ) THEN
            CALL BKCAD1(LCAD)
            BANK = 7
          ELSE
            CALL BKCAD2(LCAD)
            BANK = 8
          ENDIF
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
            SFTVSN = D0_MC_SFTVSN                 !Current D0 MC SFTVSN
C
C ****  IS THIS GEANT TB or D0?
            USER_INDEX = 0
            DATA_TYPE  = 2**29                ! assume MC
            IF (LOAD1) THEN
              USER_INDEX = 2**16              ! TB90 LOAD 1
              DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
            ELSE IF (LOAD2) THEN
              USER_INDEX = 2**17              ! TB90 LOAD 2
              DATA_TYPE  = DATA_TYPE + 2**30  ! TB90
            END IF
            IF ( PLATE_GEOM) USER_INDEX = USER_INDEX + 2**18  ! Plate geometry
C ****  NOISE LEVEL
            IF(NOILEV.EQ.0)THEN
              NOIVER = 0
            ELSEIF(NOILEV.EQ.1)THEN
              NOIVER = 2**20
            ELSEIF(NOILEV.EQ.2)THEN
              NOIVER = 2**21
            ELSE
              CALL ERRMSG('NOISY','NOI_CADFL',
     &            ' NOISE LEVEL ERROR ','W')
            ENDIF
            USER_INDEX=USER_INDEX+NOIVER
C
            IQ(LCAD+IOFF+4) = SFTVSN +            ! Version #
     &                        USER_INDEX  +       ! CALVSN:L1,L2,PLT,MIX,NOISE
     &                        DATA_TYPE           ! D0VSN: NWA/load1/load2
C
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
        ENDIF
      ENDDO
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
C ****  FIND GAIN - GeV to ADC counts
      GAIN = CAD_GAIN (IETA,ILYR)
C
C ****  ROUND ADC COUNTS (NOT TRUNCATE)
      IDAT = NINT(ENERGY/GAIN)
C
C ****  Check for overflow
      IF ( IDAT.GE.2**15 ) THEN
        WRITE(MSG,20) IETA,ILYR,IDAT
   20   FORMAT (' NOI_CADFL: ADC OVERFLOW ETA',I5,
     &       ' LYR',I5,' ',I9,' COUNTS')
        CALL ERRMSG('NOISY','NOI_CADFL','ADC OVERFLOW','W')
        IDAT = 2**15 - 1
      ENDIF
C
C ****  CHECK ADDRESS
      IF ( IQ(LCAD+LCADR).NE.ICHADR*2**16 ) THEN
        WRITE(MSG,21) IQ(LCAD+LCADR),ICHADR*2**16,ADC,BLS,ROTOW,DEPTH
   21   FORMAT (' NOI_CADFL: BAD ADRRESS ',2Z15.1,4I5)
        CALL ERRMSG('NOISY','NOI_CADFL','BAD ADDRESS','W')
      ENDIF
      IQ(LCAD+LCADR) = IQ(LCAD+LCADR) + IDAT
C
  999 RETURN
      END
