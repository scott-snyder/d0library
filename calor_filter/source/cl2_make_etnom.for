      SUBROUTINE CL2_MAKE_ETNOM(CRATID,START,STOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      localized filling of CAEP bank; based on CAEPFL and CAEHFL
C-      Book and fill CAEP bank starting from CAD1 and CAD2
C-      WARNINGS: the bottom byte of the packed address in the CAEP bank
C-                      is NOT the flag bits as in offline
C-                the CAEP bank holds ETNOM (ET calculated at zvertex = 0) not E
C-
C-   Inputs  : CRATID   Level 2 ID of crate to unpack
C-             START    pointer to first word to convert to ETNOM
C-             STOP     pointer to last word to convert
C-              link areas for L2 CAEP, PNUT banks, and CADT,CAGS banks
C-   Outputs : Partially filled CAEP bank (with packed address and ETNOM),
C-             Correct values in the channels actually converted in PTCAEP2
C-                zero for channels not converted, but zero suppressed
C-                PROVIDED that CL2_ROTOW_ETNOM has been called for that tower
C-   Controls: L2PNUT = 0 means don't do Missing PT and Sum ET calculations
C----------------------------------------------------------------------
C-
C-   Created   6-JAN-1991   James T. Linnemann
C-   Updated   1-MAY-1991   James T. Linnemann for MPT
C-   Updated  25-JUL-1992   James T. Linnemann  add zvtx correction
C-   Updated  10-SEP-1992   James T. Linnemann  add bad cell killing
C-   Updated  13-OCT-1992   James T. Linnemann  zebra protection for Zvtx
C-   Updated  11-DEC-1992   William Cobau   -- add E_TOTAL sum
C-   Updated  29-DEC-1992   Amber Boehnlein    add logic for turning off
C-                                             CH-OH when micro-blank is off.
C-   Updated  07-JAN-1993  James T. Linnemann, Amber Boehnlein,
C-                                             add list code for multiple
C-                                             hot cells per event, clean
C-                                             initialization
C-   Updated  27-SEP-1994   James T. Linnemann speedup: remember MAIN_RING
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CRATID,START,STOP
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER NONZCH                    ! how many channels so far
      INTEGER POINT,POINTCAEP,NR        ! data pointers
      INTEGER IETA,IPHI,LYR             ! offline indices after unpacking
      INTEGER IWORD,PAKADR              ! data and decoded address
      INTEGER INDEX,IPH                 ! relevant address and ph info
      INTEGER L2CADT                    ! CADT link
      INTEGER INDMAX                    ! max index into address decode bank
      INTEGER INDCAGS                   ! pointer into gain*sin theta nom table
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),PAKADR)
      INTEGER*2 ISHORT(2)
      EQUIVALENCE (ISHORT,IWORD)
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER IAND,ISHFT,IER
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CL2_ETMISS_GEOM.INC'
      INTEGER ICOARSE
      REAL    EX,EY,ET,ET_BIG
      REAL    E_TOTAL
      LOGICAL DO_ZCORR,MAIN_RING_ON,MICRO_BLANK,MRBS_LOSS,DROP_LYR
      LOGICAL PNUT_REMOVE,CAEP_REMOVE,OUT_OF_CAEP,OUT_OF_PNUT
      SAVE DO_ZCORR
      SAVE OUT_OF_CAEP,OUT_OF_PNUT
      DATA DO_ZCORR/.TRUE./
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER ETMISS_EVENT             ! event for which Mpt info is  valid
      INTEGER MAIN_RING_EVENT          ! event for which main_ring info valid
      SAVE ETMISS_EVENT,MAIN_RING_EVENT
      DATA ETMISS_EVENT/-876543/, MAIN_RING_EVENT/-7654321/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL CL2_CHECK_SFTVSN(CRATID)
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      IF(L2CAEP.LE.0) THEN
        CALL ERRMSG('CAL_DATA','CL2_MAKE_ETNOM','CAEP NOT BOOKED','E')
        GO TO 999
      ENDIF
      NR = IQ(L2CAEP + 2)
      NONZCH=IQ(L2CAEP+3)
      PTCAEP2_ALL_ZEROS = .FALSE.       ! initial opinion about PTCAEP2 status
C
C...Check to see if the microblank bit is on
C...used by unpacking for any tool
C...the conditions are now generalized for use with the active veto
      IF (MAIN_RING_EVENT.NE.IQ(LHEAD+7)) THEN
C...  this info once per event because the retreival routines are slow
        MAIN_RING_ON = MICRO_BLANK(IER).OR.MRBS_LOSS(IER) 
        MAIN_RING_EVENT = IQ(LHEAD+7)
      ENDIF
C...update Etmiss and Etsum info
      IF (L2PNUT.GT.0) THEN
        EX = -Q(L2PNUT+3)
        EY = -Q(L2PNUT+4)
        ET = Q(L2PNUT+14)
        E_TOTAL = Q(L2GLOB+2)
C
C...check if information is valid for last Etmiss event
        IF (ETMISS_EVENT.NE.IQ(LHEAD+7)) THEN
          CALL CL2_RESET_BIG_CELL_LIST(ET_BIG)  !only reset here because checked
                                                !only for etmiss events
C
C...calculate sin theta correction for towers at a given eta for this event
          IF ( DO_ZCORR ) CALL CL2_ETMISS_Z_GEOM ! Zvtx correction for event
          ETMISS_EVENT = IQ(LHEAD+7)
        ENDIF
      ENDIF
C
C...set up for address and gain fetching
      L2CADT = LL2CADT(CRATID)            ! which CADT bank
      IF (L2CADT.LE.0.OR.L2CAGS.LE.0) THEN
        CALL ERRMSG('CAL_DATA','CL2_MAKE_ETNOM',
     &          'CADT OR CAGS NOT BOOKED','F')
        GO TO 999
      ENDIF
      INDMAX = IC(L2CADT-1) -3           ! end of address
C
      POINTCAEP=L2CAEP+3+NR*NONZCH
      DO 40 POINT = START,STOP
        IWORD=IQ(POINT)
C
C...must be very sure that address comes back either safe for updating PTR2 or
C   with flag showing invalidity, to avoid overwriting.
C
C...get address and pulse height from IWORD (BEWARE!! ISHORT(2) has HI bits)
        INDEX = ISHFT(ISHORT(WORD2),-2)     ! shift away SCALE and NEGLIM; also
                                            ! shifts down sign bit so INDEX > 0
        IPH = ISHORT(WORD1)
        IF (INDEX.LE.INDMAX) THEN       ! guarantee address in legal range
          PAKADR = IC(L2CADT+INDEX+3)    ! packed address, and gain pointer
          IETA=BYT(BYTE4)
          IPHI=BYT(BYTE3)
          LYR=BYT(BYTE2)
          IF(IETA.NE.0) THEN  ! CADT maps bad channels to eta=0
            IF(MAIN_RING_ON) THEN
              DROP_LYR = (LYR.GE.16).OR.                  ! kill OCH (16,17)
     &          ((LYR.EQ.15).AND.(ABS(IETA).LE.12))  !and CH (15) in central eta
              IF (DROP_LYR) GO TO 40  !dump the point
            ENDIF
C
C ...fill CAEP
C
C...do index extraction carefully, since INDCAGS is an UNSIGNED 8 bit integer
            INDCAGS = IAND(PAKADR,255)      ! get adc_to_gev index
            IQ(POINTCAEP+1)=PAKADR               ! packed physics address
            Q(POINTCAEP+2) = C(L2CAGS+INDCAGS+15)*IPH   ! Etnom (zv = 0)
            IF (L2PNUT.GT.0) THEN
              ET = ET + Q(POINTCAEP+2)*ET_CORR(IETA)
              ICOARSE = 1
              IF (IABS(IETA).GT.32) ICOARSE = 2
              EX = EX + Q(POINTCAEP+2)*ET_CORR(IETA)*CS(IPHI,ICOARSE)
              EY = EY + Q(POINTCAEP+2)*ET_CORR(IETA)*SN(IPHI,ICOARSE)
              E_TOTAL = E_TOTAL + Q(POINTCAEP+2)*ET_TO_E(IETA)
C
C...check for highest ET cells to later check if they are bad
              IF (Q(POINTCAEP+2).GT.ET_BIG)
     &            CALL CL2_ADD_BIG_CELL(Q(POINTCAEP+2),IETA,IPHI,LYR,
     &            ET_BIG)
            ENDIF
            POINTCAEP=POINTCAEP+2
            NONZCH=NONZCH+1
            PTR2(LYR,IPHI,IETA)=NONZCH
          ENDIF
        ENDIF
   40 CONTINUE
      IQ(L2CAEP+3) = NONZCH
      PTCAEP2_ALL_ZEROS = (NONZCH.EQ.0) ! final opinion about PTCAEP2 status
      IF (L2PNUT.GT.0) THEN
        Q(L2PNUT+3) = -EX               ! missing Ex
        Q(L2PNUT+4) = -EY               ! missing Ey
        Q(L2PNUT+7) = SQRT(EX**2 + EY**2 + .000001)     ! Missing ET
        Q(L2PNUT+14) = ET               ! sum ET
        Q(L2GLOB+2) = E_TOTAL          ! energy deposited in calorimeter
        Q(L2PNUT+10) = ATAN2(-EY,-EX+.0000001)  ! PHI of missing ET
        IF (Q(L2PNUT+10).LT.0) Q(L2PNUT+10) = Q(L2PNUT+10) + TWOPI
        Q(L2PNUT+5) = 0                 ! missing Ez
        Q(L2PNUT+6) = ET                ! sum E
        Q(L2PNUT+8) = HALFPI            ! theta of missing E
        Q(L2PNUT+9) = 0                 ! eta of missing E
        Q(L2PNUT+11) = 1                ! weights
        Q(L2PNUT+12) = 1
        Q(L2PNUT+13) = 1
      ENDIF
  999 RETURN
      END
