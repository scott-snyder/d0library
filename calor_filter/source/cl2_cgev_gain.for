      FUNCTION CL2_CGEV_GAIN(LAYERC,IPHIC,IETAC,SCALE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        get a conversion of PH to Energy from CAGS bank
C-        E = C*PH
C-        This version is not optimized for speed (calls address translation)
C-   Returned value  : C  (= 0 if there was a problem)
C-   Inputs  : LYRC,IPHIC,IETAC (note the index order) physics cell indices
C-             SCALE = 0 or 1 for X8, X1
C-   Outputs : the C factor
C-   Controls: none
C-
C-   Created  17-AUG-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    CL2_CGEV_GAIN
      INTEGER  IETAC,IPHIC  !  offline Eta, Phi indexes
      INTEGER  LAYERC       !  Offline radial index.
      INTEGER  SCALE        !  Bit 1 (2nd bit) of IADDR
      INCLUDE 'D0$INC:CL2CRATE.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER PAKADR,IETA,IPHI,LYR,CRATEID,INDEX
      REAL    GAIN_SIN,CL2_SNTH
      BYTE BYT(4)
      EQUIVALENCE (BYT(1),PAKADR)
      INTEGER  CRATE        !  The crate (cabale) number.
      INTEGER  IADDR        !  The packed hex address.
      INTEGER  ADC          !  ADC card number in crate.
      INTEGER  BLS          !  BLS card number.
      INTEGER  DEPTH        !  Depth in the readout tower.
      INTEGER  ICOND        !
      INTEGER  NEGLIM       !  NEGative LIMit bit (bit 0) of IADDR.
      INTEGER  ROTOW        !  Readout tower in the BLS.
      LOGICAL  CEXIST       !  Serban's routine to check existence of cell
      INTEGER L2CADT                    ! CADT link
      INTEGER INDMAX                    ! max index into address decode bank
      INTEGER INDCAGS                   ! pointer into gain*sin theta nom table
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL CL2_INI
      ENDIF
      CL2_CGEV_GAIN = 0.
      IF (CEXIST(IETAC,IPHIC,LAYERC)) THEN
        CALL CPHAD(IETAC,IPHIC,LAYERC,CRATE,ADC,BLS,ROTOW,DEPTH,ICOND)
        NEGLIM = 0
        CALL CADPAK(ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,IADDR)
        CRATEID = L2CRATE(CRATE)
        L2CADT = LL2CADT(CRATEID)            ! which CADT bank
        IF (L2CADT.LE.0.OR.L2CAGS.LE.0) THEN
          CALL ERRMSG('NO_CADT_OR_CAGS','CL2_CGEV_GAIN',
     &          'CADT OR CAGS NOT BOOKED','F')
          GO TO 999
        ENDIF
        INDMAX = IC(L2CADT-1) -3           ! end of address
        INDEX = IADDR
        IF (INDEX.LE.INDMAX) THEN
          PAKADR = IC(L2CADT+INDEX+3)    ! packed address, and gain pointer
          IETA=BYT(BYTE4)
          IPHI=BYT(BYTE3)
          LYR=BYT(BYTE2)
          IF(IETA.NE.0) THEN              ! convention for valid address in CADT
C
C...do index extraction carefully, since INDCAGS is an UNSIGNED 8 bit integer
            INDCAGS = IAND(PAKADR,255)      ! get adc_to_gev index
            GAIN_SIN = C(L2CAGS+INDCAGS+15)
            Cl2_CGEV_GAIN = GAIN_SIN/CL2_SNTH(IETAC,IPHIC,LAYERC,0.0)
C
          ENDIF
        ENDIF
      ENDIF

  999 RETURN
      END
