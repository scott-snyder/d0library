      SUBROUTINE CAGSFL(DO_GNSCOR,D0_DATA,MONTE_CARLO,SFTVSNIN) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CAGS -
C-                         the calorimeter CAD bank look-up table
C-                         for gain correction * SIN(theta nominal).
C-                         (theta nominal = theta assuming zvertex = 0)
C-                       The table is spaced logarithmically, i.e. an equal
C-                         factor STEP separates each entry; the values in the
C-                         table run from the largest to the smallest.
C-
C-   Inputs  : CADT bank, gains bank, and CELXYZ information
C-             D0_DATA     .TRUE. when table built assuming D0, not NWA data
C-             MONTE_CARLO .TRUE. when table built assuming MC, not real data
C-             SFTVSNIN     [I] Software Version Number assumed
C-   Outputs : CAGS bank
C-                      also sets up D0VSN and SFTVSN in /CUNFLG/ for the
C-                      benefit of 
C-   Controls: DO_GNSCOR        = .TRUE. means get gains from DBL3
C-
C-   Created  24-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DO_GNSCOR
      LOGICAL D0_DATA,MONTE_CARLO
      INTEGER SFTVSNIN
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INTEGER LCAGS,L2CADT
      INTEGER PAKADR
      INTEGER SCALE,INDEX,INDMAX
      INTEGER I,JCRATE,CRATEID
      INTEGER IETA,IPHI,ILYR
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      INTEGER IBSET,IER
      LOGICAL CEXIST,CORRECTEM_BEGIN,OK
      REAL CL2_CAGS                     ! function to get CAGS value
      REAL CAGS,CAGS1,CAGS8             ! sin * adctogev * electr gain corr
      REAL BIG, SMALL                   ! extreme values of above
      REAL STEP, STEPLN                 ! step factor in table, and its ln
      REAL ECN_OFFSET,CC_OFFSET,ECS_OFFSET
C----------------------------------------------------------------------
C
C ****  Loop over CAD addresses to find extreme values
C
      SCALE = 0
      INDEX = 0
      BIG = 0
      SMALL = 1.E9
      
      CALL BKCAGS(LCAGS)    ! dummy copy in "normal" tree
C...save version information so it can be seen inside initialization
      IC(LCAGS+9)     = SFTVSNIN          ! store Version word
      IF (.NOT.D0_DATA) IC(LCAGS+9) = IBSET(IC(LCAGS+9),30)
      IF (MONTE_CARLO)  IC(LCAGS+9) = IBSET(IC(LCAGS+9),29)
      CALL CL2_PUT_CAGS    ! copy CAGS and CADT to work on under SL2H
      CALL CL2_INI                      ! get links to CADT bank
      DO CRATEID = 1,12                 ! Level 2 numbering
        L2CADT = LL2CADT(CRATEID)
        JCRATE = IC(L2CADT+2)            ! hardware crate number
        INDMAX = IC(L2CADT-1) - 3
        DO INDEX = 0,INDMAX
          PAKADR = IC(L2CADT + INDEX + 3)
          IETA = BYTES(BYTE4)
          IPHI = BYTES(BYTE3)
          ILYR = BYTES(BYTE2)
          IF(CEXIST(IETA,IPHI,ILYR)) THEN
C          IF(IETA.NE.0) THEN            ! assume CADTFL called CEXIST
C
C...CAGS is factor to multiply ADC counts by to get ETNOM in GEV
C             SCALE    = 0 ==> X8; = 1 ==> X1
            SCALE = 1
            CAGS1 =CL2_CAGS(JCRATE,INDEX,SCALE,IETA,IPHI,ILYR,DO_GNSCOR)
            SCALE = 0
            CAGS8 =CL2_CAGS(JCRATE,INDEX,SCALE,IETA,IPHI,ILYR,DO_GNSCOR)
            CAGS = SQRT(CAGS1*CAGS8)    ! compromise
            IF (CAGS.LE.0) CAGS = MAX(CAGS1,CAGS8)
            IF (CAGS.GT.BIG) BIG = CAGS
            IF ((CAGS.GT.0).AND.(CAGS.LT.SMALL)) SMALL = CAGS

          ENDIF
        ENDDO
      ENDDO
C
C...now have extreme values: build table; use link from link area
      STEPLN = LOG(BIG/SMALL)/255
      STEP = EXP(STEPLN)
      IC(L2CAGS+6)     = IQ(LHEAD+6)     ! save run number used to find gains
      C(L2CAGS+11)     = BIG
      C(L2CAGS+12)     = SMALL
      C(L2CAGS+13)     = STEP
      C(L2CAGS+14)     = STEPLN
      DO I = 0,255
        C(L2CAGS+I+15) = BIG/(STEP)**I
      ENDDO
C
C
C...get offset in E
      ECN_OFFSET = 0  !for now the offset is always zero
      CC_OFFSET = 0
      ECS_OFFSET = 0
      OK = CORRECTEM_BEGIN()
      CALL EZPICK_NOMSG('CORRECTEM_RCP',IER)
      IF (IER.EQ.0) THEN
        CALL EZGET('NEWDELTA',CC_OFFSET,IER)
        CALL EZRSET
      ENDIF
      CALL EZPICK_NOMSG('ECEMCR_RCP',IER)
      IF (IER.EQ.0) THEN
        CALL EZGETA('ECEM_DELTA_DE',1,1,1,ECN_OFFSET,IER) !use ietac = 14 for EC
        ECS_OFFSET = ECN_OFFSET
        CALL EZRSET
      ENDIF
      C(l2CAGS+271) = ECN_OFFSET 
      C(l2CAGS+272) = CC_OFFSET
      C(l2CAGS+273) = ECS_OFFSET
      CALL PRCAGS(3,L2CAGS,0,'ONE',3)
      IF (STEP.GT.1.03) THEN
        CALL ERRMSG('BIG_STEP','CAGSFL',
     &    'CAGS Table has poor accuracy: See FOR003.DAT','W')
      ENDIF
  999 RETURN
      END
