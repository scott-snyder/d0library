      SUBROUTINE CL2TEST_CAGS_HISTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Histograms checking quality of the banks CAGS/CADT
C-      for CL2_xxxx unpacking
C-
C-   Inputs  : CADT and CAGS banks
C-   Outputs : Histos
C-   Controls: DO_GNSCOR from CAHITS.RCP
C-
C-   Created 26-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INTEGER L2CADT
      INTEGER PAKADR
      INTEGER SCALE,INDEX,INDMAX
      INTEGER JCRATE,CRATEID
      INTEGER IETA,IPHI,ILYR
C&IF VAXVMS,VAXELN
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ENDIF
      LOGICAL CEXIST
      INTEGER INDCAGS,INDFND,IAND
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL IAND
C&ENDIF
      REAL CL2_CAGS                     ! function to get CAGS value
      REAL CAGS,CAGS1,CAGS8             ! sin * adctogev * electr gain corr
      REAL BIG                          ! extreme value of above
      REAL STEPLN                       ! ln of step factor in table
      LOGICAL EZERROR,OK
      INTEGER IER
      LOGICAL DO_GNSCOR
C----------------------------------------------------------------------
C
C ****  Loop over CAD addresses to find and deduce nearest CAGS entry
C
      CALL EZPICK('CAHITS_RCP')       ! Select bank
      OK = .NOT.EZERROR(IER)
      IF (IER .EQ. 0) CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
      IF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('NO CAHITS_RCP','CL2TEST_CAGS_HISTS',
     &  ' Error while reading CAHITS_RCP: DO_GNSCORR missing','F')
      ELSE
        CALL EZRSET
      ENDIF
      CALL HBOOK1(1,' ACTUAL FACTOR (GEOM MEAN) / TABLE',50,.9,1.1,0.)
      CALL HIDOPT(1,'STAT')
      CALL HBOOK1(2,' X1 / X8 GAIN',50, .5, 1.5, 0.)

      SCALE = 0
C
C...get info needed to calculate which CAGS table entry is closest
      BIG= C(L2CAGS+11)
      STEPLN = C(L2CAGS+14)
      DO CRATEID = 1,12                 ! Level 2 numbering
        L2CADT = LL2CADT(CRATEID)
        JCRATE = IC(L2CADT+2)            ! hardware crate number
        INDMAX = IC(L2CADT-1) - 3
        DO INDEX = 0,INDMAX
          PAKADR = IC(L2CADT + INDEX + 3)
C&IF VAXVMS,VAXELN
          IETA = BYTES(4)
          IPHI = BYTES(3)
          ILYR = BYTES(2)
C&ENDIF
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
            IF (CAGS.LE.0) THEN
              CALL HFILL(1,0.,0.,1.)
              CALL HFILL(2,-1.,0.,1.)
            ELSE
              INDCAGS = .5 + LOG(BIG/CAGS)/STEPLN
              INDFND = IAND(PAKADR,255)
              IF (INDFND.NE.INDCAGS) THEN
                CALL ERRMSG('CALORIMETER','CL2_CADT_HIST',
     &            'Calculated and stored index differ','W')
              ENDIF
              CALL HFILL(1,CAGS/C(L2CAGS+15+INDCAGS),0.,1.)
              IF (CAGS8.GT.0) THEN
                CALL HFILL(2,CAGS1/CAGS8,0.,1.)
              ELSE
                CALL HFILL(2,999999.,0.,1.)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
      END
