      SUBROUTINE MTC_FILL_MPV()
C----------------------------------------------------------------------
C- MTC_FILL_MPV: part of MTC (Muon Tracking in the Calorimeter) package
C- 
C-   Call this routine only once, before any events are processed.
C-
C-   Purpose and Methods : 
C-      1.  Read in RCP input
C-      2.  Convert the MPV (most probable value) from the 
C-              Moyal*Gaussian fits in ADC counts to energy in GeV
C-              for the 1x1 and 3x3 cell per layer calorimeter tower 
C-              layers about the central eta,phi in /MTC_ETOWERS/.
C-   Call this routine only once for any job.
C-
C-   Inputs  : This routine and MTC_FILL_MPVCOM read RCP input data to fill
C-      /MTC_CSFCOM/, /MTC_CALSECTS/, /MTC_ISSUBCAL/, /MTC_CSFICD/
C-   Outputs : Also the MPV and RMS(mpv) in MTC_MPVCOM by 
C-      (calorimeter section, cal sect sublayer, cal sect sublyr eta)
C-      are converted from input ADC counts to GeV used by program.
C-
C-   Created   7-JUL-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C- here we store the conversion factors from adc to total energy (GeV) ...
C- The tile to tile corrections for the ICD are in MTC_CSFICD
      REAL a(10), w_ccem(7), w_ecem(7), w_ccmg(5), w_icd(6), w_ecmg(6),
     &            w_ccfh(3), w_ecih(5), w_ecmh(5), w_ccch(1), w_ecoh(3)
C----------------------------------------------------------------------
C- /MTC_CSFICD/ contains the channel to channel variation in MIPs/ADC
C-    from tile to tile in the ICD - csf_icd_n(1:384),csf_icd_s(1:384)
C-    AICD_TYPE(1:768) - icd tube type
      INCLUDE 'D0$INC:MTC_CSFICD.INC'
      INCLUDE 'D0$INC:MTC_ACSFICD.INC'
C- There are 2 PMT types:  Hamamatsu (R647) and Russian (PM60) tubes.
cc      CHARACTER*4 APMTTYPE(3)
cc      DATA APMTTYPE/'R647','PM60','NULL'/
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers
C- and eta ranges
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C----------------------------------------------------------------------
C- /MTC_MPVCOM/ contains most prob value in adc counts, and in GeV
      INCLUDE 'D0$INC:MTC_MPVCOM.INC'
C----------------------------------------------------------------------
C- local
      INTEGER IER, ICHAR
      INTEGER ICAL,ISUB,ILYR,ISETA, ISETA2, IPHI,IETA, ICOUNT, ICOUNT2
      CHARACTER*4 ATUBE
C----------------------------------------------------------------------
C- initialization
C- the calorimeter module types
      ACALTYPE(1) = 'CCEM'
      ACALTYPE(2) = 'ECEM'
      ACALTYPE(3) = 'CCMG'
      ACALTYPE(4) = 'ICD '
      ACALTYPE(5) = 'ECMG'
      ACALTYPE(6) = 'CCFH'
      ACALTYPE(7) = 'ECIH'
      ACALTYPE(8) = 'ECMH'
      ACALTYPE(9) = 'CCCH'
      ACALTYPE(10) = 'ECOH'
C- the number of sublayers in each calorimeter module type
      ISUBLAYER(1) =  4
      ISUBLAYER(2) =  4
      ISUBLAYER(3) =  1
      ISUBLAYER(4) =  1
      ISUBLAYER(5) =  1
      ISUBLAYER(6) =  3
      ISUBLAYER(7) =  5
      ISUBLAYER(8) =  5
      ISUBLAYER(9) =  1
      ISUBLAYER(10) =  3
C----------------------------------------------------------------------
C- Read all RCP input
C----------------------------------------------------------------------
C- Fill MTC_CSFCOM block ...
      CALL INRCP('CSF_RCP',IER)
      IF(IER.EQ.0) THEN
        CALL EZPICK('CSF_RCP')
        CALL EZGET('A',A,IER)
        CALL EZGET('W_CCEM',W_CCEM,IER)
        CALL EZGET('W_ECEM',W_ECEM,IER)
        CALL EZGET('W_CCMG',W_CCMG,IER)
        CALL EZGET('W_ICD',W_ICD,IER)
        CALL EZGET('W_ECMG',W_ECMG,IER)
        CALL EZGET('W_CCFH',W_CCFH,IER)
        CALL EZGET('W_ECIH ',W_ECIH ,IER)
        CALL EZGET('W_ECMH',W_ECMH,IER)
        CALL EZGET('W_CCCH',W_CCCH,IER)
        CALL EZGET('W_ECOH',W_ECOH,IER)
        CALL EZRSET
      ELSE
        WRITE(6,*) ' MTC_FILL_MPV:  CSF_RCP error IER = ',IER
      ENDIF
C- Fill mtc_csficd ...
      CALL INRCP('CSF_ICD_RCP',IER)
      IF(IER.EQ.0) THEN
        CALL EZPICK('CSF_ICD_RCP')
        CALL EZGET('correction_north_lyr_09',CSF_ICD_N,IER)
        CALL EZGET('correction_south_lyr_09',CSF_ICD_S,IER)
        CALL EZRSET
      ELSE
        WRITE(6,*) ' MTC_FILL_MPV:  CSF_ICD_RCP error IER = ',IER
      ENDIF
C- Fill ICD PMT types for each channel ...
      CALL INRCP('ICD_TYPE_RCP',IER)
      IF(IER.EQ.0) THEN
        CALL EZPICK('ICD_TYPE_RCP')
        DO 5 ICHAR=1,768
          CALL EZGETC('AICD_TYPE',ICHAR,4,AICD_TYPE(ICHAR),IER)
    5   CONTINUE
        CALL EZRSET
      ELSE
        WRITE(6,*) ' MTC_FILL_MPV:  ICD_TYPE_RCP error IER = ',IER
      ENDIF

C- Fill mtc_mpvcom.inc with MPV's and widths in ADC counts ...
      call mtc_fill_mpvcom

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- Convert the 1x1 MPV CAL ENERGIES to GeV
C----------------------------------------------------------------------
C- FIRST CONVERT THE MPV TO GEV
C- CCEM
      ICAL=1
      DO 21 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 31 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 21
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_CCEM(ILYR) * A(1)
   31   CONTINUE
   21 CONTINUE
C- ECEM
      ICAL=2
      DO 26 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 36 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 26
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ECEM(ILYR) * A(2)
   36   CONTINUE
   26 CONTINUE
C- CCMG
      ICAL=3
      DO 24 ISUB=1,ISUBLAYER(ICAL)
        DO 34 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 24
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_CCMG(ISETA) * A(3)
   34   CONTINUE
   24 CONTINUE
C- ICD
C- For the ICD, all adcmpv(,,) = 0.003891 x 10
      ICAL=4
      DO 25 ISUB=1,ISUBLAYER(ICAL)
        DO 35 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 25
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ICD(ISETA) * A(4)
   35   CONTINUE
   25 CONTINUE
C- ECMG
      ICAL=5
      DO 20 ISUB=1,ISUBLAYER(ICAL)
        DO 30 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 20
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ECMG(ISETA) * A(5)
   30   CONTINUE
   20 CONTINUE
C- CCFH
      ICAL=6
      DO 22 ISUB=1,ISUBLAYER(ICAL)
        DO 32 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 22
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_CCFH(ISUB) * A(6)
   32   CONTINUE
   22 CONTINUE
C- ECIH
      ICAL=7
      DO 27 ISUB=1,ISUBLAYER(ICAL)
        DO 37 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 27
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ECIH (ISUB) * A(7)
   37   CONTINUE
   27 CONTINUE
C- ECMH
      ICAL=8
      DO 28 ISUB=1,ISUBLAYER(ICAL)
        DO 38 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 38
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ECMH(ISUB) * A(8)
   38   CONTINUE
   28 CONTINUE
C- CCCH
      ICAL=9
      DO 23 ISUB=1,ISUBLAYER(ICAL)
        DO 33 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 23
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_CCCH(ISUB) * A(9)
   33   CONTINUE
   23 CONTINUE
C- ECOH
      ICAL=10
      DO 29 ISUB=1,ISUBLAYER(ICAL)
        DO 39 ISETA=1,22
          IF(ADCMPV(ICAL,ISUB,ISETA).LT.0.) GO TO 29
          GEVMPV(ICAL,ISUB,ISETA) =
     &      ADCMPV(ICAL,ISUB,ISETA) * W_ECIH(ISUB) * A(10)
   39   CONTINUE
   29 CONTINUE
C----------------------------------------------------------------------
C- CONVERT THE 1x1 MPV WIDTHS TO GEV
C----------------------------------------------------------------------
C- CCEM
      ICAL=1
      DO 41 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 51 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 41
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_CCEM(ILYR) * A(1)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   51   CONTINUE
   41 CONTINUE
C- ECEM
      ICAL=2
      DO 46 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 56 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 46
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_ECEM(ILYR) * A(2)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   56   CONTINUE
   46 CONTINUE
C- CCMG
      ICAL=3
      DO 44 ISUB=1,ISUBLAYER(ICAL)
        DO 54 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 44
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_CCMG(ISETA) * A(3)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   54   CONTINUE
   44 CONTINUE
C- ECMG
      ICAL=5
      DO 40 ISUB=1,ISUBLAYER(ICAL)
        DO 50 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 40
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_ECMG(ISETA) * A(5)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   50   CONTINUE
   40 CONTINUE
C- CCFH
      ICAL=6
      DO 42 ISUB=1,ISUBLAYER(ICAL)
        DO 52 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 42
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_CCFH(ISUB) * A(6)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   52   CONTINUE
   42 CONTINUE
C- ECIH
      ICAL=7
      DO 47 ISUB=1,ISUBLAYER(ICAL)
        DO 57 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 47
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_ECIH (ISUB) * A(7)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   57   CONTINUE
   47 CONTINUE
C- ECMH
      ICAL=8
      DO 48 ISUB=1,ISUBLAYER(ICAL)
        DO 58 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 48
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_ECMH(ISUB) * A(8)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   58   CONTINUE
   48 CONTINUE
C- CCCH
      ICAL=9
      DO 43 ISUB=1,ISUBLAYER(ICAL)
        DO 53 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 43
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_CCCH(ISUB) * A(9)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   53   CONTINUE
   43 CONTINUE
C- ECOH
      ICAL=10
      DO 49 ISUB=1,ISUBLAYER(ICAL)
        DO 59 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 49
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      ADCMPV_W(ICAL,ISUB,ISETA) * W_ECIH(ISUB) * A(10)
          GEVMPV_W(ICAL,ISUB,ISETA) = (GEVMPV_W(ICAL,ISUB,ISETA)) **2
   59   CONTINUE
   49 CONTINUE
C----------------------------------------------------------------------
C- ICD
      ICAL=4
      DO 45 ISUB=1,ISUBLAYER(ICAL)
        DO 55 ISETA=1,22
          IF(ADCMPV_W(ICAL,ISUB,ISETA).LT.0.) GO TO 45
          GEVMPV_W(ICAL,ISUB,ISETA) =
     &      .25 * GEVMPV(ICAL,ISUB,ISETA)  
   55   CONTINUE
   45 CONTINUE
C----------------------------------------------------------------------
C- get the tile to tile variation for the ICD ...
C----------------------------------------------------------------------
C- ICD North first
      DO 60 IPHI=1,64
        DO 70 ISETA=1,6
          IETA = ISETA + 8
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
          IF(ATUBE.EQ.'NULL') GO TO 70
          GEV_MPV_ICD(ICOUNT) = GEVMPV(5,1,ISETA) * CSF_ICD_N(ICOUNT)
          IF(ATUBE.EQ.'R647') THEN
            GEV_MPV_ICD_W(ICOUNT) = GEV_MPV_ICD(ICOUNT) * .25
          ELSE IF(ATUBE.EQ.'PM60') THEN
            GEV_MPV_ICD_W(ICOUNT) = GEV_MPV_ICD(ICOUNT) * .33
          END IF
          GEV_MPV_ICD_W(ICOUNT) = (GEV_MPV_ICD_W(ICOUNT)) **2
   70   CONTINUE
   60 CONTINUE
C- ICD South
      DO 61 IPHI=1,64
        DO 71 ISETA=-1,-6,-1
          IETA = ISETA - 8
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
          IF(ATUBE.EQ.'NULL') GO TO 71

          ISETA2 = ABS(ISETA)
          ICOUNT2 = ICOUNT - 384
          GEV_MPV_ICD(ICOUNT) = GEVMPV(5,1,ISETA2) * CSF_ICD_S(ICOUNT2)
          IF(ATUBE.EQ.'R647') THEN
            GEV_MPV_ICD_W(ICOUNT) = GEV_MPV_ICD(ICOUNT) * .25
          ELSE IF(ATUBE.EQ.'PM60') THEN
            GEV_MPV_ICD_W(ICOUNT) = GEV_MPV_ICD(ICOUNT) * .33
          END IF
          GEV_MPV_ICD_W(ICOUNT) = (GEV_MPV_ICD_W(ICOUNT)) **2
   71   CONTINUE
   61 CONTINUE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- Convert the 3X3 MPV CAL ENERGIES in ADC counts to GeV
C----------------------------------------------------------------------
C- FIRST CONVERT THE MPV TO GEV
C- CCEM
      ICAL=1
      DO 321 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 331 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 321
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_CCEM(ILYR) * A(1)
  331   CONTINUE
  321 CONTINUE
C- ECEM
      ICAL=2
      DO 326 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 336 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 326
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ECEM(ILYR) * A(2)
  336   CONTINUE
  326 CONTINUE
C- CCMG
      ICAL=3
      DO 324 ISUB=1,ISUBLAYER(ICAL)
        DO 334 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 324
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_CCMG(ISETA) * A(3)
  334   CONTINUE
  324 CONTINUE
C- ICD
C- For the ICD, all adcmpv3(,,) = 0.003891 x 10
      ICAL=4
      DO 325 ISUB=1,ISUBLAYER(ICAL)
        DO 335 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 325
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ICD(ISETA) * A(4)
  335   CONTINUE
  325 CONTINUE
C- ECMG
      ICAL=5
      DO 320 ISUB=1,ISUBLAYER(ICAL)
        DO 330 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 320
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ECMG(ISETA) * A(5)
  330   CONTINUE
  320 CONTINUE
C- CCFH
      ICAL=6
      DO 322 ISUB=1,ISUBLAYER(ICAL)
        DO 332 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 322
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_CCFH(ISUB) * A(6)
  332   CONTINUE
  322 CONTINUE
C- ECIH
      ICAL=7
      DO 327 ISUB=1,ISUBLAYER(ICAL)
        DO 337 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 327
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ECIH (ISUB) * A(7)
  337   CONTINUE
  327 CONTINUE
C- ECMH
      ICAL=8
      DO 328 ISUB=1,ISUBLAYER(ICAL)
        DO 338 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 328
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ECMH(ISUB) * A(8)
  338   CONTINUE
  328 CONTINUE
C- CCCH
      ICAL=9
      DO 323 ISUB=1,ISUBLAYER(ICAL)
        DO 333 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 323
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_CCCH(ISUB) * A(9)
  333   CONTINUE
  323 CONTINUE
C- ECOH
      ICAL=10
      DO 329 ISUB=1,ISUBLAYER(ICAL)
        DO 339 ISETA=1,22
          IF(ADCMPV3(ICAL,ISUB,ISETA).LT.0.) GO TO 329
          GEVMPV3(ICAL,ISUB,ISETA) =
     &      ADCMPV3(ICAL,ISUB,ISETA) * W_ECIH(ISUB) * A(10)
  339   CONTINUE
  329 CONTINUE
C----------------------------------------------------------------------
C- CONVERT THE 3x3 MPV WIDTHS TO GEV
C----------------------------------------------------------------------
C- CCEM
      ICAL=1
      DO 341 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 351 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 341
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_CCEM(ILYR) * A(1)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  351   CONTINUE
  341 CONTINUE
C- ECEM
      ICAL=2
      DO 346 ISUB=1,ISUBLAYER(ICAL)
        ILYR = ISUB
        IF(ILYR.EQ.4) ILYR = 7
        DO 356 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 346
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_ECEM(ILYR) * A(2)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  356   CONTINUE
  346 CONTINUE
C- CCMG
      ICAL=3
      DO 344 ISUB=1,ISUBLAYER(ICAL)
        DO 354 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 344
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_CCMG(ISETA) * A(3)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  354   CONTINUE
  344 CONTINUE
C- ECMG
      ICAL=5
      DO 340 ISUB=1,ISUBLAYER(ICAL)
        DO 350 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 340
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_ECMG(ISETA) * A(5)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  350   CONTINUE
  340 CONTINUE
C- CCFH
      ICAL=6
      DO 342 ISUB=1,ISUBLAYER(ICAL)
        DO 352 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 342
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_CCFH(ISUB) * A(6)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  352   CONTINUE
  342 CONTINUE
C- ECIH
      ICAL=7
      DO 347 ISUB=1,ISUBLAYER(ICAL)
        DO 357 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 347
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_ECIH (ISUB) * A(7)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  357   CONTINUE
  347 CONTINUE
C- ECMH
      ICAL=8
      DO 348 ISUB=1,ISUBLAYER(ICAL)
        DO 358 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 348
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_ECMH(ISUB) * A(8)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  358   CONTINUE
  348 CONTINUE
C- CCCH
      ICAL=9
      DO 343 ISUB=1,ISUBLAYER(ICAL)
        DO 353 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 343
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_CCCH(ISUB) * A(9)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  353   CONTINUE
  343 CONTINUE
C- ECOH
      ICAL=10
      DO 349 ISUB=1,ISUBLAYER(ICAL)
        DO 359 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 349
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      ADCMPV_W3(ICAL,ISUB,ISETA) * W_ECIH(ISUB) * A(10)
          GEVMPV_W3(ICAL,ISUB,ISETA) = (GEVMPV_W3(ICAL,ISUB,ISETA)) **2
  359   CONTINUE
  349 CONTINUE
C----------------------------------------------------------------------
C- ICD
      ICAL=4
      DO 345 ISUB=1,ISUBLAYER(ICAL)
        DO 355 ISETA=1,22
          IF(ADCMPV_W3(ICAL,ISUB,ISETA).LT.0.) GO TO 345
          GEVMPV_W3(ICAL,ISUB,ISETA) =
     &      .25 * GEVMPV3(ICAL,ISUB,ISETA)
  355   CONTINUE
  345 CONTINUE
C----------------------------------------------------------------------
C- get the tile to tile variation for the ICD ...
C----------------------------------------------------------------------
C- ICD North first
      DO 360 IPHI=1,64
        DO 370 ISETA=1,6
          IETA = ISETA + 8
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
          IF(ATUBE.EQ.'NULL') GO TO 370
          GEV_MPV_ICD3(ICOUNT) = GEVMPV3(5,1,ISETA)*CSF_ICD_N(ICOUNT)
          IF(ATUBE.EQ.'R647') THEN
            GEV_MPV_ICD_W3(ICOUNT) = GEV_MPV_ICD3(ICOUNT) * .25
          ELSE IF(ATUBE.EQ.'PM60') THEN
            GEV_MPV_ICD_W3(ICOUNT) = GEV_MPV_ICD3(ICOUNT) * .33
          END IF
          GEV_MPV_ICD_W3(ICOUNT) = (GEV_MPV_ICD_W3(ICOUNT)) **2
  370   CONTINUE
  360 CONTINUE
C- ICD South
      DO 361 IPHI=1,64
        DO 371 ISETA=-1,-6,-1
          IETA = ISETA - 8
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
          IF(ATUBE.EQ.'NULL') GO TO 371

          ISETA2 = ABS(ISETA)
          ICOUNT2 = ICOUNT - 384
          GEV_MPV_ICD3(ICOUNT) = GEVMPV3(5,1,ISETA2)*CSF_ICD_S(ICOUNT2)
          IF(ATUBE.EQ.'R647') THEN
            GEV_MPV_ICD_W3(ICOUNT) = GEV_MPV_ICD3(ICOUNT) * .25
          ELSE IF(ATUBE.EQ.'PM60') THEN
            GEV_MPV_ICD_W3(ICOUNT) = GEV_MPV_ICD3(ICOUNT) * .33
          END IF
          GEV_MPV_ICD_W3(ICOUNT) = (GEV_MPV_ICD_W3(ICOUNT)) **2
  371   CONTINUE
  361 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
