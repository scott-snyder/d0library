      FUNCTION CHEXXST(CRATE,ADC,BLS,ROTOW,DEPTH)
C----------------------------------------------------------------------
C-
C-   CHEXXST = Calorimeter HEX address eXiST.
C-
C-   Purpose and Methods : Anologous to CEXIST only that input
C-                         arguments are the hex-componet addresses.
C-
C-   Returned value  : .TRUE. if exist, else .FALSE.
C-
C-   Inputs  : CRATE: the crate number      NCRATE*10+BANK where
C-                                          NCRATE=[0:5] and BANK=[7:8]
C-                    In the old system CRATE was: [96:101 112:117]
C-             ADC:   ADC number            [0:1]  |
C-             BLS:   BLS number            [0:7]  |_ CAL_ADC_NO.PARAMS
C-             ROTOW: Readout Tower number  [0:3]  |  specify the range of
C-             DEPTH: Radial index in tower [0:11] |  validity.
C-
C-   Outputs : CHEXXST is a LOGICAL FUNCTION.
C-
C-   Controls: None.
C-
C-   Created  13-SEP-1989   Dale A. Ross, MSU
C-   Modified 31-AUG-1990       ''      , MSU for new crate values.
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS/LIST'
*
C     Passed Variables:
*
         LOGICAL CHEXXST
         INTEGER CRATE,ADC,BLS,ROTOW,DEPTH
*
C     Local Variables:
*
      INTEGER CN,CI,AD,BL,RO,DE,IETAC,IPHIC,LAYER,ICOND
      INTEGER  ONEDGT,TENDGT
      LOGICAL HXST(6:7,0:(NADCRC-1),0:(NADCC-1),
     &             0:(NBLSC-1),0:(NEFC-1),0:(NDEPTC-1))
      LOGICAL CALLED,CEXIST
      SAVE CALLED,HXST
      DATA CALLED/.FALSE./
*
C     =================================================================
*
      IF (.NOT.CALLED) THEN ! Initialize and construct the table.
*
        DO CI = 0,(NADCRC-1) ! cylcle through crate index;
          DO CN = ADCR00,ADCR01 ! cycle through the cables
            DO AD = 0,(NADCC-1) ! cycle through ADC's;
              DO BL = 0,(NBLSC-1) ! cycle through BLS's;
                DO RO = 0,(NEFC-1) ! cycle throucgh ROT's;
                  DO DE = 0,(NDEPTC-1) ! cycle through DEPTH's
                    HXST(CN,CI,AD,BL,RO,DE) = .FALSE.
                    CALL CADPH((10*CI+CN),AD,BL,RO,DE,IETAC,IPHIC,
     &                         LAYER,ICOND)
                    IF (ICOND .EQ. 0) THEN
                      IF (CEXIST(IETAC,IPHIC,LAYER))
     &                   HXST(CN,CI,AD,BL,RO,DE) = .TRUE.
                    ENDIF
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO
        CALLED = .TRUE.
      ENDIF
*
C     Now look up in the table.
*
      CHEXXST = .FALSE.
      ONEDGT = MOD(CRATE,10)
      TENDGT = MOD(CRATE/10,10)
      IF ( ONEDGT.LT.ADCR00 .OR. ONEDGT.GT.ADCR01
     &   .OR. TENDGT.GT.(NADCRC-1) .OR. (CRATE/100 .GT.0) ) GOTO 999
C     --- The next two lines are good only under the old crate ---
C     --- numbring system. the prior two lines replace them.   ---
C     IF (CRATE.LT.ADCR00  .OR. (CRATE.GE.ADCR00+NADCRC .AND.
C    &    CRATE.LT.ADCR01) .OR.  CRATE.GE.ADCR01+NADCRC) GOTO 999
      IF (ADC.LT.0 .OR. ADC.GE.NADCC) GOTO 999
      IF (BLS.LT.0 .OR. BLS.GE.NBLSC) GOTO 999
      IF (ROTOW.LT.0 .OR. ROTOW.GE.NEFC) GOTO 999
      IF (DEPTH.LT.0 .OR. DEPTH.GE.NDEPTC) GOTO 999
*
      CHEXXST = HXST((CRATE/16),(IAND(CRATE,15)),ADC,BLS,ROTOW,DEPTH)
*
  999 RETURN
      END
