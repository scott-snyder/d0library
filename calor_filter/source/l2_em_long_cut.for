      SUBROUTINE L2_EM_LONG_CUT(IETAC,JENRG_BIN,JETA_BIN,EFLOOR,PHOTON,
     &  TIGHT,ETSUM_EM,RBF,RA1,RA12,RA3,RA4,IFAILED,LONG,GOOD_REGION)
C----------------------------------------------------------------------
C-
C-   Purpose And Methods : make longitudinal shape cuts on EM candidates
C-   Inputs  : IETAC  physics eta of cell (for details of cuts)
C-             JENRG_BIN energy bin number of candidate
C-             JETA_BIN  eta bin of candidate
C-             EFLOOR(1...8) sum in EM1,2,3,4, and FH1, FH2,3 CH, ICD/MG
C-   Outputs : 
C-             ETSUM_EM   sum of floors 1-4 (usually Et; see ET_IN_CAEP)
C-             RBF        FH1 fraction
C-             RA1        EM1 fraction
C-             RA12       EM1+EM2 fraction
C-             RA3        EM3 fraction
C-             RA4        EM4 fraction
C-             IFAILED = 0  a success; otherwise indicates cut failed
C-   Controls: TIGHT if cuts should be done ONLY on primary variables
C-             PHOTON if cuts should be done for a photon, not electron
C-             GOOD_REGION  .FALSE. if in a compromised area: don't cut 
C-             LONG     = .FALSE. if don't want any cutting done
C-             CriteriA for eleCtron identifiCAtion
C-
C-   CreAted 15-SEP-1990   Yi  XiA
C-   Updated  16-DEC-1991   James T. Linnemann  Pull cuts out of filling
C-                                              replace IF's with arrays
C-   Updated 25-FEB-1992   Yi  Xia put cuts (RA1...) as one of the outputs
C-   Updated   7-AUG-1992   James T. Linnemann   TIGHT (identify primary cuts)
C-   Updated  2-SEP-1992   James T. McKinley calculate all variables then do
C-                         cuts in order to fill L2EM bank with all variables
C-   Updated  13-NOV-1993   James T. Linnemann   remove EM1, EM2 from _TIGHT
C-                            because of introduction of EM energy scale with
C-                            offset.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
C
      INTEGER IETAC,JENRG_BIN,JETA_BIN  !indices in energy and eta
      LOGICAL PHOTON,TIGHT,LONG,GOOD_REGION
      INTEGER IFAILED
      REAL ETSUM_EM,E_EM,EFLOOR(8)
      REAL RBF,RA1,RA12,RA3,RA4
      LOGICAL OK
C----------------------------------------------------------------------
      IFAILED = 0               !innocent until proven guilty
C
C...calculate cut variables
C
      ETSUM_EM = EFLOOR(1) + EFLOOR(2) + EFLOOR(3) + EFLOOR(4)
C
      IF(ETSUM_EM.GT.0.)THEN     ! divide by zero protection
        RA3 = EFLOOR(3)/ETSUM_EM ! fraction in layer 3
        RBF = EFLOOR(5)/ETSUM_EM ! fine hadronic 1 fraction
        RA1 = EFLOOR(1)/ETSUM_EM ! layer 1 fraction
        RA12 = (EFLOOR(1)+EFLOOR(2))/ETSUM_EM  !fraction in layer 1+2
        RA4 = EFLOOR(4)/ETSUM_EM ! fraction in layer 4
      ELSE
        IFAILED = 3           ! No EM energy
        GOTO 990
      ENDIF
C
C...now do cuts
      IF( .NOT. GOOD_REGION ) GOTO 999   ! Bug out if in undefined region
      IF( .NOT. LONG ) GOTO 999     !Skip longitudinal cuts if requested
C
C...cut on floor fractions as a function of energy and eta bin
C
      IFAILED = 10
      IF ( RBF .GT. EMCUTS(9,JENRG_BIN,JETA_BIN) ) GO TO 990
C
C

      IFAILED = 15
      IF ( RA3 .LT. EMCUTS(5,JENRG_BIN,JETA_BIN) ) GO TO 990
      IFAILED = 16
      IF ( RA3 .GT. EMCUTS(6,JENRG_BIN,JETA_BIN) ) GO TO 990
C
C...only FH1 and EM3 are primary cuts:  check if through

      IF (.NOT.TIGHT) GO TO 999

        IF (TIGHT) GO TO 997! remove cuts on RA1, RA2 because of delta offset

C
C...now consider cutting on secondary variables: EM 1,2,4

C
C cut only on layers 3,4 for photons
C and do not cut on 1,2 where signals are missing

      IF(.NOT.PHOTON.AND.(ABS(IETAC).NE.14)) THEN
C
        IFAILED = 11
        IF ( RA1 .LT. EMCUTS(1,JENRG_BIN,JETA_BIN) ) GO TO 990
        IFAILED = 12
        IF ( RA1 .GT. EMCUTS(2,JENRG_BIN,JETA_BIN) ) GO TO 990
C
        IFAILED = 13
        IF ( RA12 .LT. EMCUTS(3,JENRG_BIN,JETA_BIN) ) GO TO 990
        IFAILED = 14
        IF ( RA12 .GT. EMCUTS(4,JENRG_BIN,JETA_BIN) ) GO TO 990

      ENDIF
C
C...don't cut where no EM4 signal
  997 CONTINUE
      IF (ABS(IETAC).NE. 12) THEN
        IFAILED = 17
        IF ( RA4 .LT. EMCUTS(7,JENRG_BIN,JETA_BIN) ) GO TO 990
        IFAILED = 18
        IF ( RA4 .GT. EMCUTS(8,JENRG_BIN,JETA_BIN) ) GO TO 990
      ENDIF
C
C
  999 IFAILED = 0 !a success if you come here according to this routine
  990 RETURN
      END
