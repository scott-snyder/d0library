      SUBROUTINE SAVE_TOP_SOLUTION
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAVE TOP SOLUTION IN FORM OF NTUPLE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      REAL    BUFFER(200)
      INTEGER IS1,IS2,IOFF,SOLM
      INTEGER IND
      REAL DEL_MASS_MAX
      DOUBLE PRECISION TMASS,WT
C
      INTEGER IER
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL DO_HBOOK('CONFIG_NTUPLE')
        CALL EZGET('DEL_MASS_MAX',DEL_MASS_MAX,IER)
        CALL EZRSET
      ENDIF
      BUFFER(1) = RUNC
      BUFFER(2) = EVENTC
      BUFFER(3) = CONFIG
      BUFFER(4) = IPHI
C
      IOFF = 4
      CALL UCOPYDS(LEPTON1,BUFFER(IOFF+1),4)
      CALL UCOPYDS(LEPTON2,BUFFER(IOFF+5),4)
      CALL UCOPYDS(JET1,BUFFER(IOFF+9),4)
      CALL UCOPYDS(JET2,BUFFER(IOFF+13),4)
      CALL UCOPYDS(JET3,BUFFER(IOFF+17),4)
      CALL UCOPYDS(PNUT,BUFFER(IOFF+21),2)
C
      BUFFER(IOFF+23) = PHI_MIN
      BUFFER(IOFF+24) = LAMBDA_LO
      BUFFER(IOFF+25) = LAMBDA_HI
      IOFF = IOFF + 25
      DO IS2  = 1 , 2
        DO IS1 = 1 , 2 !1ST SOLUTION OUT 1ST
          BUFFER(IOFF+1) = WEIGHT(IS1,IS2)
          BUFFER(IOFF+2) = LAMBDA_MIN(IS1,IS2)
          SOLM = 0.0
          IF ( SOL_MIN(IS1,IS2) ) THEN
            SOLM = 1.0
          ENDIF
          BUFFER(IOFF+3) = SOLM
          BUFFER(IOFF+4) = DEL_MASS(IS1,IS2)
          CALL UCOPYDS(PNUT1_MIN(1,IS1,IS2),BUFFER(IOFF+5),2)
          CALL UCOPYDS(W1_MIN(1,IS1,IS2),BUFFER(IOFF+7),4)
          CALL UCOPYDS(TOP1_MIN(1,IS1,IS2),BUFFER(IOFF+11),5)
          CALL UCOPYDS(W2_MIN(1,IS1,IS2),BUFFER(IOFF+16),4)
          CALL UCOPYDS(TOP2_MIN(1,IS1,IS2),BUFFER(IOFF+20),5)
          IOFF = IOFF+24
C
          IF ( ABS(DEL_MASS(IS1,IS2)).LT.DEL_MASS_MAX ) THEN
            IND = 101 + 2*(IS1-1) + IS2
            TMASS = 0.5*(TOP1_MIN(5,IS1,IS2)+TOP2_MIN(5,IS1,IS2))
C
            WT = WEIGHT(IS1,IS2)
            CALL DO_HF1D(101,TMASS,WT)
            CALL DO_HF1D(111,TMASS,1.0D0)
            CALL DO_HF1D(IND,TMASS,WT)
            CALL DO_HF1D(IND+10,TMASS,1.0D0)
C
          ENDIF
C
        ENDDO
      ENDDO
!
      CALL DO_HFN('DILEPTON',100,BUFFER)
C
  999 RETURN
      END
C 'RUN'
C 'EVENT'
C 'CONFIG'
C 'IPHI'
C!Now the configuration kinematic quantities
C 'LEPTON11'
C 'LEPTON12'
C 'LEPTON13'
C 'LEPTON14'
C!
C 'LEPTON21'
C 'LEPTON22'
C 'LEPTON23'
C 'LEPTON24'
C!
C 'JET11'
C 'JET12'
C 'JET13'
C 'JET14'
C!
C 'JET21'
C 'JET22'
C 'JET23'
C 'JET24'
C!
C 'JET31'
C 'JET32'
C 'JET33'
C 'JET34'
C!
C 'PNUT1'
C 'PNUT2'
C!Now the four solutions for this PHI of Et 1st neutrino
C
C 'PHI'
C!
C!the notation is as follows. 1st index is particle number. second index
C!is component of vector. 3rd and 4th indices are the solution numbers
C!e.g. W14_12 means the 4th component of W1 in the solution combination 12
Ci.e. 1st solution of 1st W is combined with second solution of second W
C!
C 'WT_11'    !parton model weight for solution
C 'LAMDA_11'
C 'SOL_11'
C 'DELM_11'
C
C 'NUT11_11'
C 'NUT12_11'
C
C 'W11_11'
C 'W12_11'
C 'W13_11'
C 'W14_11'
C
C 'T11_11'
C 'T12_11'
C 'T13_11'
C 'T14_11'
C 'T15_11'
C
C 'W21_11'
C 'W22_11'
C 'W23_11'
C 'W24_11'
C
C 'T21_11'
C 'T22_11'
C 'T23_11'
C 'T24_11'
C 'T25_11'
C!
C
C 'WT_21'    !parton model weight for solution
C 'LAMDA_21'
C 'SOL_21'
C 'DELM_21'
C 'NUT11_21'
C 'NUT12_21'
C
C 'W11_21'
C 'W12_21'
C 'W13_21'
C 'W14_21'
C
C 'T11_21'
C 'T12_21'
C 'T13_21'
C 'T14_21'
C 'T15_21'
C
C 'W21_21'
C 'W22_21'
C 'W23_21'
C 'W24_21'
C
C 'T21_21'
C 'T22_21'
C 'T23_21'
C 'T24_21'
C 'T25_21'
C!
C
C 'WT_12'    !parton model weight for solution
C 'LAMDA_12'
C 'SOL_12'
C 'DELM_12'
C 'NUT11_12'
C 'NUT12_12'
C
C 'W11_12'
C 'W12_12'
C 'W13_12'
C 'W14_12'
C
C 'T11_12'
C 'T12_12'
C 'T13_12'
C 'T14_12'
C 'T15_12'
C
C 'W21_12'
C 'W22_12'
C 'W23_12'
C 'W24_12'
C
C 'T21_12'
C 'T22_12'
C 'T23_12'
C 'T24_12'
C 'T25_12'
C!
C
C 'WT_22'    !parton model weight for solution
C 'LAMDA_22'
C 'SOL_22'
C 'DELM_22'
C 'NUT11_22'
C 'NUT12_22'
C
C 'W11_22'
C 'W12_22'
C 'W13_22'
C 'W14_22'
C
C 'T11_22'
C 'T12_22'
C 'T13_22'
C 'T14_22'
C 'T15_22'
C
C 'W21_22'
C 'W22_22'
C 'W23_22'
C 'W24_22'
C
C 'T21_22'
C 'T22_22'
C 'T23_22'
C 'T24_22'
C 'T25_22'

