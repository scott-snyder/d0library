      SUBROUTINE PULSER_CORR(LCLUS,CORRFACT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Corrects EM clusters for pulser variations
C-
C-   Inputs  : LCASH      LPELC OR LPPHO
C-   Outputs : CORRFACT   multiplicative correction factor
C-   Controls: 
C-
C-   Created  12-JAN-1993   Norman A. Graf
C-   Updated  18-FEB-1994   Marcel Demarteau  Changed constant for pulser
C-                          swapped during shut down and added time dep. 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL ECLUS,ECORR,ENERGY, CORR, CORRFACT
      REAL PULSCORR(0:11)
      REAL PLSR1B
      INTEGER NUMRUN,NUMEVT 
      INTEGER I,NCH,POINTER,IOK,IER
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD,LCASH,LCACL,LCLUS
      DATA PULSCORR /0.9949, 0.9924, 0.9996, 0.9991, 
     &               1.0291, 1.0090, 1.0008, 1.0065, 
     &               0.9918, 0.9970, 0.9932, 0.9862/
C
      DATA PLSR1B/0.987/
C----------------------------------------------------------------------
C
      CORRFACT=1.
      CALL EVNTID(NUMRUN,NUMEVT)
C
      IF(NUMRUN.LE.65136) THEN 
        PULSCORR(4) = 6.094E-7 * NUMRUN + 0.995 
      ELSEIF(NUMRUN.GT.65136 .AND. NUMRUN.LT.66932) THEN
        PULSCORR(4) = 1.035
      ENDIF
C
      IF(NUMRUN.GE.66932 .AND. NUMRUN.LT.74360) PULSCORR(4) = PLSR1B
      IF(NUMRUN.GE.74360) THEN 
        CALL errmsg('Runs above 74360 have CAHITS pulser correction',
     &    'correctem','Turn off Pulser correction','w')
        GOTO 999            !pulser corrections in CALIB database
      ENDIF
C
      LCACL = LQ(LCLUS-2)
      LCASH = LQ(LCACL-2)
      IF(LCASH.EQ.0) THEN 
        CORRFACT=1.
        GOTO 999
      ENDIF
      NCH    = IQ(LCASH+2)
      ECLUS  = 0
      ECORR  = 0 
      POINTER=1
      DO I = 1,NCH
        POINTER = POINTER+2
        PACKED_WORD = IQ(LCASH+POINTER)
        ENERGY = Q(LCASH+POINTER+1)
        CALL CAEP_INDICES(PACKED_WORD,ETAI,PHII,ILYR)
        IF(ABS(ETAI).LT.13) THEN  !  CC  EM
          IF(ETAI.LT.0) THEN      !  CCN EM
             IF(PHII.LE.16)                    CORR = PULSCORR(9)
             IF(PHII.GE.17 .AND. PHII .LE. 48) CORR = PULSCORR(2)
             IF(PHII.GE.49 .AND. PHII .LE. 64) CORR = PULSCORR(9)
          ELSE                    !  CCS EM
             IF(PHII.LE.16)                    CORR = PULSCORR(8)
             IF(PHII.GE.17 .AND. PHII .LE. 48) CORR = PULSCORR(3)
             IF(PHII.GE.49 .AND. PHII .LE. 64) CORR = PULSCORR(8)
          ENDIF
        ELSEIF(ETAI.LT.0)   THEN  !  ECN EM
          IF(PHII.LE.16)                    CORR = PULSCORR(10)
          IF(PHII.GE.17 .AND. PHII .LE. 32) CORR = PULSCORR(1)
          IF(PHII.GE.33 .AND. PHII .LE. 48) CORR = PULSCORR(0)
          IF(PHII.GE.49 .AND. PHII .LE. 64) CORR = PULSCORR(11)
        ELSE                      !  ECS EM
          IF(PHII.LE.16)                    CORR = PULSCORR(7)
          IF(PHII.GE.17 .AND. PHII .LE. 32) CORR = PULSCORR(4)
          IF(PHII.GE.33 .AND. PHII .LE. 48) CORR = PULSCORR(5)
          IF(PHII.GE.49 .AND. PHII .LE. 64) CORR = PULSCORR(6)
        ENDIF
C
        ECLUS = ECLUS + ENERGY
        ECORR = ECORR + CORR*ENERGY
C
      ENDDO
C
      CORRFACT = ECORR/ECLUS
C
  999 RETURN
      END

C
C From:	FNAL::OWEN         "Dan Owen; FNAL:ms357, ext4008; home:393-2388"
C 5-JAN-1993 11:14:07.07
C To:	@CALIB,FNALD0::DEAN,FNALD0::GUIDA,FNALD0::WEERTS
C Subj:	PRELIMINARY correction factors for pulser nonuniformity
C 
C Folks,
C    I list below preliminary results obtained from a set of "pulser
C swapping runs" taken on 12/19 with the pulser amplitude set to the same
C value as used in CALIB.  I list separately the results from EM and Hadron
C channels.  The results are normalized to the average response of all
C pulsers in all preamp boxes.  The first column is the fractional difference
C between that pulser and the average.  The second column is a correction
C factor by which measured energy in that preamp box should be MULTIPLIED to
C correct for the nonuniformity of the pulsers.
C 
C    For your convenience I am also including a table of name conventions to
C help you to go from preamp box name to physics coordinates.
C 
C                                         Dan
C ------------------------------------------------------------------------
C Data from Pulser swapping runs take on 12/19 with CALIB amplitude.
C 
C        Results from EM channels:    Results from Hadron channels: 
C        Del EM 12/19    corr fact    Del HD 12/19    corr fact     
C ECNSW      -0.76%      0.9924           -0.78%      0.9922        
C ECNSE      -0.68%      0.9932           -0.69%      0.9931        
C ECNNE      -1.38%      0.9862           -1.31%      0.9869        
C ECNNW      -0.51%      0.9949           -0.58%      0.9942        
C CCNE       -0.30%      0.9970           -0.38%      0.9962        
C CCNW       -0.04%      0.9996           -0.08%      0.9992        
C CCSE       -0.82%      0.9918           -0.80%      0.9920        
C CCSW       -0.09%      0.9991           -0.01%      0.9999        
C ECSNW       2.91%      1.0291            2.90%      1.0290        
C ECSNE       0.65%      1.0065            0.67%      1.0067        
C ECSSE       0.08%      1.0008            0.13%      1.0013        
C ECSSW       0.90%      1.0090            0.89%      1.0089        
C -------------------------------------------------------------------
C Some Calorimeter name conventions
C Below is a table which indicates which physics regions are serviced by each
C Preamp Box and attendant Pulsers and ADC crates.  It ignores merg regions
C (which do not affect EM channels in any case)
C 
C  phys indicies
C     ETA       PHI      Preamp  ADC Pulser
C ___________ _______     _____  ___  __
C -4.4 - -1.2  1 - 16     ECNSE   57  10
C   "      "  17 - 32      " SW   27   1
C   "      "  33 - 48      " NW   37   0
C   "      "  49 - 64      " NE   47  11
C 
C -1.2 - -0.0 49 - 64      CCNE   17   9
C   "      "   1 - 16       " "    "   "
C   "      "  17 - 48       "NW    7   2
C +0.0 - +1.2 49 - 64      CCSE    8   8
C   "      "   1 - 16       " "    "   "
C   "      "  17 - 48       "SW   18   3
C 
C +1.2 - +4.4  1 - 16     ECSNE   28   7
C   "      "  17 - 32      " NW   58   4
C   "      "  33 - 48      " SW   48   5
C   "      "  49 - 64      " SE   38   6
C 
C
C
CFrom:   FNALD0::JGUIDA
CTo:     DEMARTEAU
CCC:     JGUIDA
CSubj:   pulser correction factor
C
C
C    Hi Marcel,
C
C        I have a new fit to pulser 4 for you.  I'll give you all the
C    results here.  I've normalized to the CCNW pulser.
C
C ****  Note: the CCNW normalization is only done for the fit by time (MD)
C
C    Fit by day (where day 0 = Jan. 1 1992):  (I use this in CAHITS)
C        date is in IC(LCGNH+7)
C        slope = 3.57E-05
C        intercept = 1.017
C
C        after day 500 use a contant value of 1.035
C
C    Fit by run:
C        slope = 6.09E-07
C        intercept = 0.995
C
C        after run 65136 use a contant value of 1.035
C
C
C            Let me know if you need any more information.
C
C                                    Jan
C
