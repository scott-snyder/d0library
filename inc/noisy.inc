C----------------------------------------------------------------------
C-
C-   Created   8-AUG-1991   Rajendran Raja
C-   MODIFIED  7-JAN-1992   Allen I. Mincer
C-                          Now includes all parameters for
C-                          doing 2 level pileup
C-   MODIFIED  8-MAR-1993   Allen I. Mincer
C-                          Include switch to merge fixed number of
C-                          events and the number
C-   MODIFIED  7-SEP-1993   Ian Adam
C-    Add zero-suppression parameters
C-   MODIFIED  28-JAN-1994  Allen I. Mincer  add tracking pileup
C-   MODIFIED  29-NOV-1994  Allen I. Mincer  
C----------------------------------------------------------------------
      INTEGER KMAX
      PARAMETER (KMAX=104)
      COMMON /NOISY/ DO_PRESENT,DO_FUTURE,DO_SIGNAL,DO_ELECTRONICS,
     &  DO_URANIUM,DO_PAST,DO_PILE,NUMBPILE,NUMBSWITCH,
     &  DO_TRIG12,DO_PILE1,DO_PRESENT1,DO_FUTURE1,DO_SIGNAL1,DO_PAST1,
     &  OCCUPY,DOUBLE_VERTEX,DO_ZSP,N_SIGMA,CELL_SYMCUT,CELL_LOWCUT,
     &  CELL_EN(-NETAL:NETAL,NPHIL,NLYRL),WEIGHT(0:KMAX),KFIN,KPRES,
     &  IWBUCK(0:KMAX),DROP_OLD_CAD,SAVE_LEVEL1,USE_LEVEL1,
     &  RAN_SEED,RAN_VALUE,NOISE_AVG,NOISE_SIGMA,BASE_SUBTRACT,
     &  T_SAMP,T_CROSS,T_DECAY,USE_EXPSIG,USE_GAU_NOISE,USE_EMP_NOISE,
     &  HEADLINE,PRINT_OUTPUT,OUTUNI,
     &  TP(50),AP(50),INTEG(21),FRACTION(100),VAL_MIN,
     &  DO_GEOCUT,NLAYMIN,NLAYMAX,NPHIMIN,NPHIMAX,NETAMIN,NETAMAX,
     &  FIXNUM,DROP_SIG_CAD,DO_TRACK_PILEUP,CDDN_LEVEL,DROP_OLD_BANKS,
     &  DROP_ZERO_ADC
C
      LOGICAL DO_PRESENT,DO_FUTURE,DO_SIGNAL,DO_ELECTRONICS
      LOGICAL DO_URANIUM,DO_PAST,DO_PILE,USE_GAU_NOISE,USE_EMP_NOISE
      LOGICAL BASE_SUBTRACT,USE_EXPSIG,PRINT_OUTPUT
      LOGICAL DO_TRIG12,DO_PILE1,DROP_OLD_CAD,SAVE_LEVEL1,USE_LEVEL1
      LOGICAL DO_PRESENT1,DO_FUTURE1,DO_SIGNAL1,DO_PAST1,DO_GEOCUT
      LOGICAL DO_TRACK_PILEUP
      LOGICAL DROP_SIG_CAD,DO_ZSP
      LOGICAL CDDN_LEVEL,DROP_OLD_BANKS,DROP_ZERO_ADC
      INTEGER NUMBPILE,NUMBSWITCH,RAN_SEED,RAN_VALUE
      INTEGER OUTUNI,KFIN,KPRES,IWBUCK,FIXNUM
      INTEGER NLAYMIN,NLAYMAX,NPHIMIN,NPHIMAX,NETAMIN,NETAMAX
      REAL    OCCUPY,DOUBLE_VERTEX,CELL_SYMCUT,CELL_LOWCUT
      REAL    CELL_EN,WEIGHT,NOISE_AVG,NOISE_SIGMA,VAL_MIN
      REAL    T_SAMP,T_CROSS,T_DECAY,TP,AP,INTEG,FRACTION
      REAL    N_SIGMA
      CHARACTER*60 HEADLINE
C
C switches in NOISY_RCP:
C
C DO_PRESENT, DO_FUTURE, DO_SIGNAL, DO_ELECTRONICS,DO_URANIUM
C DO_PAST, DO_PILE, BASE_SUBTRACT, USE_EXPSIG, PRINT_OUTPUT
C USE_GAU_NOISE,USE_EMP_NOISE
C DO_TRIG12 Is the switch for doing two passes for TRIGGERS
C                      Level 1 and 2
C DO_PILE1, DO_PAST1, DO_PRESENT1, DO_FUTURE1, DO_SIGNAL1
C      are parameter switches used if DO_TRIG12 is TRUE
C DO_TRACK_PILEUP adds tracking of extra events in signal bucket.
C DROP_SIG_CAD drops signal file CAD before using banks
C DROP_OLD_CAD drops original CAD banks after adding pileup if TRUE
C SAVE_LEVEL1 keeps the level1 CAD banks.  If USE_LEVEL1 is 
C             FALSE, they will be pointed to by LQ(LEVEL2 banks)
C USE_LEVEL1 makes CAD prime banks the ouput of LEVEL1 NOISY
C            instead of the default LEVEL 2 noisy (for debug)
C DO_GEOCUT if TRUE, use only part of detector as given by the bounds
C           NLAYMIN,NLAYMAX,NPHIMIN,NPHIMAX,NETAMIN,NETAMAX
C FIXNUM if greater than 0, use this number as the fixed number of 
C           interactions per crossing instead of using OCCUPY
C DO_ZSP if true, do zero-suppression according to noise histograms used
C        to generate the noise,i.e., set to zero any channel with energy
C        less than N_SIGMA*(SIGMA OF THAT CHANNEL).
C CDDN_LEVEL  if true combines CDD banks, else combine HITS banks
C DROP_OLD_BANKS if true, drop pre-pileup tracking banks
C DROP_ZERO_ADC if true, get rid of ADC entries with no hits ( these 
C                exist for data, not for monte carlo)
C
C parameters in NOISY_RCP:
C NUMBPILE, NUMBSWITCH, RAN_SEED, OCCUPY, DOUBLE_VERTEX
C CELL_SYMCUT, CELL_LOWCUT, T_SAMP, T_CROSS, T_DECAY
C NOISE_AVG,NOISE_SIGMA,VAL_MIN,FIXNUM
C N_SIGMA level at which to zero-suppress if DO_ZSP is true.
C
C RAN_VALUE = current random number seed to use in next RAN call
C             This was used when VAX random number generator was in use.
C             This variable is not currently used in the program.
C OUTUNI = output unit for NOISY write statements
C CELL_EN = array whichs keeps accumulated cell energy from
C           all sources ( signal, pileup, noise )
C WEIGHT = fractional contribution of pileup events to current bucket
C IWBUCK = Bucket number of pileup events
C KPRES = number of interactions in current bucket
C KFIN = total number of events (not including signal event) contributing
C        to calorimeter signal
C HEADLINE = title of SPICE output file 
C AP,TP are renormalized amplitude and shifted signal time arrays
C       as calculated using SPICE input.  
C INTEG = integral probabilty distribution of multiple hits
C FRACTION = fractional contribution of pileup events in a bucket
C            ( 1- 79 ) = past buckets ( 79 is 1 before present bucket
C                                       78 is 2 before present bucket ...)
C             (80) = present bucket
C            ( 81-100 ) = future buckets ( 81 is 1 after present ...)
