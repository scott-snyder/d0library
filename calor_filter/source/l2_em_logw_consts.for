      SUBROUTINE L2_EM_LOGW_CONSTS(E_EM,IETAC,W_PHI,W_ETA,SIZE_POSITION,
     &  SIG_ETA_CM,SIG_PHI_CM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given E of candidate and IETAC, return the 
C-   log weights and the size of cluster to use for position determination
C-      Constants from Natalie Roe
C-       ********presently recycling constants from CC for EC***************
C-
C-   Inputs  : E_EM   Energy (not Et) of cluster
C-             IETAC  offline eta coordinate of cell with cluster peak
C-   Outputs : W_PHI  phi weight offset for log algorithm
C-             W_ETA  same for eta direction measurement
C-             SIZE_POSITION   1 means use central em3 cell +/- 1 (3x3) 
C-                             2 is 5x5 EM3 
C-             SIG_ETA_CM estimated resolution in cm in eta direction
C-             SIG_PHI_CM estimated resolution in cm in phi direction
C-                taken as worse of fit sigma, or rms of distribution
C-   Controls: none
C-
C-   Created  22-AUG-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
       IMPLICIT NONE
       INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
       INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
       INCLUDE 'D0$INC:CL2_RINGS.INC'
       INTEGER IETAC,EBIN,ETABIN,I
       REAL E_EM,W_PHI,W_ETA,SIG_ETA_CM,SIG_PHI_CM
       INTEGER N_EBINS_CC,N_EBINS_EC,N_ETABINS_CC,N_ETABINS_EC
       PARAMETER( N_EBINS_CC = 5 )
       PARAMETER( N_EBINS_EC = 1 )
       PARAMETER( N_ETABINS_CC = 3 )
       PARAMETER( N_ETABINS_EC = 6 )
       REAL E_BOUNDS_CC(N_EBINS_CC)
       INTEGER IETA_BOUNDS_CC(N_ETABINS_CC)
       INTEGER IETA_BOUNDS_EC(N_ETABINS_EC)
       REAL W0_PHI_CC(N_EBINS_CC,N_ETABINS_CC)
       REAL W0_ETA_CC(N_EBINS_CC,N_ETABINS_CC)
       REAL W0_PHI_EC(N_EBINS_EC,N_ETABINS_EC)
       REAL W0_ETA_EC(N_EBINS_EC,N_ETABINS_EC)
       REAL SIG_ETA_CM_CC(N_EBINS_CC,N_ETABINS_CC)
       REAL SIG_PHI_CM_CC(N_EBINS_CC,N_ETABINS_CC)
       REAL SIG_FIT_CM_EC(2)
       INTEGER SIZE_POSITION ! how much of EM3 array to use for position calc
       LOGICAL FIRST
       SAVE E_BOUNDS_CC,IETA_BOUNDS_CC,IETA_BOUNDS_EC
       SAVE W0_PHI_CC,W0_ETA_CC,W0_PHI_EC,W0_ETA_EC,
     & SIG_ETA_CM_CC,SIG_PHI_CM_CC,SIG_FIT_CM_EC
       DATA FIRST/.TRUE./
       DATA E_BOUNDS_CC/15., 25., 40.,75., 99999./
C
       DATA IETA_BOUNDS_CC/  2,  6,  14/
       DATA IETA_BOUNDS_EC/ 17, 20,23,26,29,37 /
C
C...Natalie's values for W0 from CC test beam
       DATA W0_PHI_CC/    4.2,  4.3,  4.5,  4.7, 4.7, 
     &                    4.2,  4.4,  4.6,  4.6, 4.6,
     &                    4.4,  4.0,  4.0,  5.0, 5.2/
       DATA W0_ETA_CC/    4.0,  4.3,  4.5,  4.7, 4.7, 
     &                    3.2,  3.4,  3.4,  3.4, 3.4,
     &                    3.0,  2.8,  2.6,  2.6, 2.4/ 
       DATA W0_PHI_EC/    5.3,  4.9,  4.5,  4.3, 4.9, 4.5/
       DATA W0_ETA_EC/    4.0,  3.6,  3.4,  3.2, 3.2, 3.2/      
C
C
       DATA SIG_PHI_CM_CC/ .70,  .50,  .30,  .30, .30,
     &                     .70,  .50,  .30,  .30, .30,
     &                     1.0,  .70,  .30,  .30, .30/
       DATA SIG_ETA_CM_CC/ .70,  .50,  .30,  .30, .30,
     &                     .80,  .50,  .40,  .40, .40,
     &                     2.0,  1.0,  1.0,  1.0, 1.0/
C For ECEM fit to resolution using A/sqrt(E)  + B , in cm
C note that constant does not take into account systematic offset in r
C this must be corrected for in the position finding routine
C also does not take into account alignment errors
       DATA SIG_FIT_CM_EC/1.2,0.05/
C----------------------------------------------------------------------
 
C
       IF (FIRST) THEN
C
C...THIS SHOULD NOT BE IN A .FIRST. FOR ONLINE USE
C        CALL EZPICK('LEVEL2_RCP')  ! OR SOMETHING LIKE THAT
C        CALL EZGET('W0_PHI',W0_PHI,IER)
C        CALL EZGET('W0_ETA',W0_ETA,IER)
C        CALL EZRSET
         FIRST = .FALSE.
       ENDIF
 
C----------------------------------------------------------------------
C
C
       IF (ABS(IETAC).LE.14) THEN  ! CC
C...deterimine how big a neighborhood to use.
         SIZE_POSITION = 1 ! 1 => use 3x3 EM3 cells
C Determine E, eta bins for W0; count down to lowest bin it fits in
         ETABIN = N_ETABINS_CC 
         DO I = N_ETABINS_CC-1,1,-1
           IF(IABS(IETAC).LE.IETA_BOUNDS_CC(I)) ETABIN =  I 
         ENDDO
         EBIN = N_EBINS_CC
         DO I = N_EBINS_CC-1,1,-1
           IF(E_EM.LE.E_BOUNDS_CC(I)) EBIN = I
         ENDDO
C
C...Get W0 (log weighting constant) and error estimate  for selected bins
         W_PHI  = W0_PHI_CC(EBIN,ETABIN) 
         W_ETA  = W0_ETA_CC(EBIN,ETABIN) 
         SIG_ETA_CM = SIG_ETA_CM_CC(EBIN,ETABIN)
         SIG_PHI_CM = SIG_PHI_CM_CC(EBIN,ETABIN)
         IF (EBIN.EQ.1) THEN
C
C...if in lowest E bin, inflate error estimate by root E
           SIG_ETA_CM = SIG_ETA_CM*SQRT(E_BOUNDS_CC(1)/E_EM)
           SIG_PHI_CM = SIG_PHI_CM*SQRT(E_BOUNDS_CC(1)/E_EM)
         ENDIF
       ELSE  ! EC
C...deterimine how big a neighborhood to use.
         SIZE_POSITION = 2 ! 2 => use 5x5 EM3 cells:
C                           when cells coarsen, only 0, +/- 2 exist
C Determine E, eta bins for W0; count down to lowest bin it fits in
         ETABIN = N_ETABINS_EC
         DO I = N_ETABINS_EC-1,1,-1
           IF(IABS(IETAC).LE.IETA_BOUNDS_EC(I)) ETABIN = I
         ENDDO
         EBIN = 1
C
C...Get W0 (log weighting constant) and error estimate  for selected bins
         W_PHI  = W0_PHI_EC(EBIN,ETABIN) 
         W_ETA  = W0_ETA_EC(EBIN,ETABIN) 
C SCALE ENERGY DEPENDENCE WITH 1/SQRT(E) + CONSTANT
         SIG_ETA_CM = SIG_FIT_CM_EC(1)*SQRT(1/E_EM) + 
     &                SIG_FIT_CM_EC(2)
C INFLATE ERROR FOR ETA LT 20
         IF(IABS(IETAC).LT.20)
     &   SIG_ETA_CM= (1.+(20-IABS(IETAC))*0.1) * SIG_ETA_CM 
         SIG_PHI_CM = SIG_ETA_CM
        ENDIF
C
C
C----------------------------------------------------------------------
  999  RETURN
       END
