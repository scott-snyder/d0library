      PROGRAM L1UTIL_EXAMPLE_BLS_FILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a BLS Gain Correction file with the
C-   coefficients set to 1.
C-
C-   Inputs  : none
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  24-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  20-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      L1UTIL_WRITE_BLS_FILE now takes a file name as a
C-                      parameter.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
C
      INTEGER IETAC, IPHIC, ILYRC
C
      DO ILYRC = 1, NLYRL
        DO IPHIC = 1, NPHIL
          DO IETAC = 1, NETAL
            BLS_GAIN_CORRECTION( IETAC, IPHIC, ILYRC) = 1.
            BLS_GAIN_CORRECTION(-IETAC, IPHIC, ILYRC) = 1.
          END DO
        END DO
      END DO
C
C A call to MZEBRA must occur some time before L1UTIL_WRITE_BLS_FILE is called.
C 
      CALL MZEBRA(0)
      CALL L1UTIL_WRITE_BLS_FILE('BLS.DAT')
C----------------------------------------------------------------------
      END
