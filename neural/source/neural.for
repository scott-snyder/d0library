      PROGRAM NEURAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RUN JETNET - TO TRAIN & TEST NEURAL NETS
C-
C-   Controls: NEURAL_RCP
C-
C-   Created   9-JAN-1991   Chip Stewart
C-   Updated  20-JAN-1993   Harrison B. Prosper  
C-      Get NTUPLE_ID from RCP file; Default ID = 2
C-   Updated   7-MAR-1995   Harrison B. Prosper  
C-      Implement event weighting
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
C----------------------------------------------------------------------
      INTEGER STATUS
      LOGICAL DO_TRAIN,DO_TEST
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL INPAWC
      CALL HMDIR('//PAWC/TEMP',' ')
C----------------------------------------------------------------------
C
      CALL INRCP ('NEURAL_RCP',STATUS)
      IF(STATUS.NE.0) CALL ERRMSG('NEURAL','INRCP',
     & 'No NEURAL_RCP file','F')
      CALL INRCPE('NEURAL_RCPE',STATUS)     ! read overwrite file (RCPE)
      IF(STATUS.EQ.0) CALL ERRMSG('NEURAL','INRCPE',
     & 'Default NEURAL_RCP modified by NEURAL_RCPE','W')
C
      CALL EZPICK('NEURAL_RCP')
      CALL EZGET('DO_TRAIN',DO_TRAIN,STATUS)               
      CALL EZGET('DO_TEST',DO_TEST,STATUS)               
      CALL EZRSET
C----------------------------------------------------------------------
C
      IF ( DO_TRAIN ) THEN
        CALL NEURAL_TRAIN
      ENDIF
C
      IF ( DO_TEST  ) THEN
        CALL NEURAL_TEST
      ENDIF
C----------------------------------------------------------------------
      END
