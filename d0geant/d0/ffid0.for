      LOGICAL FUNCTION FFID0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Master switches for D0 packages defined here
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  30-JUL-1987   A.M.Jonckheere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated  18-JUL-1989   Harrison B. Prosper  
C-   Moved definition of general switches to DFFINI. Keep only definition
C-   of DCAL, DCEN, DMUO and DLV0 and corresponding print flags.
C-   Updated  21-MAR-1991   Andrei Kiryunin   
C-   Include switches for SAMUS      
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'         ! RUN TIME SWITCHES
      LOGICAL PRT_FFID0
C----------------------------------------------------------------------
      FFID0  = .TRUE.
C
C CENTRAL DETECTOR - MASTER
      DCEN = 3
      CALL FFKEY('DCEN',DCEN,1,'INTEGER')
      PCEN = 1
      CALL FFKEY('PCEN',PCEN,1,'INTEGER')
C
C CALORIMETER - MASTER
      DCAL = 3
      CALL FFKEY('DCAL',DCAL,1,'INTEGER')
      PCAL = 1
      CALL FFKEY('PCAL',PCAL,1,'INTEGER')
C
C MUON SYSTEM - MASTER
      DMUO = 3
      CALL FFKEY('DMUO',DMUO,1,'INTEGER')
      PMUO = 1
      CALL FFKEY('PMUO',PMUO,1,'INTEGER')
C
C SAMUS SYSTEM - MASTER
      DSAM = 3
      CALL FFKEY('DSAM',DSAM,1,'INTEGER')
      PSAM = 1
      CALL FFKEY('PSAM',PSAM,1,'INTEGER')
C
C Level 0 SYSTEM - MASTER
      DLV0 = 3
      CALL FFKEY('DLV0',DLV0,1,'INTEGER')
      PLV0 = 1
      CALL FFKEY('PLV0',PLV0,1,'INTEGER')
C
C
      ENTRY PRT_FFID0
C
      PRT_FFID0 = .TRUE.
      WRITE(LOUT,1)     DCEN,DCAL,DMUO,DLV0,DSAM,
     &                  PCEN,PCAL,PMUO,PLV0,PSAM
    1 FORMAT(
     + ' DCEN',I3,' DCAL',I3,' DMUO',I3,' DLV0',I3,' DSAM',I3,
     +           ' (0=OFF,1=DEAD,2=HITS,3=DIG,4=ANALY)'/
     + ' PCEN',I3,' PCAL',I3,' PMUO',I3,' PLV0',I3,' PSAM',I3)
C
  999 RETURN
      END
