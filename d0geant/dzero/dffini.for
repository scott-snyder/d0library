      SUBROUTINE DFFINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define GLOBAL switches. Switching routine for 
C-                         FFREAD cards.
C-                         Called by UGINIT
C-
C-   Inputs  : /D0LOG/ D0 logical flags
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated   6-DEC-1988   A.M.Jonckheere  Add LV0 
C-   Updated   7-JUL-1989   Harrison B. Prosper
C-   Keep only detector-independent code
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
C ****  LOGICAL UNIT NUMBER FOR ZEBRA I/O.....    S.KUNORI 10-MAR-86
C
      INCLUDE 'D0$INC:ZEBIO.INC/LIST'
      INCLUDE 'D0$INC:ZEBIOG.INC'
      INTEGER KK
C----------------------------------------------------------------------
C
      CALL FFINIT(500)
C
C
C ****  Setup GENERAL switches
C
C
C BATCH/INTERACTIVE - LOGICAL
      LBAT = .FALSE.
      CALL FFKEY ('LBAT',LBAT,1,' ')
C
C SHOWER GENERATION
      SHWG = 1        ! 0=GHEISHA, =1 PARAMETRISATION, =2 FROZEN SHOWER
      CALL FFKEY('SHWG',SHWG,1,'INTEGER')
C
C SAVE SECONDARIES IN MCEN INTO JTRAK/JVERT BANKS
      SSEC = .200
      CALL FFKEY('SSEC',SSEC,1,'REAL')
C
C UTILITY FLAGS - GLOBAL
      DO KK = 1, 9
        SD0(KK) = 0
      ENDDO
      CALL FFKEY('SD0 ',SD0,9,'REAL')
C
C MASTER PRINT/DRAW
      PD0 = 1         ! Minimal Printing
      CALL FFKEY('PD0 ',PD0,1,'INTEGER')
      DTRK = 0        ! No printing of tracks
      CALL FFKEY('DTRK',DTRK,1,'INTEGER')
      DHIT = 1        ! Print hits
      CALL FFKEY('DHIT',DHIT,1,'INTEGER')
      DDIG = 1        ! Print digitization
      CALL FFKEY('DDIG',DDIG,1,'INTEGER')
C                                               
C BLUE BOOK INTERATION LENGTHS                  
C if non zero do interaction lengths using RRLAM
      DLAM = 0                                  
      CALL FFKEY('DLAM',DLAM,1,'INTEGER')       
C
C I/O
C - ZEBRA
      IRDCHR = 'IU'
      IWRCHR = 'OU'
      IRDFLN = ' '
      IWRFLN = ' '
      CALL FFKEY('IMOD',LRDCHR,1,'INTEGER')
      CALL FFKEY('OMOD',LWRCHR,1,'INTEGER')
      CALL FFKEY('IFIL',LRDFLN,20,'MIXED')
      CALL FFKEY('OFIL',LWRFLN,20,'MIXED')
      IRDUNI=0
      IWRUNI=0
      DO KK=1,8
        IZBOPT(KK)=0
      ENDDO
      IRDREC=0
      IWRREC=0        !This makes 11. original vzero was for 10 vars.
      CALL FFKEY('ZBIO',IRDUNI,10,'INTEGER')
C
C RKIN=N WILL READ EVENT KINEMATICS FROM LOGICAL UNIT N.
C RKIN=0 WILL MAKE THE KINE CARD EFFECTIVE.
      RKIN = 0
      CALL FFKEY('RKIN',RKIN,1,'INTEGER')
C
      ENTRY PRT_DFFINI
C
      WRITE(LOUT,*)
     &    ' **** GEANT FREE FORMAT SWITCH PROCESSOR ****'
C
      WRITE(LOUT,1) LBAT,SHWG,SSEC,SD0,DTRK,DHIT,DDIG,RKIN,IRDUNI,
     &  IWRUNI,PD0,DLAM
    1 FORMAT(
     & ' LBAT',L3,16X,  '(BATCH IF TRUE)'/
     & ' SHWG',I3,16X,  '(GHEISHA=0, SHOWER PARAM=1,',
     & ' SHLIB-CD=2,SHLIB-NOCD=3)'/
     & ' SSEC',F5.3,14X,'(SAVE SECONDARYS>=0, DON''T SAVE<0)'/
     & '      SD0 ',9F6.2/
     & ' DTRK',I3,' DHIT',I3,' DDIG',I3,' (0=OFF,1=PRINT,2=DRAW)'/
     & ' RKIN',I3,16X,  '(.GT.0: EVENT FROM ASCII FILE)'/
     & ' ZBIO',2I3,13X, '(ZEBCOM INPUT/OUTPUT UNIT #''S (0=OFF))'/
     & ' PD0 ',I3,16X,  '(PRINT LEVEL)'/
     & ' DLAM',I3,16X,  '(1->BLUE BOOK ABSORPTION LENGTHS)' )
C
      WRITE(LOUT,*) ' **** Package Specific Switches ****'
C
  999 RETURN
      END
