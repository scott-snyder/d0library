      SUBROUTINE TOP_FIT_WRITE_RCP(IRO,IEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out RCp file to unit IRO
C-
C-   Inputs  : IRO=OUTPUT UNIT
C-             IEV=EVENT NUMBER
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-MAR-1994   Rajendran Raja
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      INCLUDE 'D0$INC:TOP_FIT_RCP.INC'
      INTEGER IRO,IEV,K
C----------------------------------------------------------------------
        WRITE(IRO,21)IEV
   21   FORMAT('\START TOP_EVENT_',I4.4)
        WRITE(IRO,22)
   22   FORMAT('\SIZE        100       50')
C
C ****  WRITE RCP FILE
C
        WRITE(IRO,1)
    1   FORMAT(' RUN 1000 ')
        WRITE(IRO,2)IEV
    2   FORMAT(' EVENT ',I5)
C
        CALL WRITE_RCP4(IRO,'LEPTON1',EL,4)
        CALL WRITE_RCP4(IRO,'BJET_LEPTON',BLEP,4)
        CALL WRITE_RCP4(IRO,'BJET_HADRON',BHAD,4)
        CALL WRITE_RCP4(IRO,'JET1_W',J1,4)
        CALL WRITE_RCP4(IRO,'JET2_W',J2,4)
        CALL WRITE_RCP4(IRO,'PNUT',NU,2)
C
        WRITE(IRO,11)LEPTON_TYPE
   11   FORMAT(' LEPTON_TYPE ',I5)
C
        WRITE(IRO,12)TMASS,(TLEP(K),K=1,3),CTH_TL,PH_TL,CTH_WL,PH_WL,
     &    (THAD(K),K=1,3),CTH_TH,PH_TH,CTH_WH,PH_WH
   12   FORMAT('\ARRAY INIVAR',/,
     &    15(1X,F6.2/))
        WRITE(IRO,5)
    5   FORMAT(' \END')
C
        CALL WRITE_RCP4(IRO,'WLEP_T',WLEP_T,4)
        CALL WRITE_RCP4(IRO,'BLEP_T',BLEP_T,4)
        CALL WRITE_RCP4(IRO,'WLEP',WLEP,4)
        CALL WRITE_RCP4(IRO,'BLEP',BLEP,4)
C
        CALL WRITE_RCP4(IRO,'WHAD_T',WHAD_T,4)
        CALL WRITE_RCP4(IRO,'BHAD_T',BHAD_T,4)
        CALL WRITE_RCP4(IRO,'WHAD',WHAD,4)
        CALL WRITE_RCP4(IRO,'BHAD',BHAD,4)
C
        CALL WRITE_RCP4(IRO,'J1_W',J1_W,4)
        CALL WRITE_RCP4(IRO,'J2_W',J2_W,4)
C
        CALL WRITE_RCP4(IRO,'REST',REST_EVENT,2)
C
        WRITE(IRO,23)
   23   FORMAT(' \STOP')
  999 RETURN
      END
