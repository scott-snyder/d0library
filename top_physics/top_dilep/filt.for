      SUBROUTINE FILT(pass)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-MAR-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL      OK,L2NAME_PASSED,PASS(50)
      CHARACTER*32 FILT_NAME,SPACE
C----------------------------------------------------------------------
      OK = .FALSE.
      FILT_NAME = SPACE(1:32)
      filt_name ='ELE_JET_HIGH'
      PASS(1)= L2NAME_PASSED(FILT_NAME)
      filt_name = 'EM2_ELE_EMF_MS'
      PASS(2)= L2NAME_PASSED(FILT_NAME)
      filt_name = 'EM2_EIS_EIS'
      PASS(3)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'EM2_EIS2_HI'
      PASS(4)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'EM2_GIS_GAM'
      PASS(5)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'EM2_EIS_ELE'
      PASS(6)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'ELE_JET'
      PASS(7)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'ELE_JET_MAX'
      PASS(8)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'ELE_2_HIGH'
      PASS(9)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'ELE_HIGH'
      PASS(10)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'ELE_MAX'
      PASS(11)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'GIS_DIJET'
      PASS(12)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_ELE'
      PASS(13)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_ELE_BEST'
      PASS(14)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_HIGH'
      PASS(15)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_CAL'
      PASS(16)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_MAX'
      PASS(17)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_CENT'
      PASS(18)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_CENCAL'
      PASS(19)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_JET_MAX_CENCAL'
      PASS(20)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'JET_MS_MULTI'
      PASS(21)= L2NAME_PASSED(FILT_NAME)

      PASS(22) = PASS(1).OR.PASS(2).OR.PASS(3).OR.
     &           PASS(4).OR.PASS(5).OR.PASS(6)
      PASS(23) = PASS(1).OR.PASS(7).OR.PASS(8).OR.PASS(9).OR.PASS(10)
      PASS(24) = PASS(1).OR.PASS(2).OR.PASS(3).OR.
     &           PASS(4).OR.PASS(5).OR.PASS(6)
      PASS(25) = PASS(1).OR.PASS(6).OR.PASS(7).OR.
     &           PASS(8).OR.PASS(9).OR.PASS(10)
C 1A emu
      PASS(26)= PASS(1).OR.PASS(8).OR.PASS(13).OR.PASS(15)
C 1B emu
      IF (IQ(LHEAD+6).LT.87804) THEN
        PASS(27)= PASS(1).OR.PASS(13).OR.PASS(15)
      ELSE
        PASS(27)= PASS(1).OR.PASS(13).OR.PASS(18)
      ENDIF
      filt_name =  'MU_1_MAX'
      PASS(28)= L2NAME_PASSED(FILT_NAME)
      filt_name =  'MU_1_CENT_MAX'
      PASS(29)= L2NAME_PASSED(FILT_NAME)

C
C ****  
C
  999 RETURN
      END
