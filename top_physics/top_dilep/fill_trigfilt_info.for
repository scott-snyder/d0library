      SUBROUTINE FILL_TRIGFILT_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill trigger filter information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:top_DILEP_ANALYSIS.INC'
      REAL    XDATA(1000)
      INTEGER LGLOB, GZGLOB, I,NTAGS,MRVFLG(9)
      LOGICAL FILTPASS(50)
C----------------------------------------------------------------------
C
C ****  filter and active veto information
C
      LGLOB = GZGLOB()
      XFILT(1) = Q(LGLOB+17)
      CALL MRVETO_INFO(mrvflg)
      DO i=1,9
        ixfilt(i+1) = mrvflg(i)
      ENDDO
      CALL filt(filtpass)
      DO i=1,29
        IF (filtpass(i)) THEN
          ixfilt(10+i) = 1
        ENDIF
      ENDDO
  999 RETURN
C...........................................................................
      ENTRY EVT_FILTINFO(NVAR,XDATA)
      nvar = 39
      CALL ucopy(xfilt,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY EVT_FILTINFO_TAGS(NTAGS,TFILT)
      ntags = 39
      TFILT(1)='TIME29'
      TFILT(2)='MRBS_LOSS:I'
      TFILT(3)='MICRO_BLANK:I'
      TFILT(4)='CAL_RECOVERY:I'
      TFILT(5)='MU_HV_RECOVERY:I'
      TFILT(6)='MR_VETO_HIGH:I'
      TFILT(7)='MR_VETO_LOW:I'
      TFILT(8)='MAX_LIVE:I'
      TFILT(9)='GOOD_CAL:I'
      TFILT(10)='GOOD_BEAM:I'
      TFILT(11)='ELE_JET_HIGH:I'
      TFILT(12)='EM2_ELE_EMF_MS:I'
      TFILT(13)='EM2_EIS_EIS:I'
      TFILT(14)='EM2_EIS2_HI:I'
      TFILT(15)='EM2_GIS_GAM:I'
      TFILT(16)='EM2_EIS_ELE:I'
      TFILT(17)='ELE_JET:I'
      TFILT(18)='ELE_JET_MAX:I'
      TFILT(19)='ELE_2_HIGH:I'
      TFILT(20)='ELE_HIGH:I'
      TFILT(21)='ELE_MAX:I'
      TFILT(22)='GIS_DIJET:I'
      TFILT(23)='MU_ELE:I'
      TFILT(24)='MU_ELE_BEST:I'
      TFILT(25)='MU_JET_HIGH:I'
      TFILT(26)='MU_JET_CAL:I'
      TFILT(27)='MU_JET_MAX:I'
      TFILT(28)='MU_JET_CENT:I'
      TFILT(29)='MU_JET_CENCAL:I'
      TFILT(30)='MU_JET_MAX_CENCAL:I'
      TFILT(31)='JET_MS_MULTI:I'
      TFILT(32)='XFILT1E:I'
      TFILT(33)='XFILT2E:I'
      TFILT(34)='XFILT1A_EE:I'
      TFILT(35)='XFILT1B_EE:I'
      TFILT(36)='XFILT1A_EMU:I'
      TFILT(37)='XFILT1B_EMU:I'
      TFILT(38)='MU_1_MAX:I'
      TFILT(39)='MU_1_CENT_MAX:I'
      RETURN
      END
