      SUBROUTINE MAC_SET_NBINS_TOT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets total number of bins in comparison
C-   according to over/underflow options and current histogram.
C-
C-   Inputs  : None, just the common block options
C-   Outputs : Sets nbins_tot accoring to options and sets over/underflow
C-   logical flags and removes/adds inconsitent options.
C-   Controls: The over/underflow options are set according to
C-   Test_opt_stat, from the instruction file.
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated  26-OCT-1993   R. J. Genik II  deals with the inclusion of 2-d
C-   histograms. Also sets two_dim flag.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Current.inc'
      Include 'D0$INC:MAC_Global.inc'
      LOGICAL O_option,U_option,R_option,L_option,T_option,B_option
      CHARACTER*10 Test_option_pct_default
      INTEGER IX,IY
      LOGICAL first
      SAVE first
      DATA first / .true. /
      IF( first ) THEN ! get the default Test_option_pct
        first = .false.
        Test_option_pct_default = Test_option_pct
      ENDIF
C
C----------------------------------------------------------------------
C... set two_dim flag
      two_dim = (hist_nbins_y.gt.0)
      O_option = (Index(Test_opt_stat,'O').ge.1)
      U_option = (Index(Test_opt_stat,'U').ge.1)
      R_option = (Index(Test_opt_stat,'R').ge.1)
      L_option = (Index(Test_opt_stat,'L').ge.1)
      T_option = (Index(Test_opt_stat,'T').ge.1)
C... Go back to the default Test_option_pct
      Test_option_pct = Test_option_pct_default
      overflow = .false.
      underflow = .false.
C... Use RLTB instead to make logic simpler
      If (O_option) R_option = .true.
      If ((O_option).and.(two_dim)) T_option = .true.
      If (U_option) L_option = .true.
      If ((U_option).and.(two_dim)) B_option = .true.
C... set min_bin and max_bin according to requested over/underflows
      if (L_option) then
        underflow = .true.
        min_bin_x = 0
        Call ADD_CHR(Test_option_pct,'L')
      else
        min_bin_x = 1
        Call SUB_CHR(Test_option_pct,'L')
      endif
      if (B_option) then
        underflow = .true.
        min_bin_y = 0
        Call ADD_CHR(Test_option_pct,'B')
      else
        min_bin_y = MIN(1,hist_nbins_y)
        Call SUB_CHR(Test_option_pct,'B')
      endif
      if (R_option) then
        overflow = .true.
        max_bin_x = hist_nbins_x + 1
        Call ADD_CHR(Test_option_pct,'R')
      else
        max_bin_x = hist_nbins_x
        Call SUB_CHR(Test_option_pct,'R')
      endif
      if (T_option) then
        overflow = .true.
        max_bin_y = hist_nbins_y + MIN(1,hist_nbins_y)
        Call ADD_CHR(Test_option_pct,'T')
      else
        max_bin_y = hist_nbins_y
        Call SUB_CHR(Test_option_pct,'T')
      endif
C... Count the number of bins
      nbins_tot = 0
      Do 20 IY = min_bin_y, max_bin_y
        Do 10 IX = min_bin_x, max_bin_x
          nbins_tot = nbins_tot + 1
   10   Continue
   20 Continue
  999 RETURN
      END
