      SUBROUTINE MAC_2D_PCT_DIFF(percent_arry,failed)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does a bin by bin percent difference of 2-d
C-   histograms and returns the results in pct_diffs. This could be
C-   accomplished via HOPERA, however, this would not allow for
C-   over/underflow bins to be included easily. this routine will no longer
C-   be needed when HBOOK accepts error bars on 2-histograms.
C-
C
C ****  This routine is written right now for only the hdiffb C-option
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-OCT-1993   R. J. Genik II
C-   Updated   1-NOV-1993   R. J. Genik II  Note: for errors here, we call
C-   HBUG. this is because we want these errors to appear the same place as
C-   errors from HDIFFB, which we are emulating. 
C-   Updated   2-NOV-1993   R. J. Genik II  Corrected over/under flow
C-   calculation of lambda, added possibility of R/L/T/B options. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  HIJ,scale_factor,ex_value,entries_idr,entries_idd
      INTEGER  two_d_index,IX_Bin,IY_Bin,failed
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Current.inc'
      REAL percent_arry(nbins_max)
      LOGICAL Z_Option
C----------------------------------------------------------------------
C
C ****  get the proper scale factor unless option N is selected for percent
C ****  difference test
C
      Z_option = (Index(Test_option_pct,'Z').ge.1)
      entries_idr = 0.
      entries_idd = 0.
      Do 5 IY_Bin = min_bin_y,max_bin_y
        Do 4 IX_Bin = min_bin_x,max_bin_x
          entries_idr = entries_idr + HIJ(Ref_id,IX_Bin,IY_Bin)
          entries_idd = entries_idd + HIJ(hist_id_data,IX_Bin,IY_Bin)
    4   Continue
    5 Continue
      IF (Entries_Idr.LE.0.) THEN
        CALL HBUG('Sum of Reference Histogram Zero or negative',
     +    'MAC_2D_PCT_DIFF', (Ref_id - reference_id_offset))
        IF (Z_option) RETURN
        DO 10 IX_Bin = 1,nbins_max
          percent_arry(IX_Bin) = -9.99
   10   CONTINUE
        RETURN
      ENDIF
      IF (Entries_Idd.LE.0.) THEN
        CALL HBUG('Sum of Data Histogram Zero or negative',
     +    'MAC_2D_PCT_DIFF', hist_id_data)
        IF (Z_Option) RETURN
        DO 12 IX_Bin = 1,nbins_max
          percent_arry(IX_Bin) = -9.99
   12   CONTINUE
        RETURN
      ENDIF
      IF (.NOT.(Index(Test_option_pct,'N').ge.1)) then
        scale_factor = entries_idd/entries_idr
      ELSE
        scale_factor = 1
      ENDIF
      Failed = 0
      DO 110 IY_Bin = min_bin_y,max_bin_y
        DO 100 IX_Bin = min_bin_x,max_bin_x
          two_d_index = IX_Bin - min_bin_x + 1 + 
     +        hist_nbins_x*(IY_Bin - min_bin_y)
          ex_value = scale_factor * HIJ(Ref_id,IX_Bin,IY_Bin)
          IF (ex_value.LE.0.) THEN
            IF ((Z_option).AND.(ex_value.EQ.0.)) THEN
              percent_arry(two_d_index) = 0.0
              GOTO 100
            ELSEIF (HIJ(Hist_id_data,IX_Bin,IY_Bin).eq.ex_value) Then 
              percent_arry(two_d_index) = 0.0
              GOTO 100
            ELSEIF(ex_value.LT.0.) THEN
              percent_arry(two_d_index) = -9.99
              GOTO 100
            ENDIF
          ENDIF
          percent_arry(two_d_index) = (HIJ(Hist_id_data,IX_Bin,IY_Bin)
     +        - ex_value)/ex_value
          IF (abs(percent_arry(two_d_index)).GE.pct_tol)
     +        Failed = Failed + 1
  100   CONTINUE
  110 CONTINUE
  999 RETURN
      END
