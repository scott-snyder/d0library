C----------------------------------------------------------------------
C-
C-   Created  15-APR-1993   R. J. Genik II
C-
C- Notes: Current_IDX is Not in this Block, it is in MAC_Global
C- as is Test_option_pct
C----------------------------------------------------------------------
C
C ****  current comparison parameters
C
      INTEGER hist_id_data,hist_nbins_x,hist_nbins_y,nbins_tot
      INTEGER max_bin_x,min_bin_x
      INTEGER max_bin_y,min_bin_y,dhist_nbins,ref_id,ref_id2
      REAL    stat_tol,pct_tol, 
     +  dhist_xmin,dhist_xmax,itmp1,rtmp1,rtmp2,itmp2,itmp3
     +  ,hist_xmin,hist_xmax,hist_ymin,hist_ymax
      LOGICAL two_dim
      Common/Mac_current/ hist_id_data,stat_tol,pct_tol
     +  ,hist_nbins_x,nbins_tot,max_bin_x,min_bin_x,
     +  hist_nbins_y,hist_ymin,hist_ymax,
     +  max_bin_y,min_bin_y,dhist_nbins,
     +  dhist_xmin,dhist_xmax,itmp1,rtmp1,rtmp2,itmp2,itmp3,
     +  hist_xmin,hist_xmax,ref_id,ref_id2,two_dim
C
C ****  current strings
C
      CHARACTER*80 pathname, title,dtitle,dirpath,chpath,
     +  current_comment 
      CHARACTER*3 Test_selection
      CHARACTER*10 Test_opt_stat
      Common/Mac_current_chrs/pathname, Test_selection,Test_opt_stat, 
     +  title,dtitle,dirpath,chpath ,current_comment
C
C ****  paramters used for above
C
      Integer Ipathname_max,Itest_selection_max,Itest_opt_stat_max
      Parameter(  Ipathname_max        =  72 )
      Parameter(  Itest_selection_max  =  3  )
      Parameter(  Itest_opt_stat_max   =  10 )
C
C
      Save /Mac_current/, /Mac_current_chrs/
