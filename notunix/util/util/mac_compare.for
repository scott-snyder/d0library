      SUBROUTINE MAC_Compare
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Comparison routine for the Mega_Autocompare
C-   package. Reads in the requested histograms, does the required
C-   comparisons, and writes to the output file the results of the
C-   comparison.
C-
C-   Inputs  : The MAC_Global and MAC_Current common blocks control the
C-   routines used and the comparisons done.
C-
C-   Outputs :  Various errors and info on comparisons done
C-
C-
C-   Updated  18-APR-1993   R. J. Genik II  Hacked out from
C-   Autocompare_Histogram to be compatable with new I/O specs from
C-   Mega_Autocompare_Histogram.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:MAC_Global.inc'
      INCLUDE 'd0$inc:MAC_Current.inc'
      LOGICAL first,hexist,logic_tmp
      REAL    hsum,sum_cont
      INTEGER eye,jay,kay,eye2,jay2,kay2,ierr
      SAVE first
      DATA first / .true. /

C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL MAC_Open_Reference
        CALL MAC_Open_Data
C
C... get a free unit number for use in several opens/closes
C
        CALL gtunit(Igtunit_userid,Tmp_Unit,IERR)
        IF (ierr.NE.0) CALL Errmsg('GTUNIT failed',
     +    'MAC_Compare',
     +    'CANNOT get unit number', 'F')
      ENDIF
C
C... now start the hbook stuff
C
C
      dirpath ='//'//topdir_ref//pathname
      CALL HCDIR (dirpath,' ')
      CALL hcdir (chpath,'R')
      CALL swords(chpath,eye,jay,kay)
      CALL swords(dirpath,eye2,jay2,kay2)
      IF (chpath(eye:jay).NE.dirpath(eye2:jay2)) THEN
        WRITE (out_unit,fmt=22) reference_file,pathname
        GOTO 999
      ENDIF
      CALL HRIN(REF_ID,0,Reference_id_offset)
      logic_tmp = HEXIST (REF_ID+Reference_id_offset)
      IF (.NOT.logic_tmp) THEN ! histogram was not in directory
      CALL swords(pathname,eye,jay,kay)
      CALL swords(reference_file,eye2,jay2,kay2)
        WRITE (out_unit,fmt=23)
     +    reference_file(eye2:jay2),pathname(eye:jay),Ref_ID
        GOTO 999
      ENDIF
      Ref_ID = Ref_ID + Reference_id_offset! set correct id #
C
C... now we have the reference histogram in memeory.
C
      CALL HGIVE(ref_id,title,hist_nbins_x,
     +  hist_xmin,hist_xmax,hist_nbins_y,hist_ymin,hist_ymax
     +  ,itmp2,itmp3)
C
C ****  now we need to set the total number of bins in the comparison
C
      Call MAC_Set_Nbins_tot ! also sets the min_bin and max_bins
C
C ****  now, get the data histogram
C
      dirpath ='//'//topdir_dat//pathname
      CALL HCDIR (dirpath,' ')
      CALL hcdir (chpath,'R')
      CALL swords(chpath,eye,jay,kay)
      CALL swords(dirpath,eye2,jay2,kay2)
      IF (chpath(eye:jay).NE.dirpath(eye2:jay2)) THEN! directory doesn't
                                                     ! exist.
      CALL swords(pathname,eye,jay,kay)
      CALL swords(data_file,eye2,jay2,kay2)
        WRITE (out_unit,fmt=22) data_file(eye2:jay2),
     +    pathname(eye:jay)
      ENDIF
      CALL HRIN(HIST_ID_data,0,0)
      logic_tmp = HEXIST (hist_ID_data)
      IF (.NOT.logic_tmp) THEN ! histogram wasn't in directory
      CALL swords(pathname,eye,jay,kay)
      CALL swords(data_file,eye2,jay2,kay2)
        WRITE (out_unit,fmt=23)
     +    data_file(eye2:jay2),pathname(eye:jay),hist_id_data
        GOTO 999
      ENDIF
   22 FORMAT(2X,'** HST File ',A,/,5x,'dir tree does not contain ',A)
   23 FORMAT(2X,'** HST File ',A,/,5x,'dir. ',A,
     +  ' does not contain ID =',I6)
      sum_cont = hsum(hist_id_data)
C
C ****  really all we do with this now is get the title.
C
      CALL HGIVE(hist_id_data,dtitle,dhist_nbins,
     +  dhist_xmin,dhist_xmax,itmp1,rtmp1,rtmp2,itmp2,itmp3)
      IF(sum_cont.EQ.0.) THEN
      CALL swords(pathname,eye,jay,kay)
        WRITE(out_unit,25) data_file,pathname(eye:jay),hist_id_data
   25   FORMAT(2x,'**Data file ',A,/,3x,' Dir ',A,' ID = ',I,
     +    ' contains zero entries. Skipping this histogram.')
        GOTO 999
      ENDIF
C
C
C... now do the comparisons
C
C
      IF (index(test_selection,'B').ge.1) then
        IF (nbins_tot.gt.nbins_max) 
     +    CALL Errmsg('Number of bins greater than maximum',
     +    'MAC_Compare',
     +    'Contact Czar for Larger Limits', 'W') ! Issue a warning
        CALL MAC_hdiffb
      ELSE
        CALL MAC_Hdiff
      ENDIF
  999 If (reset_names_of_bins) Call MAC_Bin_Code_Defaults 
C
C ****  delete all histograms in memeory if flagged to do so
C
      If (del_after_compare_all) Call HDELET(0)
C
C ****  If there are a large number of skipped histograms and this logical
C ****  is set false for an upgrade, such as an optional command which
C ****  could store stuff in huge ntupes, one can get an error in HRIN when
C ****  it reads in a new histogram; the default is to replace it. Care
C ****  should be taken in the addition of such an option to make sure that
C ****  HRIN doesn't replace histograms before we write them to a file.
C
      RETURN
      END
