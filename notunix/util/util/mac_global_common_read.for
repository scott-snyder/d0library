      SUBROUTINE MAC_GLOBAL_COMMON_READ(Bank_Name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the MAC_Global common block fron the
C-   passed RCP file, then deletes it via EZDROP.
C-
C-   Inputs  : None
C-   Outputs : Fills common block
C-   Controls:
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated   2-NOV-1993   R. J. Genik II  Added Show_Bins 
C-   Updated   3-DEC-1993   R. J. Genik II  Also enetered the call which
C-   opens the output at the end of this file.
C-   Updated   3-JAN-1994   R. J. Genik II  I'm trying to eliminate the
C-   expert RCP by hard coding intial values. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      CHARACTER*(*) Bank_Name
      INTEGER Name_Len,IERR
      use_bin_codes = .false.  ! Init to false
      reset_names_of_bins = .true.  ! Init to true
      Call MAC_Bin_Code_Defaults
C----------------------------------------------------------------------
      Call INZBRA
      Call INZCOM(2)
      Call INPAWC
C----------------------------------------------------------------------
      DATA_FILE = 'MAC$DATA'
      TEST_OPTION_PCT = 'AZ' ! default to ignore zeros
      DESCRIPTION_FILE = 'MAC$DESCRIPTION'
      REFERENCE_FILE = 'MAC$REFERENCE' 
      BIN_CODE_FILE = 'MAC$BIN_CODE_FILE'
      OUTFILE_LOC = 'SYS$OUTPUT'
C....
      REFERENCE_ID_OFFSET = 10000 ! Since the data and reference id are the
                                ! same, we use hist_id = id, ref_id = id+
                                ! this offset. Below, we set the offset for
                                ! the percent difference histogram.
      tmp_hist_offset =  20000
      del_after_compare_all = .true. ! deletes all histograms in memory at the
                                 ! end
                                 ! of each comparison. Planned for future,
                                 ! right now set to .true.
      
      SUMMARIZE_ALL = .FALSE.
      IGTUNIT_USERID  = 666         ! gtunit user id
      UNDERFLOW = .false.
      OVERFLOW = .false.
      Show_Bins = .true.
      Confirm_OK = .true.
      Output_All_Failures = .false.
C
C ****  The opening of the output file has been relocated to here instead
C ****  of in MAC compare because we now write to the output file before we
C ****  do any comparisons some times.
        CALL MAC_Open_Outfile_Loc
  999 RETURN
      END
