      SUBROUTINE MAC_READ_6_HIST_PARAMS(Parameter_Array_Name,
     +  Array_index ,Ierror)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads the NEXT six parameters from the
C-                         Parameter_Array_Name from the current RCP bank
C-                         starting with Array_index. This is the required
C-                         parameters from the 'instructions'. These 6
C-                         parameters are placed in the /Mac_current/
C-                         common block.
C-
C-   Inputs  : Parameter_Array_Name(C*),Array_index(I)
C-   Outputs : Ierror = 0 if all ok, else = -x where x is the
C-              sequence in the calls to ezget which failed first see
C-              logic below.
C-             Array_index = Array_index+6
C-   Controls:
C-
C-   Created  15-APR-1993   R. J. Genik II
C-   Updated  22-APR-1993   R. J. Genik II  only reads in 5 parameters if
C-   Test_selection is not 'B' 
C-   Updated  23-MAY-1993   R. J. Genik II  for beta release, array index
C-   now incremented via mac_get_Next_command and ezer_ezgets has been
C-   modified. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) Parameter_Array_Name
      INTEGER Ierror,Array_index,Ierr,eye,jay,kay
      CHARACTER*80 Tmp_string
      REAL    Value
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:MAC_CURRENT.INC'
C----------------------------------------------------------------------
C set current comment to blanks
      Write (current_comment,FMT=1)
    1 Format(80X)
      Ierror = 0
C
C ****  get pathname to histogram
C
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -1
      ELSE
        CALL Swords(Tmp_string,eye,jay,kay)
        IF (kay.GT.Ipathname_max) THEN ! it's too long
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +      ,'Pathname is too long, truncating','W')
          IF (ierror.EQ.0) ierror = -9
          pathname = Tmp_string(eye:ipathname_max+eye+1)
        ELSE
          pathname = Tmp_string(eye:jay)
        ENDIF
      ENDIF
C
C ****  get histogram id number within tree
C
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -2
      ELSE
        hist_id_data = INT(Value(Tmp_string,eye,jay,kay))
        IF (kay.NE.1) THEN ! it wasn't an integer
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +      ,'I was expecting an integer for the Hist ID #','W')
          IF (ierror.EQ.0) ierror = -9
        ENDIF
      ENDIF
C
C ****  get test selection
C
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -3
      ELSE
        CALL Swords(Tmp_string,eye,jay,kay)
        IF (kay.GT.Itest_selection_max) THEN ! it's too long
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +      ,'Test_selection is too long, truncating','W')
          IF (ierror.EQ.0) ierror = -9
          Test_selection = Tmp_string(eye:Itest_selection_max+eye+1)
        ELSE
          Test_selection = Tmp_string(eye:jay)
        ENDIF
      ENDIF
C
C ****  get test options
C
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -4
      ELSE
        CALL Swords(Tmp_string,eye,jay,kay)
        IF (kay.GT.Itest_opt_stat_max) THEN ! it's too long
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +      ,'Test_option is too long, truncating','W')
          IF (ierror.EQ.0) ierror = -9
          Test_opt_stat = Tmp_string(eye:eye+Itest_opt_stat_max+1)
        ELSE
          Test_opt_stat = Tmp_string(eye:jay)
        ENDIF
      ENDIF
C
C ****  get statistical tolerance
C
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -5
      ELSE
        Stat_tol = Value(Tmp_string,eye,jay,kay)
        IF ((kay.NE.2).AND.(kay.NE.3)) THEN ! it wasn't real
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +      ,'I was expecting a real number for stat_tol','W')
          IF (ierror.EQ.0) ierror = -9
        ENDIF
      ENDIF
C
C ****  get percent toelerance (if Test_Selection = 'B')
C
      IF (Index(Test_selection,'B').ge.1) then
      CALL MAC_Ezer_Ezgets( Parameter_Array_Name, Array_index,
     +    Tmp_string,'MAC_READ_6_HIST_PARAMS', IERR)
      IF (Ierr.NE.0) THEN
        Ierror = -6
      ELSE
        pct_tol = Value(Tmp_string,eye,jay,kay)
        IF ((kay.NE.2).and.(kay.NE.3)) THEN ! it wasn't real
          CALL Errmsg('EZGETS FAILED','MAC_READ_6_HIST_PARAMS'
     +        ,'I was expecting a real number for pct_tol','W')
          IF (ierror.EQ.0) ierror = -9
        ENDIF
      ENDIF
      Endif
C
C ****  set default ref_id to hist_id
C
      Ref_id = hist_id_data
  999 RETURN
      END
