      SUBROUTINE MAC_HDIFF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call HDIFF for 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-APR-1993   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Current.inc'
      REAL    Kolmo_prob
      CHARACTER*100 title_with_id
      INTEGER eye,jay,kay, twid_beg, twid_end,
     +  twid_len
C----------------------------------------------------------------------
      CALL Hdiff(REF_ID,hist_id_data,Kolmo_prob,test_opt_stat)
      Call Swords(dtitle,eye,jay,kay)
C
C ****  Add id," to title for header output
C
      Write (title_with_id, 1) hist_id_data, dtitle(eye:jay)
    1 Format (I,', "',A,'"')
      Call Swords(title_with_id,twid_beg,twid_end,twid_len)
      IF (Kolmo_prob.LT.stat_tol) THEN
        nbad_runs = nbad_runs +1
        WRITE (out_unit,27) title_with_id(twid_beg:twid_end)
     +    ,100.*Kolmo_prob, 100.*stat_tol
   27   FORMAT(/,X,/,2x,'** Histogram ',A,
     +    /,2x,'** Fails Kolmogorov test (Hdiff), ',
     +     'Prob = ',F6.1,'%,',' Stat Cut = ',F6.1,'%')
        Write (out_unit,200)
        GOTO 800
      ENDIF
  200 FORMAT(X,80('-'))
  201 FORMAT(X,80('='))
  300 IF ((Summarize_all).or.(Kolmo_prob.LT.stat_tol)) THEN
        WRITE (out_unit,200)
        WRITE (out_unit, FMT=310) dtitle(eye:kay),100.*Kolmo_prob
        WRITE (out_unit,201)
  310 Format (X,' Histogram "',A,'"',/,3x,' Kolmogorov test (Hdiff), ',
     +     ' Prob = ',F6.1,'%')
      ELSEIF (Confirm_OK) then
        WRITE (out_unit,fmt=350) dtitle(eye:jay)
  350   FORMAT(X,'Histogram "',A,'" OK ')
      ENDIF
  800 no_comps_done = no_comps_done + 1
  999 RETURN
      END
