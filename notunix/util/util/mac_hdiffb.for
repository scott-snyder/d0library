      SUBROUTINE MAC_HDIFFB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calls hdiffb and dumps the requested info on the
C-   results.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-APR-1993   R. J. Genik II
C-   Updated  26-APR-1993   R. J. Genik II  hacked 'B' specific
C-   initialization from calling routine and added here.
C-   Updated  17-NOV-1993   R. J. Genik II   Two dimensional histograms now
C-   accepted.
C-   Updated   9-DEC-1993   R. J. Genik II  Some formats changed and the
C-   option to use bins instead of bin codes was inserted and using bins is
C-   made the default elsewhere. 
C-   Updated  13-DEC-1993   R. J. Genik II  The histogram title now is
C-   always written out with the id number preceding it. One could bum out
C-   a few variables from the routine, but the loss in speed is quite
C-   small. (i.e. I'll do it later if it becomes a problem.) 
C-   Updated  13-DEC-1993   R. J. Genik II  Added the local logicals
C-   bin_dump_combined, bin_dump_individual to simplify the structure.
C-   These will be optimized out, of course, by the optimizer. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MAC_Global.inc'
      INCLUDE 'D0$INC:MAC_Current.inc'
      INTEGER hist_bin_x,hist_bin_y,nbad,ok_zero,stat_bad,pct_bad
      REAL    sig_arry,pct_arry,fill_arry_e,HI,HIJ,ref_bin
      LOGICAL first_bin, bin_dump_combined, bin_dump_individual
      CHARACTER*100 title_with_id
      INTEGER eye,jay,kay,array_index,Bin_Code, twid_beg, twid_end,
     +  twid_len, trulen
       Dimension sig_arry(nbins_max),
     +  pct_arry(nbins_max),
     +  FILL_ARRY_E(nbins_max)
C----------------------------------------------------------------------
C
C ****  initialization
C    reduce the need for several if-then blocks by finding out if we are
C    using the HDIFFB A-option.
      use_gt = (index(test_opt_stat,'A').ge.1)
C
C ****  get the limits of the title.
C
      CALL Swords(dtitle,eye,jay,kay)
C
C ****  Add id," to title for header output
C
      Write (title_with_id, 1) hist_id_data, dtitle(eye:jay)
    1 Format (I,', "',A,'"')
      Call Swords(title_with_id,twid_beg,twid_end,twid_len)
C
C...  book and unpack, etc.
C    For the dual comparison using HDIFFB we need a histogram reference
C    with the % tol as its error bars.
C
      If (.not.(two_dim)) then ! we will call hdiffb to do the percent
                               ! difference
      Ref_ID2 = REF_ID+tmp_hist_offset
      CALL HCOPY(Ref_id,Ref_id2,'TEMP COMPARE HISTOGRAM')
C
C ****  array_index here will take the top portion of the fill errors array
C ****  and send it to hpake amd hunpake: this was done because of the
C ****  requirement that the array size be exactly equal to NX*NY for
C ****  HBOOK; Note, however, that HBOOK doesn't yet keep error bars on 2-d
C ****  histogram bins. therefore this can be simplified. Also, another
C ****  routine must be written to calculate to %diff on 2-histograms.
C
C
C ****  I have impletmented the above and left the array_index calculated
C ****  out for future changes
C
C      array_index = (nbins_max+2)**2+1-hist_nbins_x*Max(1,hist_nbins_y)
      Array_index = 1
      CALL HUNPAK(REF_ID,FILL_ARRY_E(array_index),' ',0)
c      DO 12 HIST_BIN_y = 1,Max(1,hist_nbins_y)
        DO 10 HIST_BIN_x = 1,hist_nbins_x
          FILL_ARRY_E(array_index) =
     +      PCT_TOL*FILL_ARRY_E(array_index)
          array_index = array_index + 1
   10   CONTINUE
c   12 CONTINUE
C      array_index = (nbins_max+2)**2+1-hist_nbins_x*Max(1,hist_nbins_y)
      Array_index = 1
      CALL HPAKE(REF_ID2,FILL_ARRY_E(array_index))
      Endif ! From if not-2d above
C
C ****  call the comparison routines
C
      CALL hdiffb(REF_ID,hist_id_data,STAT_TOL,nbins_tot,test_opt_stat,
     +    stat_bad,sig_arry)
      If (Two_Dim) then
        Call MAC_2d_Pct_Diff(pct_arry,pct_bad)
      Else
      CALL hdiffb(REF_ID2,hist_id_data,1.,nbins_tot,Test_option_pct,
     +    pct_bad,pct_arry)
C
C ****  now we have to fix the fact that hdiffb returns BIGP for the over
C ****  and underflow bins.
C
      If (Underflow) Then
        pct_bad = MAX(pct_bad-1,0)  ! remove the failing bin
        Ref_Bin = HI(Ref_id,0)      ! Get the reference value
        If (Ref_Bin.LE.0.) then     ! default to -1. if reference bin is
                                    ! less than or equal zero.
          pct_arry(1) = -1.
        Else
          pct_arry(1) = (HI(Hist_id_data,0) - Ref_Bin)/(Ref_Bin*pct_Tol)
        Endif
      Endif
      If (Overflow) Then
        pct_bad = MAX(pct_bad-1,0)  ! remove the failing bin
        Ref_Bin = HI(Ref_id,hist_nbins_x+1)      ! Get the reference value
        If (Ref_Bin.LE.0.) then     ! default to -1. if reference bin is
                                    ! less than or equal zero.
          pct_arry(nbins_tot) = -1.
        Else
          pct_arry(nbins_tot) = (HI(Hist_id_data,hist_nbins_x+1) - 
     +      Ref_Bin)/(Ref_Bin*pct_Tol)
        Endif
      Endif
      Endif ! from not two_dim
C... if nothing, inform user and return
      nbad = 0
      ok_zero = 0
      IF ((pct_bad.EQ.0).AND.(stat_bad.EQ.0)) GOTO 300
      first_bin = .true.! this logical controls the printing of the info
                        ! line just before we dump the bin_codes, etc.
C... convert number of ref percentages to actual percentages and loop
      DO 120 hist_bin_y = min_bin_y,max_bin_y
        DO 100 hist_bin_x = min_bin_x,max_bin_x
C
C ****  this was lifted from hdiffb. it is how we use 1-d arrays for 2-d
C ****  information.
C
          array_index = hist_bin_x - min_bin_x + 1
          Bin_Code = hist_bin_x - 1 +
     +      hist_nbins_x*(hist_bin_y - min_bin_y)
C
C ****  we combine two i 2-d's here. the returned values from
C ****  MAC_2d_Pct_Diff are the actual percentages
C
          if (two_dim)  then
            array_index = array_index +
     +      hist_nbins_x*(hist_bin_y - min_bin_y)
          else
          pct_arry(array_index) =
     +      pct_arry(array_index) *pct_tol
          endif
          bin_dump_combined = (((abs(sig_arry(array_index)).GT.
     +      stat_tol).eqv.(use_gt))
C                   fail stat test
     +      .and.
C                   fail pct test
     +      (abs(pct_arry(array_index)).GT.pct_tol))
          bin_dump_individual = ((output_all_failures).AND.
     +      (((abs(sig_arry(array_index)).GT.
     +      stat_tol).eqv.(use_gt)).OR.(abs(pct_arry(array_index)).GT.
     +      pct_tol)))
          IF (bin_dump_combined.or.bin_dump_individual) THEN
            IF (((abs(sig_arry(array_index)).GT.stat_tol).eqv.
     +      (use_gt)).AND.
     +      (abs(pct_arry(array_index)).GT.pct_tol))
     +       nbad = nbad + 1 ! We had a combined failure
            IF( first_bin) THEN ! Write the header to the output
              first_bin = .false.
              IF (use_gt) THEN ! we have A option
                WRITE(out_unit,29) title_with_id(twid_beg:twid_end),
     +            stat_tol,100.*pct_tol
              ELSE
                WRITE(out_unit,28) title_with_id(twid_beg:twid_end),
     +            100.*stat_tol, 100.*pct_tol
              ENDIF
              If (output_all_failures) Write (out_unit,130)
              WRITE(out_unit,200) !dashes
   28         FORMAT(X,/,x,'Histogram ',A,' failures :',
     +          /,2x,' Bins which have statistical prob <',
     +          F6.2, '% and percent difference > ',F6.1,'%')
   29         FORMAT(X,/,x,'Histogram ',A,' failures :',
     +          /,2x,' Bins which are >',F6.2,
     +          ' sigma away and have percent difference > ',F6.1,'%')
            ENDIF! for first bin
C
C
C ****  Altered here for bin code option
C
            IF (Use_Bin_Codes) then
            IF (use_gt) THEN ! we have the 'A' option
              WRITE (out_unit,30) (bin_code),
     +          bin_code_name(bin_code),sig_arry( array_index), 
     +          100.*pct_arry(array_index)
            ELSE
              WRITE (out_unit,31) (bin_code),
     +          bin_code_name(bin_code),100.*sig_arry(array_index),
     +          100.*pct_arry(array_index)
            ENDIF
            Else ! Don't use bin codes
            If (Two_Dim) Then 
            IF (use_gt) THEN ! we have the 'A' option
              WRITE (out_unit,232) hist_bin_x,hist_bin_y,
     +          bin_code_name(bin_code),sig_arry( array_index), 
     +          100.*pct_arry(array_index)
            ELSE
              WRITE (out_unit,233) hist_bin_x,hist_bin_y,
     +          bin_code_name(bin_code),100.*sig_arry(array_index),
     +          100.*pct_arry(array_index)
            ENDIF
            Else ! We have one Dim
            IF (use_gt) THEN ! we have the 'A' option
              WRITE (out_unit,230) hist_bin_x,
     +          bin_code_name(bin_code),sig_arry( array_index), 
     +          100.*pct_arry(array_index)
            ELSE
              WRITE (out_unit,231) hist_bin_x,
     +          bin_code_name(bin_code),100.*sig_arry(array_index),
     +          100.*pct_arry(array_index)
            ENDIF
            ENDIF ! One or Two Dim
            ENDIF ! Use bin codes or not

            If (Show_Bins) Then
              If (two_dim) then
                Write (out_unit,32)  hist_bin_x,
     +            hist_bin_y,
     +            HIJ(Ref_ID,hist_bin_x,
     +            hist_bin_y), HIJ(Hist_ID_Data,hist_bin_x,
     +            hist_bin_y)
              Else
                Write (out_unit,33) hist_bin_x, HI(Ref_ID,hist_bin_x),
     +             HI(Hist_ID_Data,hist_bin_x) 
              Endif
            Endif
            ENDIF
   30       FORMAT(3X,'Bin code',I5,': ',A32,' : sig =',F7.2,
     +        ' diff = ',F6.1,'%')
   31       FORMAT(3X,'Bin code',I5,': ',A32,' : prob =',F6.2,
     +        '% diff = ',F6.1,'%')
   32       Format(5X,'Bin ',I4,',',I4,' Ref: ',F9.2,' Dat: ',
     +        F9.2)
   33       Format(5X,'Bin ',I4,' Ref: ',F9.2,' Dat: ',F9.2)
          IF ((pct_arry(array_index).LE.-0.99).and.
     +      ((abs(sig_arry(array_index)).LT.stat_tol)! Note this is .LT.
                                                     ! because we are
                                                     ! interested in if the
                                                     ! bin passed, rather
                                                     ! than failed.
     +      .eqv.(use_gt))) THEN
            ok_zero = ok_zero + 1               ! Insig. zero in data
          ENDIF
  100 ENDDO
  120 Enddo
  130 Format (2X,'* Dump individual failures has been selected')
  200 FORMAT(X,80('-'))
  201 FORMAT(X,80('='))
  230       FORMAT(3X,'Bin ',I7,': ',A32,' : sig =',F7.2,
     +        ' diff = ',F6.1,'%')
  231       FORMAT(3X,'Bin ',I7,': ',A32,' : prob =',F6.2,
     +        '% diff = ',F6.1,'%')
  232       FORMAT(3X,'Bin ',I3,', ',I3,': ',A32,' : sig =',F7.2,
     +        ' diff = ',F6.1,'%')
  233       FORMAT(3X,'Bin ',I3,', ',I3,': ',A32,' : prob =',F6.2,
     +        '% diff = ',F6.1,'%')

C
C... output results: summary of this id number
C
  250 Format(X,A80)
  300 IF (((Summarize_all).and.((pct_bad+stat_bad).gt.0))
     +  .OR.(nbad.NE.0)) THEN
        WRITE (out_unit,200) ! dashes
        WRITE (out_unit,335) title_with_id(twid_beg:twid_end)
     +    ,stat_bad,pct_bad,ok_zero,
     +    nbad
       IF (TRULEN(Current_Comment).gt.0) 
     + Write (Out_unit,250) Current_Comment
        WRITE (out_unit,201) ! equals signs
  335   FORMAT(X,'Summary: Histogram ',A,/,7X,
     +    I3,' Sig fails, ',I3,' Pct fails,',I3,
     +    ' Ok zeroes,',I3,' failures (both sig and pct)')
      ELSEIF (Confirm_OK) then
        WRITE (out_unit,fmt=350) title_with_id(twid_beg:twid_end)
  350   FORMAT(X,'Histogram ',A,' OK ')
      ENDIF
      no_comps_done = no_comps_done + 1
      IF (nbad.GT.0) nbad_runs = nbad_runs +1
  999 RETURN
      END
