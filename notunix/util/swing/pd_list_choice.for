      subroutine pd_list_choice( board_id, keyboard, width, num_choice,
     .                           pd_choices, choice, pullmode,
     .				 code, do_bar)

      implicit none
      include '($smgdef)'
      include '($ssdef)'
      include 'd0$inc:pulldown.inc'

      record /pd_choice_type/ pd_choices

      integer smg$create_virtual_display
      integer max_cell, ii, jj, kk, lens(PD_MAX_CHOICES), code, istat
      integer start_pos, pd_list_id, atts(PD_MAX_CHOICES), num_choice
      integer pos, new_pos, key, width, keyboard, board_id
      logical exit, do_bar, direct, pullmode
      character choice*(PD_MAX_CHOICE_LEN)

      do_bar = .false.

C     FIND OUT HOW MANY CHOICES THERE ARE AND THE MAXIMUM LENGTH
      ii = 1
      max_cell = 0
      do while ( ii .le. pd_choices.number )
         call str$trim( pd_choices.choice(ii), pd_choices.choice(ii),
     .                  lens(ii) )
         max_cell = max( max_cell, lens(ii) )
         ii = ii + 1
      end do
      ii = ii - 1

C     CREATE THE VIRTUAL DISPLAY FOR THE LIST
      istat = smg$create_virtual_display( ii, max_cell, pd_list_id,
     .                                    smg$m_border, smg$m_reverse )

C     PUT THE CHOICES IN THE LIST
      do jj = 1, ii
         if ( pd_choices.ptr(jj) .eq. 0 ) then
            call smg$put_chars( pd_list_id,
     .                          pd_choices.choice(jj)(1:max_cell),
     .                          jj, 1 )
            atts(jj) = 0
            else
            call smg$put_chars( pd_list_id,
     .                          pd_choices.choice(jj)(1:max_cell),
     .                          jj, 1,, smg$m_underline )
            atts(jj) = smg$m_underline
            end if
      end do

      start_pos = 1 + (pd_cell_size*(num_choice-1))
      if ( start_pos + max_cell .gt. width ) then
         start_pos = width - max_cell + 1
         end if

      call smg$begin_pasteboard_update( board_id )
      call smg$paste_virtual_display( pd_list_id, board_id, 2,
     .                                start_pos )
      call smg$repaste_virtual_display( pd_bar_id, board_id, 1, 1 )
      call smg$end_pasteboard_update( board_id )

C     GET A CHOICE FROM THE LIST
      exit = .false.
      key = 0
      direct = .false.
      pos = 1
      new_pos = 1

C     SET THE RENDITION OF THE FIRST CHOICE
      call smg$change_rendition( pd_list_id, 1, 1, 1,
     .                           max_cell, smg$m_bold + atts(1) )

      do while ( key .ne. smg$k_trm_enter .and.
     .           key .ne. smg$k_trm_cr .and.
     .           .not. exit .and. .not. direct )

         call smg$set_cursor_abs( pd_list_id, pos, 1 )

         call smg$read_keystroke( keyboard, key )

         if ( key .eq. smg$k_trm_up ) then
            if ( pos .gt. 1 ) then
               new_pos = pos - 1
               else
               do_bar = .true.
               exit = .true.
               end if
            else if ( key .eq. smg$k_trm_down ) then
            if ( pos .lt. ii ) then
	      new_pos = pos + 1
	      else
	      new_pos = 1
	      end if
            else if ( key .eq. smg$k_trm_left .and.
     .		      pullmode ) then
            if ( num_choice .gt. 1 ) num_choice = num_choice - 1
            do_bar = .true.
            exit = .true.
            else if ( key .eq. smg$k_trm_right .and.
     .		      pullmode ) then
            if ( num_choice .lt. pd_num_choices )
     .         num_choice = num_choice + 1
            do_bar = .true.
            exit = .true.
            else if ( key .eq. smg$k_trm_ctrlz ) then
            exit = .true.
	    else
	      ii = 1
	      do while ( ii .le. pd_choices.number )
		if ( key .eq. pd_choices.key(ii) .or.
     .	  	     key .eq. pd_choices.key(ii) + 32 ) then
		  direct = .true.
		  new_pos = ii
		end if
		ii = ii + 1
	      end do
            end if

         if ( new_pos .ne. pos ) then
            call smg$change_rendition( pd_list_id, pos, 1, 1,
     .                                 max_cell, atts(pos))
            call smg$change_rendition( pd_list_id, new_pos, 1, 1,
     .                                 max_cell,
     .                                 smg$m_bold+atts(new_pos) )
            end if

         pos = new_pos

      end do

      call smg$unpaste_virtual_display( pd_list_id, board_id )

      if ( exit ) then
         choice = ' '
         code = -1
         else
         choice = pd_choices.choice(pos)
         code = pd_choices.code(pos)
         end if

      return
      end
