      subroutine pd_bar_choice( keyboard, num_choice, pd_choices )

      implicit none
      include '($smgdef)'
      include 'd0$inc:pulldown.inc'

      integer pos, new_pos, key, num_choice, keyboard, ii
      logical exit, down
      record /pd_choice_type/ pd_choices

      exit = .false.
      down = .false.
      key = 0
      new_pos = num_choice
      pos = num_choice

C     SET THE RENDITION OF THE FIRST CHOICE
      ii = 1 + (pd_cell_size*(new_pos-1))
      call smg$change_rendition( pd_bar_id, 1, ii, 1,
     .                           pd_cell_size, smg$m_bold )

      do while ( key .ne. smg$k_trm_enter .and.
     .           key .ne. smg$k_trm_cr .and.
     .           .not. down .and. .not. exit )

         call smg$set_cursor_abs( pd_bar_id, 1, 1 )

         call smg$read_keystroke( keyboard, key )

         if ( key .eq. smg$k_trm_left ) then
            if ( pos .gt. 1 ) then
	      new_pos = pos - 1
	      else
	      new_pos = pd_num_choices
	      end if
            else if ( key .eq. smg$k_trm_right ) then
            if ( pos .lt. pd_num_choices ) then
	      new_pos = pos + 1
	      else
	      new_pos = 1
	      end if
            else if ( key .eq. smg$k_trm_down ) then
            if ( pd_choices.ptr(pos) .ne. 0 ) down = .true.
            else if ( key .eq. smg$k_trm_ctrlz ) then
            exit = .true.
            end if

         if ( new_pos .ne. pos ) then
            ii = 1 + (pd_cell_size*(pos-1))
            call smg$change_rendition( pd_bar_id, 1, ii, 1,
     .                                 pd_cell_size, 0, 0 )
            ii = 1 + (pd_cell_size*(new_pos-1))
            call smg$change_rendition( pd_bar_id, 1, ii, 1,
     .                                 pd_cell_size, smg$m_bold )
            end if

         pos = new_pos

      end do

      ii = 1 + (pd_cell_size*(pos-1))
      call smg$change_rendition( pd_bar_id, 1, ii, 1,
     .                           pd_cell_size, 0, 0 )

      if ( exit ) then
         num_choice = 0
         else
         num_choice = pos
         end if

      return
      end
