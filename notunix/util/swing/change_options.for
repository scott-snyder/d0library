      subroutine change_options( code )

      include    'd0$inc:swing.inc'

      character  choice*(pd_max_choice_len)
      integer code, ii
      logical do_bar
      logical temp

      if ( code .eq. 0 ) then
	 ii = 7
         call pd_list_choice( board_id, keyboard, width, ii,
     .                        %val(pull_choices.ptr(7)),
     .                        choice, .false., code, do_bar )
      end if

      if ( code .eq. 71 ) then
         call execute_dcl

      else if ( code .eq. 72 ) then
         use_window1 = .not. use_window1
         if ( .not. use_window1 ) then
            call smg$erase_display( window1 )
            else
            call update_window1
            end if

      else if ( code .eq. 73 ) then
         temp = use_window1
         call show_files
         use_window1 = temp
         end if

      return
      end
