      subroutine show_files

c     Craig Young               3-AUG-87

c     This subroutine controls the initialization of the filer window as well
c     as movement and command_level input within the filer.

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    ikey, isave, code, code_type, jj
      logical    finished
      character  key, choice*(PD_MAX_CHOICE_LEN)

      call smg$begin_pasteboard_update( board_id )

      call smg$erase_display( file_window )
      call smg$label_border( file_window, node(node_num).name )
      call smg$paste_virtual_display( file_window, board_id, 10, 40 )
      call load_files
      call update_file_window
      call redefine_smg_layout

      call smg$end_pasteboard_update( board_id )

      finished = .false.

      do while ( .not. finished )

         call smg$set_cursor_abs( file_window,
     .                            file_num - top_file_line + 1, 1 )

         call smg$read_keystroke( keyboard, ikey )

         call print_message( ' ', 0 )

         if ( ikey .eq. smg$k_trm_do .or.
     .        ikey .eq. smg$k_trm_ctrlp ) then
            call pd_get_choice( board_id, keyboard, width,
     .                          pull_choices, choice, code )
            code_type = code / 10
            else
            code_type = 0
            code = 0
            end if

         if ( ikey .eq. smg$k_trm_enter .or.
     .        ikey .eq. smg$k_trm_lowercase_q .or.
     .        ikey .eq. smg$k_trm_uppercase_q .or.
     .        ikey .eq. smg$k_trm_lowercase_x .or.
     .        ikey .eq. smg$k_trm_uppercase_x .or.
     .        ikey .eq. smg$k_trm_ctrlz       .or.
     .        ikey .eq. smg$k_trm_f10         .or.
     .        code .eq. 181 ) then
            finished = .true.

            else if ( ikey .eq. smg$k_trm_up ) then
            jj = file_num - 1
            if ( jj .ge. 1 ) file_num = jj
            call update_file_window

            else if ( ikey .eq. smg$k_trm_down ) then
            jj = file_num + 1
            if ( jj .le. num_files ) file_num = jj
            call update_file_window

            else if ( ikey .eq. smg$k_trm_prev_screen .or.
     .		      ikey .eq. smg$k_trm_ctrlb ) then
            jj = file_num - 11
            if ( jj .lt. 1 ) jj = 1
	    file_num = jj
            call update_file_window

            else if ( ikey .eq. smg$k_trm_next_screen .or.
     .		      ikey .eq. smg$k_trm_ctrlf ) then
            jj = file_num + 11
            if ( jj .gt. num_files ) jj = num_files
	    file_num = jj
            call update_file_window

            else if ( ikey .eq. smg$k_trm_lowercase_t .or.
     .		      ikey .eq. smg$k_trm_uppercase_t ) then
            file_num = 1
            call update_file_window

            else if ( ikey .eq. smg$k_trm_lowercase_b .or.
     .		      ikey .eq. smg$k_trm_uppercase_b ) then
            file_num = num_files
            call update_file_window

            else if ( code .eq. 111 .or.
     .                ikey .eq. smg$k_trm_lowercase_d .or.
     .                ikey .eq. smg$k_trm_uppercase_d ) then
            call delete_file

            else if ( code_type .eq. 12 .or.
     .                ikey .eq. smg$k_trm_lowercase_e .or.
     .                ikey .eq. smg$k_trm_uppercase_e ) then
            call edit_file

            else if ( code_type .eq. 13 .or.
     .                ikey .eq. smg$k_trm_lowercase_m .or.
     .                ikey .eq. smg$k_trm_uppercase_m ) then
            call move_file

            else if ( code_type .eq. 14 .or.
     .                ikey .eq. smg$k_trm_lowercase_o .or.
     .                ikey .eq. smg$k_trm_uppercase_o ) then
            call file_options( code )

            else if ( code_type .eq. 15 .or.
     .                ikey .eq. smg$k_trm_lowercase_p .or.
     .                ikey .eq. smg$k_trm_uppercase_p ) then
            call print_file

            else if ( code_type .eq. 16 .or.
     .                ikey .eq. smg$k_trm_lowercase_r .or.
     .                ikey .eq. smg$k_trm_uppercase_r ) then
            call rename_file

            else if ( code_type .eq. 17 .or.
     .                ikey .eq. smg$k_trm_lowercase_h .or.
     .                ikey .eq. smg$k_trm_uppercase_h ) then
            call help_filer
            end if

      end do

      call smg$begin_pasteboard_update( board_id )

      call smg$unpaste_virtual_display( file_window, board_id )
      call define_smg_layout

      call smg$end_pasteboard_update( board_id )

      return
      end
