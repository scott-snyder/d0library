      subroutine edit_file

c     Craig Young               3-AUG-87

c     This subroutine spawns a process which calls the editor as specified
c     by Swing$Edit or defaults to TPU.  When the process is terminated,
c     the swing process is continued.

      include    'd0$inc:swing.inc'
      include    '($ssdef)'

      character  string*50, logical_name*50, command*120
      integer*2  old_file, old_top, old_bottom, len_string
      integer*4  translate_logical, screen_num, lib$spawn, istat

      old_file = file_num                       !Save current cursor position
      old_top = top_file_line                   !Save window range
      old_bottom = bottom_file_line

c     Check if current file is a directory.  If so, abort edit.

      istat = index( fnode(file_num).spec(1:), '.DIR;1' )

      if ( istat .eq. 0 ) then
         call smg$save_physical_screen( board_id, screen_num )
         call print_message( ' ', 0 )
         call smg$set_cursor_abs( window3, 2, 1 )

         logical_name = 'SWING$EDIT'            !Check for user's editor
         istat = translate_logical( logical_name, string )
         if ( istat .eq. ss$_normal ) then
            call str$trim( string, string, len_string )
            command = string(1:len_string)//' '//fnode(file_num).spec
            call str$trim( command, command, len_string )
            istat = lib$spawn( command(1:len_string) )
            if ( istat .ne. ss$_normal ) call exit(istat)
         else
            call tpu$tpu ( 'tpu '//fnode(file_num).name )
            end if

         call smg$restore_physical_screen( board_id, screen_num )
         call load_files
         file_num = old_file                    !Reset cursor position
         top_file_line = old_top                !Reset window range
         bottom_file_line = old_bottom
         call update_file_window

      else
         call print_message( 'Cannot edit a directory.', 0 )
         end if

      return
      end
