      subroutine move_file

c     Craig Young               3-AUG-87

c     This subroutine controls the moving of the current file to another
c     directory.  The new host directory is determined in the same manner
c     as in the subroutine Rename_Directory.

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      character  file*255, message*255
      integer    istat, lib$rename_file
      integer    from_num, from_line, from_level, old_line, old_level
      integer    old_file, old_top, old_bottom
      integer    ikey, len_message, ii, jj
      logical    finished

c     Check if current file is a directory.  If so, abort move.

      istat = index( fnode(file_num).spec(1:), '.DIR;1' )

      if ( istat .eq. 0 ) then
         old_file = file_num
         old_top =  top_file_line
         old_bottom = bottom_file_line

         from_num = node_num
         from_line = cur_line
         from_level = cur_level
         node(from_num).rend = smg$m_reverse + smg$m_blink

         call smg$unpaste_virtual_display( file_window, board_id )
         call smg$change_rendition( window2, from_line, from_level*17+1,
     .                              1, 12, node(from_num).rend )

         call print_message( 'Travel to new host directory and press '//
     .                       'RETURN - Press any other key to abort',0)
         call smg$set_cursor_abs( window2, from_line, from_level*17+1 )

         finished = .false.

         do while ( .not. finished )

            call smg$read_keystroke( keyboard, ikey )

            old_line = cur_line
            old_level = cur_level
            old_rend = node(node_num).rend

            if ( ikey .eq. smg$k_trm_cr .or.
     .           ikey .eq. smg$k_trm_enter ) then
               finished = .true.

               else if ( ikey .eq. smg$k_trm_up ) then
               ii = cur_level
               jj = cur_line - 1
               do while( node_pointer(ii,jj) .eq. 0 .and. jj .ge. 1 )
                  jj = jj - 1
               end do
               if ( jj .ge. 1 ) cur_line = jj
               call update_screen( old_line, old_level )

               else if ( ikey .eq. smg$k_trm_down ) then
               ii = cur_level
               jj = cur_line + 1
               do while( node_pointer(ii,jj) .eq. 0 .and.
     .                   jj .le. num_lines )
                  jj = jj + 1
               end do
               if ( jj .le. num_lines ) cur_line = jj
               call update_screen( old_line, old_level )

               else if ( ikey .eq. smg$k_trm_right ) then
               ii = cur_level + 1
               jj = cur_line
               do while( node_pointer(ii,jj) .eq. 0 .and.
     .                   ii .le. MAX_LEVELS )
                  ii = ii + 1
               end do
               if ( ii .le. MAX_LEVELS ) cur_level = ii
               call update_screen( old_line, old_level )

               else if ( ikey .eq. smg$k_trm_left .and.
     .                   cur_level .ge. 1 ) then
               ii = cur_level - 1
               jj = cur_line
               do while( node_pointer(ii,jj) .eq. 0 .and. jj .ge. 1 )
                  jj = jj - 1
               end do
               if ( jj .ge. 1 ) then
                  cur_level = ii
                  cur_line = jj
                  end if
               call update_screen( old_line, old_level )

               else
               finished = .true.
               end if

            call smg$set_cursor_abs( window2, cur_line, cur_level*17+1 )

         end do

         node(from_num).rend = smg$m_reverse

         call smg$change_rendition( window2, from_line, from_level*17+1,
     .                              1, 12, node(from_num).rend )

         if ( ikey .eq. smg$k_trm_cr .or.
     .        ikey .eq. smg$k_trm_enter ) then

            istat = lib$rename_file( fnode(old_file).spec,
     .                               '[]'//fnode(old_file).name )

            if ( istat ) then
               call print_message( 'File has been moved', 0 )

               else
               call sys$getmsg( %val(istat), len_message, message,
     .                          %val(1), )
               call print_message( message(1:len_message), 0 )
               end if
         else
            call smg$erase_display( window3 )
            end if

         call load_display

         cur_line = node(from_num).line
         cur_level = node(from_num).level

         call update_screen( cur_line, cur_level )

         call load_files

         if ( old_file .le. num_files ) then
            file_num = old_file
         else
            file_num = 1
            end if
         top_file_line = old_top
         bottom_file_line = old_bottom

         call smg$paste_virtual_display( file_window, board_id, 10, 40)
         call update_file_window

      else
         call print_message('Cannot move a directory with the filer.',0)
         end if

      return
      end
