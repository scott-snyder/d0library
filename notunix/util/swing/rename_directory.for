      subroutine rename_directory( code )

      include    'd0$inc:swing.inc'
      include    '($ssdef)'
      include    '($smgdef)'

      character  new_dir*42, key, string*39, message*255, file*255
      integer    ikey, len_string, lib$rename_file, code, parent
      integer    sys$getmsg, istat, len_message, ipos, from_level
      integer    old_line, old_level, from_num, from_line, ii, jj
      logical    dir_to_file, finished, check_directory_move

      if ( code .eq. 20 ) then

         call print_message( ' ', 0 )
         call smg$set_cursor_abs( window3, 1, 1 )
         call smg$read_string( keyboard, string,
     .                         'Enter new name to give directory: ',
     .                         39,,,,len_string,, window3 )

         new_dir = ' '
         jj = 0

         do ii = 1, len_string
            if (string(ii:ii) .ne. '[' .and. string(ii:ii) .ne. ']'.and.
     .         string(ii:ii) .ne. '.' .and. string(ii:ii) .gt. ' ' .and.
     .         string(ii:ii) .ne. ';' ) then
               jj = jj + 1
               new_dir(jj:jj) = string(ii:ii)
               end if
         end do

         call str$upcase( new_dir, new_dir )

         if ( jj .ne. 0 ) then
            if ( dir_to_file( node(node_num).spec,
     .                        node(node_num).length,
     .                        file, ipos ) ) then
               istat = lib$rename_file( file,
     .                                  new_dir(1:jj)//'.DIR;1',,,
     .                                  1 )

               if ( istat .eq. ss$_normal ) then
                  call file_to_dir( file(1:ipos)//new_dir(1:jj)//'.DIR',
     .                              node(node_num).spec,
     .                              node(node_num).length,
     .                              node(node_num).name )

                  parent = 0
                  call move_node( node_num, parent )

                  call adjust_node_pointers

                  call load_display

                  cur_line = node(node_num).line
                  cur_level = node(node_num).level

                  call update_screen( cur_line, cur_level )

                  call print_message( 'Subdirectory renamed', 0 )

                  do_save = .true.

                  else
                  call sys$getmsg( %val(istat), len_message, message,
     .                             %val(1), )
                  call print_message( message(1:len_message), 0 )
                  end if
               end if
            else
            call smg$erase_display( window3 )
            end if

         else if ( code .eq. 30 ) then

         from_num = node_num
         from_line = cur_line
         from_level = cur_level
         node(from_num).rend = smg$m_reverse + smg$m_blink

         call smg$change_rendition( window2, from_line, from_level*17+1,
     .                              1, 12, node(from_num).rend )

         call print_message( 'Travel to new parent directory and hit '//
     .                       'RETURN - Hit any other key to abort', 0 )
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
               do while( jj .ge. 1 .and. node_pointer(ii,jj) .eq. 0 )	!LAW
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

            if ( .not. check_directory_move( from_num, node_num ) ) then
               call update_screen( cur_line, cur_level )
               call print_message( 'Rename would cause too great a '//
     .            'directory depth', 0 )
               return
               end if

            if ( dir_to_file( node(from_num).spec,
     .                        node(from_num).length,
     .                        file, ipos ) ) then

               istat = lib$rename_file( file,
     .                 node(node_num).spec(1:node(node_num).length)//
     .                 '*.dir;1',,, 1 )

               if ( istat ) then
                  call move_node( from_num, node_num )

                  call adjust_node_pointers

                  call load_display

                  cur_line = node(from_num).line
                  cur_level = node(from_num).level

                  call update_screen( cur_line, cur_level )

                  call print_message( 'Subdirectory has been moved', 0 )

                  do_save = .true.

                  else
                  call sys$getmsg( %val(istat), len_message, message,
     .                             %val(1), )
                  call print_message( message(1:len_message), 0 )
                  end if
               end if
            else
            call smg$erase_display( window3 )
            end if
         else
         call smg$erase_display( window3 )
         end if

      return
      end
