      subroutine delete_directory( code )

      include    'd0$inc:swing.inc'
      include    '($ssdef)'

      character  spec(0:MAX_LEVELS)*255, search(0:MAX_LEVELS)*255
      character  term*5, string*3, message*255, name*50
      integer    iterm, len_string, code, jj
      integer    sys$getmsg, istat, len_message, len(0:MAX_LEVELS)
      integer    icont(0:MAX_LEVELS), lib$find_file, ii
      logical    found_node

      call print_message( ' ', 0 )
      call smg$set_cursor_abs( window3, 1, 1 )
      call smg$read_string( keyboard, string,
     .                      'Enter YES to to delete this direc'//
     .                      'tory and all directories below it: ',
     .                      3,,,,len_string,, window3 )

      call str$upcase( string, string )

      if ( string .eq. 'YES' ) then

         do_save = .true.

         call print_message('Deleting current directory structure...',0)

         delete_problem = .false.
         search(0)=node(node_num).spec(1:node(node_num).length)//'*.dir'

         icont(0) = 0
         do while ( lib$find_file( search(0), spec(0), icont(0) ) )
          call file_to_dir( spec(0), search(1), len(1), name )
          search(1) = search(1)(1:len(1))//'*.dir'
          icont(1) = 0
          do while ( lib$find_file( search(1), spec(1), icont(1) ) )
           call file_to_dir( spec(1), search(2), len(2), name )
           search(2) = search(2)(1:len(2))//'*.dir'
           icont(2) = 0
           do while ( lib$find_file( search(2), spec(2), icont(2) ) )
            call file_to_dir( spec(2), search(3), len(3), name )
            search(3) = search(3)(1:len(3))//'*.dir'
            icont(3) = 0
            do while ( lib$find_file( search(3), spec(3), icont(3) ) )
             call file_to_dir( spec(3), search(4), len(4), name )
             search(4) = search(4)(1:len(4))//'*.dir'
             icont(4) = 0
             do while ( lib$find_file( search(4), spec(4), icont(4) ) )
              call file_to_dir( spec(4), search(5), len(5), name )
              search(5) = search(5)(1:len(5))//'*.dir'
              icont(5) = 0
              do while ( lib$find_file( search(5), spec(5), icont(5) ) )
               call file_to_dir( spec(5), search(6), len(6), name )
               search(6) = search(6)(1:len(6))//'*.dir'
               icont(6) = 0
               do while ( lib$find_file( search(6), spec(6), icont(6) ))
                call file_to_dir( spec(6), search(7), len(7), name )
                call delete_files( search(7)(1:len(7)) )
               end do
               call lib$find_file_end( icont(6) )
               call delete_files( search(6)(1:len(6)) )
              end do
              call lib$find_file_end( icont(5) )
              call delete_files( search(5)(1:len(5)) )
             end do
             call lib$find_file_end( icont(4) )
             call delete_files( search(4)(1:len(4)) )
            end do
            call lib$find_file_end( icont(3) )
            call delete_files( search(3)(1:len(3)) )
           end do
           call lib$find_file_end( icont(2) )
           call delete_files( search(2)(1:len(2)) )
          end do
          call lib$find_file_end( icont(1) )
          call delete_files( search(1)(1:len(1)) )
         end do
         call lib$find_file_end( icont(0) )
         call delete_files( search(0)(1:node(node_num).length) )

         if ( cur_level .ge. 1 ) then
            ii = cur_level - 1
            jj = cur_line
            do while( node_pointer(ii,jj) .eq. 0 .and. jj .ge. 1 )
               jj = jj - 1
            end do
            if ( jj .ge. 1 ) then
               node_num = node_pointer(ii,jj)
               else
               node_num = 1
               end if
            else
            node_num = 1
            end if

         call adjust_node_pointers
         call load_display

         cur_level = node(node_num).level
         cur_line = node(node_num).line

         call update_screen( cur_line, cur_level )

         if ( delete_problem ) then
            call print_message( 'Attempted to delete subdirectory - '//
     .           'but some files could not be deleted', 0 )
            else
            call print_message( 'Deleted subdirectory structure', 0 )
            end if
         else

         call print_message( 'No directories deleted', 0 )
         end if

      return
      end
