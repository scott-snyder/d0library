      subroutine create_directory( code )

      include    'd0$inc:swing.inc'
      include    '($ssdef)'

      character  new_dir*42, term*5, string*39, message*255
      integer    iterm, len_string, ii, jj
      integer    lib$create_dir
      integer    sys$getmsg, istat, len_message, code

      call print_message( ' ', 0 )
      call smg$set_cursor_abs( window3, 1, 1 )
      call smg$read_string( keyboard, string,
     .                      'New subdirectory name: ',
     .                      39,,,,len_string,, window3 )

      new_dir = ' '
      jj = 0

      do ii = 1, len_string
         if ( string(ii:ii) .ne. '[' .and. string(ii:ii) .ne. ']' .and.
     .        string(ii:ii) .ne. '.' .and. string(ii:ii) .gt. ' ' ) then
            jj = jj + 1
            new_dir(jj:jj) = string(ii:ii)
            end if
      end do

      call str$upcase( new_dir, new_dir )

      if ( jj .ne. 0 ) then
         istat = lib$create_dir( '[.'//new_dir(1:jj)//']' )

         if ( istat .eq. ss$_created ) then

            do_save = .true.

            call add_node( new_dir(1:jj), node_num )

            call adjust_node_pointers

            call load_display

            call update_screen( cur_line, cur_level )

            call print_message( 'Created new subdirectory', 0 )

            else if ( .not. istat ) then
            call sys$getmsg( %val(istat), len_message, message,
     .                       %val(1), )
            call print_message( message(1:len_message), 0 )

            else
            call smg$erase_display( window3 )
            end if

         else
         call smg$erase_display( window3 )
         end if

      return
      end
