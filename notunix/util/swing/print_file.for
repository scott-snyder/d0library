      subroutine print_file

c     Craig Young               3-AUG-87

c     This subroutine sends the current file to the printer as specified
c     by Swing$Print or defaults to $Print.  The terminal is not attached
c     to the spawned process so use of swing can continue.

      include    'd0$inc:swing.inc'
      include    '($ssdef)'
      include    '($clidef)'

      character  string*255, logical_name*64, file*255
      integer    istat, len_string, len_file
      integer*4  translate_logical

c     Check if current file is a directory.  If so, abort print.

      istat = index( fnode(file_num).spec(1:), '.DIR;1' )

      if ( istat .eq. 0 ) then
         call print_message( ' ', 0 )
         call smg$set_cursor_abs( window3, 2, 1 )
         call str$trim( file, fnode(file_num).spec, len_file )

         logical_name = 'SWING$PRINT'           !Check for user's printer
         istat = translate_logical( logical_name, string )
         if ( istat .eq. ss$_normal ) then
            call str$trim( string, string, len_string )
            call lib$spawn( string(1:len_string)
     .                      //'/noidentify/nonotify '
     .                      //file(1:len_file),,,cli$m_nowait)
         else
            call lib$spawn( '$print/noidentify/nonotify '
     .                      //file(1:len_file),,,cli$m_nowait)
            end if

         call print_message( 'Sent file to printer.', 0 )

      else
         call print_message( 'Cannot print a directory.', 0 )
         end if

      return
      end
