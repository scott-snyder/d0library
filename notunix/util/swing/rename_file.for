      subroutine rename_file

c     Craig Young               3-AUG-87

c     This subroutine prompts the user for a new name for the current file.

      include   'd0$inc:swing.inc'

      integer   istat, len_string, len_message, lib$rename_file
      character string*100, message*255

c     Check if current file is a directory.  If so, abort rename.

      istat = index( fnode(file_num).spec(1:), '.DIR;1' )

      if ( istat .eq. 0 ) then
         call print_message( ' ', 0 )
         call smg$set_cursor_abs( window3, 2, 1 )
         call smg$read_string( keyboard, string, 'New name: ',
     .                         ,,,,len_string,, window3 )
         call str$upcase( string, string )

         istat = lib$rename_file( fnode(file_num).spec, string )

         if ( istat ) then
            call print_message( 'File has been renamed.', 0 )

         else
            call sys$getmsg( %val(istat), len_message, message,
     .                       %val(1), )
            call print_message( message(1:len_message), 0 )
            end if

         call load_files
         call update_file_window

      else
         call print_message('Cannot rename directory with the filer.',0)
         end if

      return
      end
