      subroutine delete_file

c     Craig Young               3-AUG-87

c     This subroutine deletes the current file after verification.

      include   'd0$inc:swing.inc'

      character string*5
      integer   old_file, old_top, old_bottom, istat, len_string

c     Check if current file is a directory.  If so, abort delete.

      istat = index( fnode(file_num).spec(1:), '.DIR;1' )

      if ( istat .eq. 0 ) then
         old_file = file_num                    !Save current position
         old_top  = top_file_line               !and window range
         old_bottom = bottom_file_line

         call print_message( ' ', 0 )
         call smg$set_cursor_abs( window3, 2, 1 )
C
C        Call to smg$read_string changed from
C     .                         ,,,,len_string,, window3 )
C        to
C     .                        3,,,,len_string,, window3 )
C        so it is the same as the delete node confirmation.
C
C        EH Perkins Aug 16, 1987
C
	 call smg$read_string( keyboard, string,
     .                         'Enter YES to delete this file: ',
     .                        3,,,,len_string,, window3 )
         call str$upcase( string, string )

         if ( string .eq. 'YES' ) then
            call lib$delete_file( fnode(file_num).spec )
            call print_message( 'File deleted', 0 )
         else
            call print_message( 'Delete aborted', 0 )
         end if

         call load_files                        !Reload fnode array

         if ( old_file .le. num_files ) then
            file_num = old_file                 !Reset cursor position
         else
            file_num = 1
            end if
         top_file_line = old_top                !Reset window range
         bottom_file_line = old_bottom

         call update_file_window
      else
         call print_message( 'Cannot delete a directory '//
     .                       'with the filer.', 0 )
         end if

      return
      end
