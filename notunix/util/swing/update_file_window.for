      subroutine update_file_window

c     Craig Young               3-AUG-87

c     This subroutine updates the filer window to reflect movement of the
c     cursor and scrolling.

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    ii

      call smg$begin_pasteboard_update( board_id )

c     Check if scrolling required.  If so, scroll a half window.

      if ( file_num .eq. bottom_file_line + 1 ) then    !If cursor at bottom
         top_file_line = top_file_line + 6              !Scroll up
         bottom_file_line = bottom_file_line + 6

         else if ( file_num .eq. top_file_line - 1 ) then   !If cursor at top
         top_file_line = top_file_line - 6              !Scroll down
         bottom_file_line = bottom_file_line - 6

         else if ( file_num .gt. bottom_file_line ) then
         top_file_line = file_num
         bottom_file_line = top_file_line + 11

         else if ( file_num .lt. top_file_line ) then
         top_file_line = file_num
         bottom_file_line = top_file_line + 11

         end if

      if ( bottom_file_line .gt. num_files ) then
	top_file_line = num_files - 11
      end if

      if ( top_file_line .lt. 1 ) then
	top_file_line = 1
	bottom_file_line = 12
      end if

      if ( bottom_file_line .gt. top_file_line + 11 )
     .	   bottom_file_line = top_file_line + 11

      call smg$set_cursor_abs( file_window, 1, 1 )

      do ii = top_file_line, bottom_file_line           !Reprint file names
         call smg$put_line( file_window, fnode(ii).name )
      end do                                            !for new range

      call smg$change_rendition( file_window,
     .                           file_num - top_file_line + 1,
     .                           2, 1, 24, smg$m_reverse )

      call smg$end_pasteboard_update( board_id )

      return
      end
