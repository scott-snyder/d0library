      subroutine update_screen( old_line, old_level )

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

C
C    To cope with strange terminal screen lengths
C        19 --- > lenest - 5
C        23 --- > lenest - 1
C
C       Added by EH Perkins, Aug 16, 1987
C

      integer linest
      common /length/ linest

      integer    old_line, old_level, ii, istat
      integer    sys$setddir

      node_num = node_pointer( cur_level, cur_line )

      call smg$begin_pasteboard_update( board_id )

      call smg$change_rendition( window2, old_line, old_level*17+1,
     .                           1, 12, old_rend )
      call smg$change_rendition( window2, cur_line, cur_level*17+1,
     .                           1, 12,
     .                           smg$m_bold + node(node_num).rend )

      call update_window1

      call smg$end_pasteboard_update( board_id )

      if ( cur_line .gt. bottom_line ) then
         do ii = bottom_line+1, cur_line
            call smg$move_virtual_display( window2, board_id,
     .					  (linest - 1) - ii,1)
         end do
         top_line = cur_line - (linest - 5)
         bottom_line = cur_line

         else if ( cur_line .lt. top_line ) then
         do ii = top_line-1, cur_line, -1
            call smg$move_virtual_display( window2, board_id, 4-ii,1)
         end do
         top_line = cur_line
         bottom_line = cur_line + (linest - 5)
         end if

      istat = sys$setddir( node(node_num).spec, %val(0), %val(0) )

      return
      end
