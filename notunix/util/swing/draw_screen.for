      subroutine draw_screen

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

C
C    To cope with strange terminal screen lengths
C        19 --- > linest - 5
C        20 --- > linest - 4
C        23 --- > linest - 1
C
C
C       Added by EH Perkins, Aug 16, 1987
C

	integer linest
	common /length/ linest

      integer    ii, jj, kk, smg$change_pbd_characteristics
      integer    smg$change_rendition

      call smg$begin_pasteboard_update( board_id )

      call smg$paste_virtual_display( window2, board_id, 3, 1 )		!LAW
      call smg$paste_virtual_display( window1, board_id, 2, 1 )
      call smg$paste_virtual_display( window3, board_id,
     .                                (linest - 1), 1 )

      call smg$set_display_scroll_region( window3, 1, 2 )

      call pd_draw_bar( board_id )

      top_line = 1
      bottom_line = (linest - 4)

      node_num = node_pointer( cur_level, cur_line )

      call smg$change_rendition( window2, cur_line, cur_level*17+1,
     .                           1, 12,
     .                           smg$m_bold + node(node_num).rend )

      if ( cur_line .gt. bottom_line ) then
	 top_line = cur_line - (linest - 5)
	 bottom_line = cur_line
	 call smg$move_virtual_display( window2, board_id,
     .                                  (linest - 1) - cur_line, 1 )
	 else if ( cur_line .lt. top_line ) then
	 top_line = cur_line
	 bottom_line = cur_line + (linest - 5)
         call smg$move_virtual_display( window2, board_id,
     .                                     cur_line, 1 )
         end if

      call update_window1

      call smg$end_pasteboard_update( board_id )

      call smg$set_cursor_abs( window2, cur_line, cur_level*17+1 )

      update = .true.

      return
      end
