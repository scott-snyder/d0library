      subroutine load_display

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    ii, istat, jj, kk, level
      integer     smg$change_pbd_characteristics,smg$change_rendition

      using_screen = .true.

      if ( .not. found ) then
         cur_level = 0
         cur_line = 1
         end if

      last_level = 0
      line = 0
      do ii = 0, MAX_LEVELS
         last_line(ii) = 1
      end do

      if ( lowest_level .gt. 4 .and. width .ne. 132 ) then
         width = 132
         call pd_undraw_bar( board_id )
         call smg$erase_display( window1 )
         call smg$erase_display( window2 )
         call smg$erase_display( window3 )
         istat = smg$change_pbd_characteristics( board_id,132,,24 )
         call smg$set_display_scroll_region( window3, 1, 2 )
         call pd_load_bar( width, pull_choices)
         call pd_draw_bar( board_id )

         else if ( lowest_level .le. 4 .and. width .ne. 80 ) then
         width = 80
         call pd_undraw_bar( board_id )
         call smg$erase_display( window1 )
         call smg$erase_display( window2 )
         call smg$erase_display( window3 )
         istat = smg$change_pbd_characteristics( board_id,80,,24 )
         call smg$set_display_scroll_region( window3, 1, 2 )
         call pd_load_bar( width, pull_choices)
         call pd_draw_bar( board_id )
         end if

      call smg$begin_pasteboard_update( board_id )

      call smg$erase_display( window2 )

      do jj = 1, num_lines
         do level = 0, MAX_LEVELS
            if ( node_pointer(level,jj) .ne. 0 )
     .         call add_node_to_display( node_pointer(level,jj) )
         end do
      end do

c     PUT UNDERLINES ON THE LEAF NODES

      do jj = 2, num_nodes
         do ii = 2, MAX_LEVELS
            if ( node_pointer(ii,jj) .ne. 0 .and.
     .           node_pointer(ii-1,jj) .ne. 0 .and.
     .           node_pointer(ii,jj-1) .ne. 0 ) then
               kk = node_pointer( ii, jj-1 )
               node(kk).rend = smg$m_underline + smg$m_reverse
               istat = smg$change_rendition( window2, node(kk).line,
     .                                       node(kk).level*17+1,
     .                                       1, 12, node(kk).rend )
               end if
         end do
      end do

      call smg$end_pasteboard_update( board_id )

      if ( .not. found )
     .   call print_message( 'The current directory was not found in'//
     .                       ' your save file', 0 )

      return
      end
