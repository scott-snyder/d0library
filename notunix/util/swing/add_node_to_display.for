      subroutine add_node_to_display( num )

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    column, num, level

      node(num).rend = smg$m_reverse

      level =  node(num).level
      column = level * 17 + 1
      line =   node(num).line

      call smg$put_chars( window2, node(num).name, line, column,,
     .                    node(num).rend )
      call smg$draw_line( window2, line, column-3, line, column-1 )

      if ( level .eq. last_level ) then
         call smg$draw_line( window2, line-1, column-3, line, column-3 )
         else if ( level .eq. last_level + 1 ) then
         call smg$draw_line( window2, line, column-5, line, column-2 )
         else if ( level .lt. last_level ) then
         call smg$draw_line( window2, last_line(level), column-3,
     .                       line, column-3 )
         end if

      if ( .not. found .and. root(2:len_root-1)
     .     .eq. node(num).spec(2:node(num).length-1) ) then
         found = .true.
         cur_line = line
         cur_level = level
         end if

      last_level = level
      last_line(level) = line

      return
      end
