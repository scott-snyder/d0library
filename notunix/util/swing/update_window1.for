      subroutine update_window1

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    start

      if ( use_window1 ) then

      start = ( width - (len_disk + node(node_num).length) ) / 2
      if ( start .le. 0 ) start = 1

      call smg$erase_line( window1, 1, 1 )

      call smg$put_chars( window1,
     .                    disk(1:len_disk)//
     .                    node(node_num).spec(1:node(node_num).length),
     .                    1, start,, smg$m_underline )

      end if

      return
      end
