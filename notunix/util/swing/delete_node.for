      subroutine delete_node( ptr )

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      logical    found_node
      integer    ptr, ii

      found_node = .false.
      ii = 1

      do while ( .not. found_node .and. ii .le. num_nodes )
         if ( node(ii).sister .eq. ptr ) then
            found_node = .true.
            node(ii).sister = node(ptr).sister

            else if ( node(ii).child .eq. ptr ) then
            found_node = .true.
            node(ii).child = node(ptr).sister
            end if
         ii = ii + 1
      end do

      if ( found_node ) then
         node(ptr).name = ' '
         call smg$put_chars( window2, node(ptr).name,
     .                                node(ptr).line,
     .                                node(ptr).level * 17 + 1,,
     .                                node(ptr).rend )
         node(ptr).level = 0
         node(ptr).length = 0
         node(ptr).sister = 0
         node(ptr).child = 0
         end if

      return
      end
