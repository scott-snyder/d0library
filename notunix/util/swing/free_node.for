      integer function free_node

      include 'd0$inc:swing.inc'

      integer ii

      if (num_lines .ge. MAX_LINES )
     .   call print_message( 'Directory structure is too large', 1 )

      if ( num_nodes .lt. MAX_NODES ) then
         num_nodes = num_nodes + 1
         node(num_nodes).length = 0
         node(num_nodes).child = 0
         node(num_nodes).sister = 0
         free_node = num_nodes

         else
         ii = 1
         do while ( ii .le. MAX_NODES )
            if ( node(ii).length .eq. 0 ) then
               node(ii).length = 0
               node(ii).child = 0
               node(ii).sister = 0
               free_node = ii
               return
               end if
            ii = ii + 1
         end do
         if ( ii .gt. MAX_NODES )
     .      call print_message( 'Directory structure is too large', 1 )
         end if

      return
      end
