      subroutine add_node( new_dir, parent )

      include    'd0$inc:swing.inc'

      character new_dir*42, spec*255
      integer   parent, len, new_node, free_node, ii, jj
      logical   greater

      call str$trim( new_dir, new_dir, len )

      spec = node(parent).spec(1:node(parent).length)//
     .       new_dir(1:len)//'.DIR;1'

      new_node = free_node()

      call file_to_dir( spec,
     .                  node(new_node).spec,
     .                  node(new_node).length,
     .                  node(new_node).name )

      if ( node(parent).child .eq. 0 ) then
         node(parent).child = new_node

         else
         ii = node(parent).child
         if ( node(new_node).name .lt. node(ii).name ) then
            node(new_node).sister = node(parent).child
            node(parent).child = new_node

            else
            greater = .true.
            do while ( greater )
               if ( node(ii).sister .eq. 0 ) then
                  node(ii).sister = new_node
                  greater = .false.

                  else
                  jj = ii
                  ii = node(ii).sister
                  if ( node(new_node).name .lt. node(ii).name ) then
                     node(jj).sister = new_node
                     node(new_node).sister = ii
                     greater = .false.
                     end if
                  end if
            end do
            end if
         end if

      return
      end
