      subroutine move_node( num, parent )

      include    'd0$inc:swing.inc'

      logical    found_node, greater
      integer    num, ii, jj, parent, ptr(0:7)

      found_node = .false.
      ii = 1

      do while ( .not. found_node .and. ii .le. num_nodes )
         if ( node(ii).sister .eq. num ) then
            found_node = .true.
            node(ii).sister = node(num).sister

            else if ( node(ii).child .eq. num ) then
            found_node = .true.
            node(ii).child = node(num).sister
            end if
         ii = ii + 1
      end do

      if ( .not. found_node ) return

      node(num).sister = 0

      if ( parent .eq. 0 ) then
         ii = cur_level - 1
         jj = cur_line
         do while( node_pointer(ii,jj) .eq. 0 .and. jj .ge. 1 )
            jj = jj - 1
         end do
         if ( jj .ge. 1 ) then
            parent = node_pointer(ii,jj)
            else
            parent = 1
            end if
         end if

      if ( node(parent).child .eq. 0 ) then
         node(parent).child = num

         else
         ii = node(parent).child
         if ( node(num).name .lt. node(ii).name ) then
            node(num).sister = node(parent).child
            node(parent).child = num

            else
            greater = .true.
            do while ( greater )
               if ( node(ii).sister .eq. 0 ) then
                  node(ii).sister = num
                  greater = .false.

                  else
                  jj = ii
                  ii = node(ii).sister
                  if ( node(num).name .lt. node(ii).name ) then
                     node(jj).sister = num
                     node(num).sister = ii
                     greater = .false.
                     end if
                  end if
            end do
            end if
         end if

      ptr(0) = num

      call change_spec( parent, ptr(0) )
      ptr(1) = node(ptr(0)).child
      do while( ptr(1) .ne. 0 )
        call change_spec( ptr(0), ptr(1) )
        ptr(2) = node(ptr(1)).child
        do while( ptr(2) .ne. 0 )
          call change_spec( ptr(1), ptr(2) )
          ptr(3) = node(ptr(2)).child
          do while( ptr(3) .ne. 0 )
            call change_spec( ptr(2), ptr(3) )
            ptr(4) = node(ptr(3)).child
            do while( ptr(4) .ne. 0 )
              call change_spec( ptr(3), ptr(4) )
              ptr(5) = node(ptr(4)).child
              do while( ptr(5) .ne. 0 )
                call change_spec( ptr(4), ptr(5) )
                ptr(6) = node(ptr(5)).child
                do while( ptr(6) .ne. 0 )
                  call change_spec( ptr(5), ptr(6) )
                  ptr(7) = node(ptr(6)).child
                  do while( ptr(7) .ne. 0 )
                    call change_spec( ptr(6), ptr(7) )
                    ptr(7) = node(ptr(7)).sister
                  end do
                  ptr(6) = node(ptr(6)).sister
                end do
                ptr(5) = node(ptr(5)).sister
              end do
              ptr(4) = node(ptr(4)).sister
            end do
            ptr(3) = node(ptr(3)).sister
          end do
          ptr(2) = node(ptr(2)).sister
        end do
        ptr(1) = node(ptr(1)).sister
      end do

      return
      end
