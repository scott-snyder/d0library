      subroutine adjust_node_pointers

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer ll, jj, ptr(0:7), ii

      do ll = 1, MAX_LINES
         do jj = 0, MAX_LEVELS
            node_pointer(jj,ll) = 0
         end do
      end do

      do jj = 0, MAX_LEVELS
         ptr(jj) = 0
      end do

      ll = 1     !LINE
      ptr(0) = 1

      node_pointer(0,ll) = 1
      ptr(1) = node(ptr(0)).child
      do while( ptr(1) .ne. 0 )
        node_pointer(1,ll) = ptr(1)
        node(ptr(1)).line = ll
        node(ptr(1)).level = 1
        ptr(2) = node(ptr(1)).child
        do while( ptr(2) .ne. 0 )
          node_pointer(2,ll) = ptr(2)
          node(ptr(2)).line = ll
          node(ptr(2)).level = 2
          ptr(3) = node(ptr(2)).child
          do while( ptr(3) .ne. 0 )
            node_pointer(3,ll) = ptr(3)
            node(ptr(3)).line = ll
            node(ptr(3)).level = 3
            ptr(4) = node(ptr(3)).child
            do while( ptr(4) .ne. 0 )
              node_pointer(4,ll) = ptr(4)
              node(ptr(4)).line = ll
              node(ptr(4)).level = 4
              ptr(5) = node(ptr(4)).child
              do while( ptr(5) .ne. 0 )
                node_pointer(5,ll) = ptr(5)
                node(ptr(5)).line = ll
                node(ptr(5)).level = 5
                ptr(6) = node(ptr(5)).child
                do while( ptr(6) .ne. 0 )
                  node_pointer(6,ll) = ptr(6)
                  node(ptr(6)).line = ll
                  node(ptr(6)).level = 6
                  ptr(7) = node(ptr(6)).child
                  do while( ptr(7) .ne. 0 )
                    node_pointer(7,ll) = ptr(7)
                    node(ptr(7)).line = ll
                    node(ptr(7)).level = 7
                    ptr(7) = node(ptr(7)).sister
                    if ( ptr(7) .ne. 0 ) ll = ll + 1
                  end do
                  ptr(6) = node(ptr(6)).sister
                  if ( ptr(6) .ne. 0 ) ll = ll + 1
                end do
                ptr(5) = node(ptr(5)).sister
                if ( ptr(5) .ne. 0 ) ll = ll + 1
              end do
              ptr(4) = node(ptr(4)).sister
              if ( ptr(4) .ne. 0 ) ll = ll + 1
            end do
            ptr(3) = node(ptr(3)).sister
            if ( ptr(3) .ne. 0 ) ll = ll + 1
          end do
          ptr(2) = node(ptr(2)).sister
          if ( ptr(2) .ne. 0 ) ll = ll + 1
        end do
        ptr(1) = node(ptr(1)).sister
        if ( ptr(1) .ne. 0 ) ll = ll + 1
      end do

      lowest_level = 0
      do ii = 1, num_nodes
         if ( node(ii).level .gt. lowest_level )
     .      lowest_level = node(ii).level
      end do

c      if ( lowest_level .gt. 7 ) then
c         call print_message( 'Directory nesting is to deep', 1 )
c         end if

      num_lines = ll

      return
      end
