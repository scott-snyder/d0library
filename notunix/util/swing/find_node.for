      logical function find_node( dir_spec, ptr )

      include    'd0$inc:swing.inc'

      character  dir_spec*(*)
      integer    ii, jj, ptr
      logical    found_node

      ii = len( dir_spec )
      do while ( dir_spec(ii:ii) .eq. ' ' .and. ii .gt. 0 )
         ii = ii - 1
      end do

      jj = 1
      found_node = .false.
      do while ( .not. found_node .and. jj .le. max_nodes)		!LAW
         if ( node(jj).length .ne. 0 ) then
         if ( node(jj).spec(1:node(jj).length) .eq. dir_spec(1:ii) )then
            found_node = .true.
            ptr = jj
            end if
         end if
         jj = jj + 1
      end do

      find_node = found_node

      return
      end
