      logical function check_directory_move( from_num, cur_num )

      include 'd0$inc:swing.inc'

      integer from_num, cur_num, from_levels, ptr(0:7)

      from_levels = 1

      ptr(0) = from_num

      ptr(1) = node(ptr(0)).child
      do while( ptr(1) .ne. 0 )
        if ( from_levels .lt. 2 ) from_levels = 2
        ptr(2) = node(ptr(1)).child
        do while( ptr(2) .ne. 0 )
          if ( from_levels .lt. 3 ) from_levels = 3
          ptr(3) = node(ptr(2)).child
          do while( ptr(3) .ne. 0 )
            if ( from_levels .lt. 4 ) from_levels = 4
            ptr(4) = node(ptr(3)).child
            do while( ptr(4) .ne. 0 )
              if ( from_levels .lt. 5 ) from_levels = 5
              ptr(5) = node(ptr(4)).child
              do while( ptr(5) .ne. 0 )
                if ( from_levels .lt. 6 ) from_levels = 6
                ptr(6) = node(ptr(5)).child
                do while( ptr(6) .ne. 0 )
                  if ( from_levels .lt. 7 ) from_levels = 7
                  ptr(7) = node(ptr(6)).child
                  do while( ptr(7) .ne. 0 )
                    if ( from_levels .lt. 8 ) from_levels = 8
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

      if ( node(cur_num).level + from_levels .gt. 7 ) then
         check_directory_move = .false.
         else
         check_directory_move = .true.
         end if

      return
      end
