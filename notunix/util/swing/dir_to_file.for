      logical function dir_to_file( dir, len_dir, file, ipos )

      implicit none
      character dir*(*), file*(*)
      integer   len_dir, ii, ipos, istat

      ii = len_dir
      do while ( dir(ii:ii) .ne. '.' .and. ii .gt. 0 )
         ii = ii - 1
      end do

      if ( ii .ne. 0 ) then
         dir_to_file = .true.
         file = dir

         istat = index( file(1:), '<' )
         if ( istat .ne. 0 ) then
            file(istat:istat) = '['
            end if

         file(ii:ii) = ']'
         file(len_dir:) = '.dir;1'
         ipos = ii

         else
         call print_message( 'Operation not allowed on main directory',
     .                       0 )
         dir_to_file = .false.
         end if

      return
      end
