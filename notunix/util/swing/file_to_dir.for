      logical function file_to_dir( file, dir, len_dir, name )

      implicit none
      character dir*(*), file*(*), name*(*), left_bracket
      integer   len_dir, kk, ii, len_node, jj, istat

      kk = 1
      do while ( file(kk:kk) .ne. '[' .and. file(kk:kk) .ne. '<' )
         kk = kk + 1
      end do
      dir = file(kk:)

      ii = 1
      do while ( dir(ii:ii) .ne. ']' .and. dir(ii:ii) .ne. '>' )
         ii = ii + 1
      end do

      jj = ii
      do while ( dir(jj:jj) .ne. '.' )
         jj = jj + 1
      end do

      dir(ii:ii) = '.'
      istat = index( dir(1:), '<' )
      if ( istat .ne. 0 ) then
         dir(jj:) = '>'
         left_bracket = '<'
      else
         dir(jj:) = ']'
         left_bracket = '['
         end if

      len_dir = jj

      istat = index( dir(1:), '000000.' )
      if ( istat .ne. 0 ) then
         dir = dir(1:istat-1)//dir(istat+7:)
         len_dir = len_dir - 7
         ii = ii - 7
         if ( dir(ii:ii) .ne. '.' ) ii = ii + 1
         end if

      len_node = len_dir - ii - 1
      if ( len_node .le. 9 ) then
         name = left_bracket//dir(ii:len_dir)
         else
         name = left_bracket//dir(ii:ii+9)//'*'
         end if

      return
      end
