	options	/extend_source
      subroutine load_nodes

      include    'd0$inc:swing.inc'

      integer    istat, error, ii, jj
      integer*2  cli$present, cli$get_value, len_temp
      integer*4  icontext(MAX_LEVELS), lib$find_file, lib$sys_trnlog, ltr
      character  input*255, spec*255, search(0:MAX_LEVELS)*255, tr*255
      character  temp*20

      do ii = 1, MAX_LINES
         do jj = 0, MAX_LEVELS
            node_pointer(jj,ii) = 0
         end do
      end do

      found = .false.
      lowest_level = 0
      last_level = 1
      line = 0
      num_nodes = 0
      node_num = 0

c     If START qualifier was specified, set Main to value of START

      if ( cli$present( 'START' ) ) then
         error = cli$get_value( 'START', spec, len_root )
         if ( .not. error ) call sys$exit( %val(error) )

         call str$upcase( spec, spec )
         if ( spec .eq. 'CURRENT' ) then
            call get_location( disk, len_disk, spec, len_root )
            end if

         ii = 1
         do while ( spec(ii:ii) .ne. '[' .and. spec(ii:ii) .ne. '<' )
            ii = ii + 1
         end do

         if ( ii .ne. 1 ) then
            disk = spec(:ii-1)
            len_disk = ii-1
            call lib$sys_trnlog( 'SYS$DISK', len_temp, temp )
            if ( temp .ne. disk )
     .         call print_message( 'Cannot SWING to another device.',1)
         else
            call lib$sys_trnlog( 'SYS$DISK', len_disk, disk )
            end if

         spec = spec(ii:)
         main = spec
         root = spec
         len_root = len_root - (ii - 1)
         len_main = len_root

      else
         call get_location( disk, len_disk, root, len_root )

         ii = 1
         do while ( root(ii:ii) .ne. '.' .and. root(ii:ii) .ne. ']'
     .              .and. root(ii:ii) .ne. '>' )
            ii = ii + 1
         end do

         istat = index( root(1:), '<' )
         if ( istat .ne. 0 ) then
            root(istat:istat) = '['
            end if

         main = root(:ii-1)//']'
         len_main = ii
         spec = main
         end if

! If the logical name SWING_SAVE exists,
	If(lib$sys_trnlog('SWING_SAVE',lTR,TR,,,%VAL(0)).eq.1) then
! construct a file name like: SWING_SAVE:USERNAME_SWING.SAV
	    main(1:len_main+10)='SWING_SAVE:'//main(2:len_main-1)//'_'
	    len_main=len_main+10
	End If

      ii = 0
      if ( .not. update .and. lib$find_file( main(1:len_main)//
     .     'swing.sav', input, ii ) ) then

         open( unit=1,
     .         readonly,
     .         file=main(1:len_main)//'swing.sav',
     .         status='old',
     .         carriagecontrol='list',
     .         access='sequential',
     .         form='unformatted',
     .         recl=73,
     .         organization='sequential',
     .         recordtype='variable',
     .         err=99 )

         read( 1, err=99 ) num_lines, num_nodes, lowest_level

         do ii = 1, num_lines
            read( 1, err=99 ) (node_pointer(jj,ii), jj=0,MAX_LEVELS)
         end do

         do ii = 1, num_nodes
            read( 1, err=99 ) node(ii)
         end do

         close( unit=1 )

         swing_file_exists = .true.

      else
99       call print_message( 'Searching directory structure...', 0 )

         call append_node( 0, spec, search(1) )

         icontext(1) = 0
         do while ( lib$find_file( search(1), spec, icontext(1) ) )
          if ( index( spec, '000000.DIR;1' ) .eq. 0 ) then
          call append_node( 1, spec, search(2) )
          icontext(2) = 0
          do while ( lib$find_file( search(2), spec, icontext(2) ) )
           call append_node( 2, spec, search(3) )
           icontext(3) = 0
           do while ( lib$find_file( search(3), spec, icontext(3) ) )
            call append_node( 3, spec, search(4) )
            icontext(4) = 0
            do while ( lib$find_file( search(4), spec, icontext(4) ) )
             call append_node( 4, spec, search(5) )
             icontext(5) = 0
             do while ( lib$find_file( search(5), spec, icontext(5) ) )
              call append_node( 5, spec, search(6) )
              icontext(6) = 0
              do while ( lib$find_file( search(6), spec, icontext(6) ) )
               call append_node( 6, spec, search(7) )
               icontext(7) = 0
               do while ( lib$find_file( search(7), spec, icontext(7) ))
                call append_node( 7, spec, search(0) )
               end do
               call lib$find_file_end( icontext(7) )
              end do
              call lib$find_file_end( icontext(6) )
             end do
             call lib$find_file_end( icontext(5) )
            end do
            call lib$find_file_end( icontext(4) )
           end do
           call lib$find_file_end( icontext(3) )
          end do
          call lib$find_file_end( icontext(2) )
         end if
         end do
         call lib$find_file_end( icontext(1) )
         end if

      return
      end
