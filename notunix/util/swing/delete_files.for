      subroutine delete_files( dir_spec )

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

      integer    len_spec, istat, ii, ipos
      integer    icontext, lib$delete_file, modify_file_prot, ptr
      integer    lib$find_file
      character  dir_spec*(*), spec*255
      logical    find_node, found_node

      ii = len( dir_spec )
      do while ( dir_spec(ii:ii) .eq. ' ' .and. ii .gt. 0 )
         ii = ii - 1
      end do

      if ( find_node( dir_spec(1:ii), ptr ) ) then
         found_node = .true.
         call smg$change_rendition( window2, node(ptr).line,
     .                              node(ptr).level*17+1,
     .                              1, 12,
     .                              smg$m_blink + node(ptr).rend )
         else
         found_node = .false.
         end if

      icontext = 0
      do while( lib$find_file( dir_spec(:ii)//'*.*;*', spec, icontext ))
         if ( .not. lib$delete_file( spec ) ) then
            call str$trim( spec, spec, len_spec )
            if ( modify_file_prot( spec(1:len_spec), 0, 0 ) ) then
               istat = lib$delete_file( spec )
               if ( .not. istat ) delete_problem = .true.
               else
               call lib$spawn( 'SET PROT=(S:D,O:D,G:D,W:D) '//
     .                         dir_spec(:ii)//'*.*;*', 'NL:', 'NL:' )
               istat = lib$delete_file( spec )
               if ( .not. istat ) delete_problem = .true.
               end if
            end if
      end do
      call lib$find_file_end( icontext )

      call dir_to_file( dir_spec, ii,
     .                  spec, ipos )
      if ( .not. lib$delete_file( spec ) ) then
         call str$trim( spec, spec, len_spec )
         if ( modify_file_prot( spec(1:len_spec), 0, 0 ) ) then
            istat = lib$delete_file( spec )
            if ( .not. istat ) delete_problem = .true.
            else
            call lib$spawn( 'SET PROT=(S:D,O:D,G:D,W:D) '//
     .                      dir_spec(:ii)//'*.*;*', 'NL:', 'NL:' )
            istat = lib$delete_file( dir_spec(1:ii) )
            if ( .not. istat ) delete_problem = .true.
            end if
         end if

      if ( .not. delete_problem ) then
         if ( found_node ) call delete_node( ptr )
         else
         if ( found_node )
     .      call smg$change_rendition( window2, node(ptr).line,
     .                                 node(ptr).level*17+1,
     .                                 1, 12,
     .                                 node(ptr).rend )
         end if

      return
      end
