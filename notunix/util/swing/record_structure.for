	options	/extend_source
      subroutine record_structure( search )

      include    'd0$inc:swing.inc'

      character  spec*255
      logical    search, modify_file_prot
      integer    icontext, ii, jj, istat, len_spec
      integer    lib$find_file, lib$delete_file

      if ( search .and. swing_file_exists ) then
         do ii = 1, num_nodes
            node(ii).length = 0
            node(ii).child = 0
            node(ii).sister = 0
         end do

         call load_nodes

         call load_display

         call update_screen( cur_line, cur_level )
         end if

      do_save = .false.

      call print_message( 'Saving directory structure', 0 )

      icontext = 0
      do while( lib$find_file( main(1:len_main)//'swing.sav;*',
     .                         spec, icontext ))
         if ( .not. lib$delete_file( spec ) ) then
            call str$trim( spec, spec, len_spec )
            if ( modify_file_prot( spec(1:len_spec), 0, 0 ) ) then
               istat = lib$delete_file( spec )
               else
               call lib$spawn( 'SET PROT=(S:D,O:D,G:D,W:D) '//
     .                         main(1:len_main)//'swing.sav',
     .                         'NL:', 'NL:' )
               istat = lib$delete_file( spec )
               end if
            end if
      end do
      call lib$find_file_end( icontext )

      open( unit=2,
     .      file=main(1:len_main)//'swing.sav',
     .      status='new',
     .      carriagecontrol='list',
     .      access='sequential',
     .      form='unformatted',
     .      recl=73,
     .      organization='sequential',
     .      recordtype='variable',
     .      iostat=istat,
     .      err=99 )

      write( 2 ) num_lines, num_nodes, lowest_level

      do ii = 1, num_lines
         write( 2 ) (node_pointer(jj,ii), jj=0, MAX_LEVELS)
      end do

      do ii = 1, num_nodes
         write( 2 ) node(ii)
      end do

      close( unit=2 )

      swing_file_exists = .true.

      call print_message( 'Finished saving directory structure', 0 )

      return

99    call print_message( 'Unable to record directory structure', 0 )
      return
      end
