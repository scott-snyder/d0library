      subroutine load_files

c     Craig Young               3-AUG-87

c     This subroutine stores the file names in the current directory into
c     the FNode array for use by the filer.

      include    'd0$inc:swing.inc'

      integer    ii
      integer*4  icontext, lib$find_file
      character  spec*255, search*255

      do ii = 1, MAX_FILES      !Initialize fnode array
         fnode(ii).length = 0
         fnode(ii).spec = ' '
         fnode(ii).name = ' '
      end do

      num_files = 0             !Initialize num_files

      search = node(node_num).spec(1:node(node_num).length)//'*.*;*'

      icontext = 0
      do while ( lib$find_file( search, spec, icontext ) .and.
     .           num_files .lt. MAX_FILES )
C
C         Modified to exclude directories ....... EHP  7/sep/87
C
         ii = index( spec, '.DIR')
         if (ii .eq. 0) call append_fnode( spec )
      end do
      call lib$find_file_end( icontext )

      if ( num_files .eq. MAX_FILES )
     .   call print_message ( 'Too many files; not all displayed', 0 )

      file_num = 1              !Initialize cursor, window range
      top_file_line = 1
      bottom_file_line = 12

      return
      end
