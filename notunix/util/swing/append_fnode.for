      subroutine append_fnode( spec )

c     Craig Young               3-AUG-87

c     This subroutine adds a file name to the FNode array, truncating, if
c     necessary, the file spec into a 20-character name.

      include    'd0$inc:swing.inc'

      integer    len_node, ii
      character  specout*255, spec*255

      num_files = num_files + 1
      file_num = num_files
      ii = 1
      do while ( spec(ii:ii) .ne. ']' .and. spec(ii:ii) .ne. '>' )
         ii = ii + 1
         end do

      call str$trim( specout, spec, len_node )
      fnode(file_num).spec = specout
      fnode(file_num).length = len_node - ii

      if ( fnode(file_num).length .le. 23 ) then
         fnode(file_num).name = specout(ii+1:)
         else
         fnode(file_num).name = specout(ii+1:ii+22)//'*'
         end if

      return
      end
