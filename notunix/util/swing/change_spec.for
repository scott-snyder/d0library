      subroutine change_spec( parent, ptr )

      include    'd0$inc:swing.inc'

      character  spec*255
      integer    len, parent, ptr, jj, ii

      jj = node(ptr).length - 1
      ii = jj
      do while ( ii .gt. 1 .and.
     .           node(ptr).spec(ii:ii) .ne. '[' .and.
     .           node(ptr).spec(ii:ii) .ne. '.' )
         ii = ii - 1
      end do
      ii = ii + 1

      spec = node(parent).spec(1:node(parent).length)//
     .       node(ptr).spec(ii:jj)//'.DIR;1'

      call file_to_dir( spec,
     .                  node(ptr).spec,
     .                  node(ptr).length,
     .                  node(ptr).name )

      return
      end
