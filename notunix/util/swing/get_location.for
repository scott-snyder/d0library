      subroutine get_location( disk, len_disk, root, len_root )

      implicit none
      integer*2  len_root
      integer*4  sys$setddir, len_disk, istat
      character  root*255, disk*31

      call lib$sys_trnlog( 'SYS$DISK', len_disk, disk )
      istat = sys$setddir( 0, len_root, root )

      root = root(1:len_root)

      return
      end
