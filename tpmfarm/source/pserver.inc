C ********************************************
C *                                          *
C * Kirill Denisenko, 7/9/93                 *
C *                                          *
C * Include file for parallel servers        *
C *                                          *
C ********************************************

      integer*4 blk_factor_in
      parameter ( blk_factor_in = 20 )

      integer*4 blk_factor_out
      parameter ( blk_factor_out = 25 )

      integer*4 reclen
      parameter ( reclen = 8190 )

      integer*4 max_cli
      parameter ( max_cli = 1000 )

      integer*4 select_time
      parameter (select_time = 20 )

C-- Client common
      integer*4 cli_dscr, select_bits, runit 
      character*20 hostname_by_cli(max_cli)
      integer*4 avail_mask
      COMMON /CLIENT/ cli_dscr, select_bits, runit, avail_mask,
     +                hostname_by_cli
