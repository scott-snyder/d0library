      subroutine sm_allow_repaint

      include 'd0$inc:swing.inc'

      integer  address
      external sm_repaint_screen

      address =  %loc( sm_repaint_screen )
      call smg$set_out_of_band_asts( board_id, '800000'x,
     .                               %val(address) )

      return
      end
