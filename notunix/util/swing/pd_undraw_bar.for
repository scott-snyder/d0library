      subroutine pd_undraw_bar( board_id )

*     PD_UNDRAW_BAR( BOARD_ID )
*
*     BOARD_ID          INTEGER*4
*
      implicit none
      include 'd0$inc:pulldown.inc'

      integer board_id

      call smg$unpaste_virtual_display( pd_bar_id, board_id )

      return
      end
