      subroutine define_paste_board

      include    'd0$inc:swing.inc'
      include    '($smgdef)'

C     DEC FORGOT THIS PARAMETER IN $SMGDEF
      parameter  SMG$S_PASTEBOARD_INFO_BLOCK = '20'x

      integer    smg$create_pasteboard
      integer    smg$create_virtual_keyboard
      integer    smg$set_keypad_mode
      integer    smg$get_pasteboard_attributes
      integer    istat

C
C	Q and D fix to cope with strange terminal screen lengths
C	Added by EH Perkins, Aug 16, 1987
C
	integer linest
	common /length/ linest

      record     /smgdef/ table

      call set_notab( 'SYS$COMMAND', set_term_buf )

	istat = smg$create_pasteboard( board_id,,linest)  ! linest added
C                                                          by EHP

      istat = smg$get_pasteboard_attributes(board_id, %ref(table),
     .                                %ref(SMG$S_PASTEBOARD_INFO_BLOCK))

      width = table.smg$w_width

      istat = smg$create_virtual_keyboard( keyboard )
      istat = smg$set_keypad_mode( keyboard, 1 )

      call sm_allow_repaint

      return
      end
