      subroutine exit_swing

      include    'd0$inc:swing.inc'

      character  string*3
      integer    len_string

      if ( proc_created ) then
         call process_command( 'stop/id=0', window3, 1 )
         call sys$dassgn( %val(inbox_channel) )
         call sys$dassgn( %val(outbox_channel) )
      end if

      if ( do_save .and. swing_file_exists ) then
         call record_structure( .false. )
         end if

      call smg$delete_pasteboard( board_id, 1 )

      call smg$change_pbd_characteristics( board_id, 80,, 24 )

      call reset_terminal( 'SYS$COMMAND', set_term_buf )

      stop ' '
      end
