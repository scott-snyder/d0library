      subroutine execute_dcl

c     Craig Young               3-AUG-87

c     This subroutine passes DCL commands to a subprocess for execution then
c     reads the output from the associated mailbox.  If the subprocess is
c     nonexistant, it is created.

      include   'd0$inc:swing.inc'

      character  again, string*255
      integer*2  len_string, screen_num
      integer    key

      call smg$erase_display( DCL_window )
      call smg$label_border( DCL_window, 'DCL Command' )
      call smg$set_cursor_abs( DCL_window, 2, 1 )
      call smg$paste_virtual_display( DCL_window, board_id, 5, 5 )

      call smg$read_string( keyboard, string, '$ ',
     .                      ,,,,len_string,, DCL_window )
      call str$trim( string, string, len_string )

      call process_command( string, DCL_window, 0 )

      call smg$put_line( DCL_window, 'Press any key to continue' )
      call smg$read_keystroke( keyboard, key )

      call smg$unpaste_virtual_display( DCL_window, board_id )

      return
      end
