	options	/extend_source
      subroutine help

      include    'd0$inc:swing.inc'
      include    '($hlpdef)'

      external   LIB$PUT_OUTPUT, LIB$GET_INPUT

      integer    isave, flags, input, output, stat
      integer    lbr$output_help

      call smg$save_physical_screen( board_id, isave )

	flags = hlp$m_prompt

      output = %loc( lib$put_output )
      input =  %loc( lib$get_input )

      stat = lbr$output_help( %val(output),
     .                        width,
     .                        'swing',
     .                        'd0$util:swing',
     .                        flags,
     .                        %val(input) )

      call smg$restore_physical_screen( board_id, isave )

      if ( .not. stat ) then
         call print_message(
     .        'There is no SWING.HLB help file in D0$UTIL', 0 )
         end if

      return
      end
