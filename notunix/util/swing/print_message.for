      subroutine print_message( message, abort )

      include    'd0$inc:swing.inc'

      logical    abort, erased
      character  message*(*)

      if ( using_screen ) then

         if ( message .eq. ' ' ) then
            if ( .not. erased ) then
               erased = .true.
               call smg$erase_display( window3 )
               call smg$erase_line( window3, 2, 1 )
               end if

            else
            erased = .false.
            call smg$erase_display( window3 )
            call smg$put_chars( window3, message, 2, 1, 1 )
            end if

         if ( abort ) call exit_swing

         else
         print *, 'SWING: ', message
         if ( abort ) stop ' '
         end if

      return
      end
