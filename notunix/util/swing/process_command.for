        subroutine process_command ( out_message, window, stop )

c       Craig Young             3-AUG-87

        include   'd0$inc:swing.inc'
        include   '($syssrvnam)'
        include   '($ssdef)'
        include   '($iodef)'
        include   '($clidef)'

        integer     window, stop
        integer*4   status, done, proc_id, message_len, lib$spawn
        integer*4   str$position
        character   in_message*100, out_message*100, terminator*50
        character   set_noon*8/'set noon'/
        parameter
     -  (terminator = 'swing dcl subprocess command output terminator')

        message_len = 100

        if ( proc_created .eq. 0 ) then
           status = sys$crembx ( ,outbox_channel ,,,,,'swing_dcl_inbox')
           if ( status .ne. ss$_normal ) call exit(status)
           status = sys$crembx ( ,inbox_channel ,,,,,'swing_dcl_outbox')
           if ( status .ne. ss$_normal ) call exit(status)

           status = lib$spawn ( ,'swing_dcl_inbox' ,'swing_dcl_outbox'
     -                          ,cli$m_nowait ,,proc_id
     -                          ,,done )
           if (status .ne. ss$_normal) call exit(status)
           status = sys$qiow( ,%val(outbox_channel)
     -                     ,%val(io$_writevblk) ,
     -                     ,,,%ref(set_noon)
     -                     ,%val(len(set_noon)) ,,,, )
           if ( status .ne. ss$_normal ) call exit(status)
           proc_created = 1
           end if

        status = sys$qiow( ,%val(outbox_channel)
     -                     ,%val(io$_writevblk) ,
     -                     ,,,%ref(out_message)
     -                     ,%val(message_len) ,,,, )
        if ( status .ne. ss$_normal ) call exit(status)

        if ( stop .eq. 0 ) then
           out_message = 'write sys$output "'//terminator//'"'

           status = sys$qio ( ,%val(outbox_channel)
     -                        ,%val(io$_writevblk) ,
     -                        ,,,%ref(out_message)
     -                        ,%val(message_len) ,,,, )
           if ( status .ne. ss$_normal ) call exit(status)

           in_message = '      '
           status = 0
           do while ( in_message .ne. terminator )
              if ( window .eq. DCL_window ) then
                 call smg$put_line( window, in_message )
              else
                 if ( in_message .ne. '     ' ) then
                    call print_message( in_message, 0 )
                    end if
                 end if

              in_message = '      '
              status = sys$qiow( ,%val(inbox_channel)
     -                           ,%val(io$_readvblk) ,
     -                           ,,,%ref(in_message)
     -                           ,%val(message_len) ,,,, )
              if ( status .ne. ss$_normal ) call exit(status)

              end do
           end if

        return
        end
