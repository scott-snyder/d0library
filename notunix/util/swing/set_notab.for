      subroutine set_notab( terminal, save_buffer )
      implicit none

C     ROUTINE USES THE IO$_SETMODE FUNCTION TO SET TERMINAL
C     CHARACTERISTICS

      include    '($iodef)'
      include    '($ttdef)'
      include    '($tt2def)'

c                LAYOUT OF char_buffer
c
c     --------------------------------------------
c     |    buffer size     |   type   |   class  |  <- longword
c     |page len  |   terminal characteristics    |  <- longword (TTDEF)
c     |    extended terminal characteristics     |  <- longword (TT2DEF)
c     --------------------------------------------
c     31                                         0

      integer*2  iosb(4)
      integer*4  status, sys$trnlog, sys$assign, sys$qiow, chan
      integer*4  char_buffer(3), save_buffer(3)
      character  terminal*(*)

      status = sys$assign( terminal, chan,, )

      status = sys$qiow ( %val(1),
     .                  %val(chan),
     .                  %val(io$_sensemode),
     .                  iosb,,,
     .                  %Ref(save_buffer),
     .                  %val(12),,,, )

      char_buffer(1) = save_buffer(1)
      char_buffer(2) = jibclr( save_buffer(2), tt$v_mechtab )
      char_buffer(3) = save_buffer(3)

      status = sys$qiow ( %val(1),
     .                  %val(chan),
     .                  %val(io$_setmode),
     .                  iosb,,,
     .                  %Ref(char_buffer),
     .                  %val(12),,,, )

      return
      end
