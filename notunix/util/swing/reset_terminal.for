      subroutine reset_terminal( terminal, char_buffer )
      implicit none

C     ROUTINE USES THE IO$_SETMODE FUNCTION TO SET TERMINAL
C     CHARACTERISTICS

      include    '($iodef)'

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
      integer*4  reset, char_buffer(3)
      character  terminal*(*)

      status = sys$assign( terminal, chan,, )

      status = sys$qiow ( %val(1),
     .                  %val(chan),
     .                  %val(io$_setmode),
     .                  iosb,,,
     .                  %ref(char_buffer),
     .                  %val(12),,,, )

      return
      end
