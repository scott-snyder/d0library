C----------------------------------------------------------------------
C-
C-   Created   13-Aug-1993 Kirill Denisenko
C-   Version  1.0 for use with shared memory
C-
C----------------------------------------------------------------------
      integer nstream
      parameter ( nstream = 2 )
      COMMON /PFARMS/ pid_in, pid_out, shmid_out, shmid_in,
     +                addr_out, addr_in, lunout, key_out,
     +                outrec
      INTEGER pid_in, pid_out(nstream), addr_out(nstream), addr_in
      INTEGER shmid_in, shmid_out(nstream)
      integer lunout(10)
      integer precl
      parameter ( precl  =  8190 )
      integer key_in, key_out(nstream)
      parameter ( key_in  = 111 )
      integer key_sta, key_dst
      parameter ( key_sta = 222 )
      parameter ( key_dst = 322 )
      integer inrec, outrec(nstream)
      parameter ( inrec  = 20 )
      integer outdst, outsta
      parameter ( outdst =  2 )
      parameter ( outsta = 25 )
C&IF SIUNIX
C&      integer SIGINT /2/, SIGUSR1 /16/, SIGUSR2 /17/
C&ELSEIF IBMAIX
C&      integer SIGINT /2/, SIGUSR1 /30/, SIGUSR2 /31/
C&ELSEIF LINUX
      integer SIGINT /2/, SIGUSR1 /10/, SIGUSR2 /12/
C&ENDIF
