      SUBROUTINE CLI_FIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Termination Routine for use with shmem
C-
C-   Inputs  : NONE
C-
C-   Outputs : NONE
C-   Controls:
C-
C-   Created   20-Aug-1993   Kirill Denisenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'pfarms.inc'
      INTEGER  status,words
      INTEGER  get_word, check_pid, kill, pause
      INTEGER  shm_del, sleep_time
      INTEGER  count, ostream
      INTEGER  sig_list(2)
      logical  first / .true. /
      save first
C----------------------------------------------------------------------

      if(first) then
        first = .false.


C-Terminate input server
	status = check_pid (%val(pid_in))
        if ( status .ne. 0 ) goto 283

	call put_word(%val(addr_in),%val(3),%val(-1))
	status = kill(pid_in, SIGUSR1)

C-Give enough time to die
	call sleep(2)

C-Remove the input shared memory
        status = shm_del(%val(shmid_in))

C-Send a terminating request to the output server
        count = 0

283     continue

C-Check whether server is still there
        sig_list(1) = 17
        sig_list(2) = 2
        do ostream = 1, nstream
          status = check_pid (%val(pid_out(ostream))) 
          if ( status .ne. 0 ) goto 888

          status = get_word(%val(addr_out(ostream)),%val(1))
          if ( status .eq. 2 ) then
            call sig_set(sig_list(ostream))
            status = sleep_time(%val(20))
            call sig_ign(sig_list(ostream))
            count = count + 1
            if(count.gt.3) goto 888
            goto 283
          endif

C-Set a flag for the outwriter that outmem is ready
        call put_word(%val(addr_out(ostream)),%val(2),
     &                %val(1))

C-Put a termination value into the buffer[11]
          call put_word(%val(addr_out(ostream)),%val(11),%val(9))
	  status = kill(pid_out(ostream), sig_list(ostream))
888       continue
        enddo

      ENDIF

      
  999 RETURN
      END
