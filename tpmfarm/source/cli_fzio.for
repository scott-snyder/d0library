      SUBROUTINE CLI_FZIO(ibuf,ioway)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : ioway      = 0 input  (FZIN)
C-                        = 1 output (FZOUT)
C-             iquest(3)  = record type
C-                        For ioway = 0
C-                          = 0 => next continuation physical record
C-                          = 1 => start of a new logical record
C-                        For ioway = 1
C-                          = 0 => next continuation phys. record
C-                          = 1 => first phys. record of a new logical record
C-                          = 2 => last phys. record of current logical record
C-                          = 3 => only phys. record of a new logical record
C-             iquest(2)  = No. of data words
C-                        => maximum No. of words IBUF can accept (IOWAY=0)
C-                        => No. of words in IBUF to be transmitted (IOWAY=1)
C-             iquest(1)  = LUN for the I/O stream
C-   Outputs : ibuf       = data received for IOWAY = 0
C-             iquest(1)  = I/O status
C-                        = 0 => success
C-                        = -1 => error
C-                        > 0 see ZEBRA manual about FZIN/FZOUT for details
C-   Controls:
C-
C-   Created  12-AUG-1993   Kirill Denisenko
C-   Version  1.0 for use with shared memory
C-
C----------------------------------------------------------------------


      IMPLICIT NONE
      INCLUDE  'd0$inc:quest.inc'
      INCLUDE  'pfarms.inc'
      INTEGER  evin,evout,lun
      INTEGER  ltype,words,ioway,status,socket_descr
      PARAMETER ( words = 10 )
      INTEGER  ibuf(precl+1),event_request(words)
      SAVE     event_request
      INTEGER  runno, evonum
      DATA     event_request/3,9*0/
      LOGICAL  record_type,first, wrong
      INTEGER  keyaux_in, addraux_in, shmaux_in
      SAVE     shmaux_in, addraux_in, keyaux_in
      INTEGER  keyaux_out, addraux_out, shmaux_out
      SAVE     shmaux_out, addraux_out, keyaux_out
      INTEGER  sig_list(nstream)
      SAVE     sig_list
      SAVE     first,evin,evout
      DATA     first/.true./
      INTEGER  eor /.false./
      SAVE     eor

      LOGICAL  flgmain /.true./, err_ini /.false./
      INTEGER  nspec
      PARAMETER ( nspec = 20 )
      INTEGER  numword, offset, numrec
      INTEGER  numaux
      SAVE     numrec, numaux
      INTEGER  event_stat, valword
      INTEGER  shm_get, shm_get_c, shm_atch, shm_dtch, shm_del
      INTEGER  kill
      INTEGER  get_word, pause, check_pid, sleep_time
      INTEGER  atcount, gtcount
      INTEGER  ostream, stream_nr
      SAVE     stream_nr

      INTEGER  checksum, i, ieor

C----------------------------------------------------------------------
C-Initialization
      lun = iquest(1)
      iquest(1) = -1
      IF (first) THEN
        first=.false.
        evin=0
        evout=0

C attach memories for both of the keys
        gtcount = 0
124     continue
        shmid_in = shm_get(%val(key_in),
     &  %val(inrec),%val(nspec))
        if(shmid_in .eq. -1) then
          gtcount = gtcount + 1
          if(gtcount .gt. 5 ) then
          CALL errmsg('(I)Error getting shmid_in','CLI_FZIO',
     &   'Unable to get an input shared memory id','F')
          GOTO 999
          endif
          call sleep(5)
          goto 124
        endif
        addr_in = shm_atch(%val(shmid_in))
        if(addr_in .eq. -1) then
          CALL errmsg('(I)Error getting addr_in','CLI_FZIO',
     &   'Unable to get an input shared memory address','F')
          GOTO 999
        endif

C get pid of the inreader to send signals
        pid_in  = get_word(%val(addr_in) ,%val(4))

C- Check that inreader is running
        status = check_pid(%val(pid_in))
        if(status.eq.-1) then
          err_ini = .true.
          call errmsg('(I)INREADER NOT THERE','CLI_FZIO',
     &    'Exit because of no inreader','W')
        endif

        outrec(1) = outsta
        outrec(2) = outdst
        key_out(1) = key_sta
        key_out(2) = key_dst
        sig_list(1) = SIGUSR2
        sig_list(2) = SIGINT
        do 424 ostream = 1, nstream
          gtcount = 0
125       continue
          shmid_out(ostream) = shm_get(%val(key_out(ostream)),
     &    %val(outrec(ostream)),
     &    %val(nspec))
          if(shmid_out(ostream) .eq. -1) then
            gtcount = gtcount + 1
            if(gtcount .gt. 5 ) then
            err_ini = .true.
            CALL errmsg('(O)Error getting shmid_out','CLI_FZIO',
     &      'Unable to get an output shared memory id','W')
            GOTO 424
            endif
            call sleep(5)
            goto 125
          endif
          addr_out(ostream) = shm_atch(%val(shmid_out(ostream)))
          if(addr_out(ostream) .eq. -1) then
            err_ini = .true.
            CALL errmsg('(O)Error getting addr_out','CLI_FZIO',
     &      'Unable to get an output shared memory address','W')
            GOTO 424
          endif
          pid_out(ostream) = get_word(%val(addr_out(ostream)),
     &                       %val(4))
          status = check_pid(%val(pid_out(ostream)))
          if(status.eq.-1) then
            err_ini = .true.
            call errmsg('(O)OUTWRITER NOT THERE','CLI_FZIO',
     &      'Exit because of no outwriter','W')
            goto 424
          endif
424     continue

        if (err_ini) then
          iquest(2) = 0
          goto 999
        endif


      ENDIF

C-Record type
      ltype = iquest(3)
      record_type = (ltype .EQ. 0) .OR. (ltype .EQ. 1)
      record_type = record_type .OR. (ltype .EQ. 2) .OR. (ltype .EQ. 3)
      IF ( .NOT. record_type ) GOTO 999

      IF ( ioway .EQ. 1 ) GOTO 100

C############################################################
C-Input
C############################################################

C-Socket for input
      IF (ltype .EQ. 1) THEN
        
C-Zero the offset
        offset = 0

C-Zero the numrecs
        numrec = 0

C-Zero the aux key
        keyaux_in = key_in

C-Check whether event is ready for transfer
        wrong = .false.
282     continue
        status = check_pid(%val(pid_in))
        if (status.eq.-1) then
C If this is an end of data
          numrec  = get_word(%val(addr_in),%val(3))
          if(numrec .eq. -1) then
            iquest(2) = 0
            CALL errmsg('End of Data','CLI_FZIO',' ','W')
            GOTO 999
          endif
C Inreader just died by itself which is stupid of it
          do ostream = 1, nstream
            call put_word(%val(addr_out(ostream)),%val(3),%val(-1))
            status = kill(pid_out(ostream), sig_list(ostream))
          enddo
          CALL errmsg('Inreader dead','CLI_FZIO',' ','F')
          goto 999
        endif
        event_stat = get_word(%val(addr_in),%val(1))
        if ( event_stat .eq. 2 ) then
C Something is wrong with the input - consider it an end of data
          if(wrong) then
            iquest(2) = 0
            CALL errmsg('Forced End of Data','CLI_FZIO',' ','W')
            goto 999
          endif
          call sig_set(SIGUSR1)
          status = sleep_time(%val(800))
          call sig_ign(SIGUSR1)
          if(status.eq.0) wrong = .true.
          goto 282
        endif

C-Get event request
        call shm_read(%val(addr_in), event_request, %val(words), 
     &                %val(0), %val(10))

C-Get number of records ready
        numword = 3
        numrec  = get_word(%val(addr_in),%val(numword))
        if(numrec .eq. -1) then
          iquest(2) = 0
          CALL errmsg('End of Data','CLI_FZIO',' ','W')
          GOTO 999
        endif

        numaux = 0
        if ( numrec .gt. inrec ) then
          numaux = numrec
          numrec = inrec
        endif

        if(event_request(1) .ne. 4 ) evin = evin+1

C-Block myself from reading the same event twice
        numword = 1
        valword = 2
        call put_word(%val(addr_in),%val(numword),%val(valword))


C-Reading from the main memory
        flgmain = .true.

      ENDIF

C-New buffer of the same event
      IF  (numrec .eq. 0 .and. numaux .gt. 0 ) then

        numaux = numaux - inrec
        if ( numaux .gt. inrec ) then
          numrec = inrec
        else
          numrec = numaux
          numaux = 0
        endif
        
C-Zero the offset
        offset = 0

C-Get new shmid for the extra segment
        keyaux_in = keyaux_in + 1
        shmaux_in = shm_get(%val(keyaux_in),
     &  %val(numrec), %val(0))
        if(shmaux_in .eq. -1) then
          CALL errmsg('(I)Error getting shmaux_in','CLI_FZIO',
     &    'Unable to get an inaux shared memory id','F')
          GOTO 999
        endif
        addraux_in = shm_atch(%val(shmaux_in))
        if(addraux_in .eq. -1) then
          CALL errmsg('(I)Error getting addraux_in','CLI_FZIO',
     &    'Unable to get an inaux shared memory address','F')
          GOTO 999
        endif

C-Reading from the aux memory
        flgmain = .false.

      ENDIF

C-Read input record
      if(flgmain) then
        call shm_read(%val(addr_in), ibuf, %val(precl), 
     &                %val(offset), %val(nspec))
      else
        call shm_read(%val(addraux_in), ibuf, %val(precl),
     &                %val(offset), %val(0))
      endif
        

C-Decrement numrec and increment offset
      offset = offset + 1
      numrec = numrec - 1

C-If the last record read in from this buffer
      if ( numrec .eq. 0 ) then

C Remove the aux shared memory
        if ( event_stat .eq. 4 .and. .not.flgmain ) then
          status = shm_dtch(%val(addraux_in))
          status = shm_del(%val(shmaux_in))
        endif

C-Read the last record of the whole event from the shared buffer
        if ( numaux .eq. 0 ) then
          
C-Change the first word for myself
          numword = 1
          call put_word(%val(addr_in),%val(numword),%val(2))

C-Change the second word for inreader
          numword = 2
          call put_word(%val(addr_in),%val(numword),%val(1))

C-Send a signal to inreader
          status = kill(pid_in, SIGUSR1)
          if(status.ne.0) then
            CALL errmsg('(I)Send signal error','CLI_FZIO',
     &      'Unable to send a signal to inreader','W')
            goto 999
          endif
        endif

      endif

C-Success
      iquest(1) = 0
      iquest(2) = precl

      GOTO 999

C############################################################
C-Output
C############################################################

  100 CONTINUE


C-Start a new dataset
      IF ((ltype .EQ. 1) .OR. (ltype .EQ. 3)) THEN

C-If eor
        if(event_request(1).eq.7) goto 998
        if(lun.eq.lunout(1)) then
          if((evout.ge.evin.and.ltype.eq.3)) then
            eor = .true.
          endif
        endif

C-Skip it if eof reached and termination already sent
        if(eor) goto 998

C-Reset the record counter and the offsets
        offset = 0

C-DST or STA
        IF (lun.eq.lunout(2)) THEN
          event_request(1) = 11
          stream_nr = 2
        ELSE
          event_request(1) = 3
          stream_nr = 1
        ENDIF

C-Set the RECO event number
        event_request(6) = EVONUM()
C-If MC data has zero event_number - set it to the serial count of
C-events served
        if ( event_request(6) .eq. 0 ) event_request(6) = evout + 1

C-Set the RECO run number
        event_request(7) = RUNNO()

C-Event request
        wrong = .false.
283     continue
        status = check_pid(%val(pid_out(stream_nr)))
        if (status.ne.0) then
          call put_word(%val(addr_in),%val(3),%val(-1))
          status = kill(pid_in, SIGUSR1)
          call cli_fin
          CALL errmsg('Outwriter dead','CLI_FZIO',
     &    'Not there','F')
          goto 999
        endif
        event_stat = get_word(%val(addr_out(stream_nr)),%val(1))
        if ( event_stat .eq. 2 ) then
          if(wrong) then
            call cli_fin
            CALL errmsg('Outwriter dead','CLI_FZIO',
     &      'Not responding','F')
            goto 999
          endif
          call sig_set(sig_list(stream_nr))
          status = sleep_time(%val(500))
          call sig_ign(sig_list(stream_nr))
          if ( status .eq. 0 ) wrong = .true.
          goto 283
        endif

C-Store the event_request in words 11-20
        call shm_write(%val(addr_out(stream_nr)), 
     &                event_request, %val(words), 
     &                %val(0), %val(10))

        if(lun.eq.lunout(1)) evout = evout + 1

        keyaux_out = key_out(stream_nr)

C-Number of records to store
        numrec   = ibuf(8) + 1
        numaux   = 0

C-Put total number of records into the 3rd word
        numword = 3
        call put_word(%val(addr_out(stream_nr)),%val(numword),
     &                %val(numrec))

        if(numrec.gt.outrec(stream_nr)) then
          numaux = numrec
          numrec = outrec(stream_nr)
        endif

C-Reading from the main memory
        flgmain = .true.

      ENDIF

C-If numrec = 0 ( next buffer is filled )
      IF ( numrec .eq. 0 .and. numaux .gt. 0 ) then

        numaux = numaux - outrec(stream_nr)
        if ( numaux .gt. outrec(stream_nr) ) then
          numrec = outrec(stream_nr)
        else
          numrec = numaux
          numaux = 0
        endif

        keyaux_out = keyaux_out + 1
        gtcount = 0
172     continue
        gtcount = gtcount + 1
        shmaux_out = shm_get_c(%val(keyaux_out),
     &  %val(numrec),%val(0))
        if(shmaux_out .eq. -1) then
C Try to check whether it exists; if yes - delete it
          if(gtcount.le.3 ) then
            shmaux_out = shm_get(%val(keyaux_out),
     &      %val(0),%val(0))
            if ( shmaux_out .ne. -1 ) then
              status = shm_del ( %val(shmaux_out) )
              goto 172
            endif
          endif
          write(6,*) 'Numrec=',numrec, ' key_aux =',keyaux_out
          CALL errmsg('(O)Error getting shmaux_out','CLI_FZIO',
     &    'Unable to get an outaux shared memory id','F')
          GOTO 999
        endif
        atcount = 0
173     continue
        atcount = atcount + 1
        addraux_out = shm_atch(%val(shmaux_out))
        if(addraux_out .eq. -1) then
           write(6,*) 'Numrec=',numrec, ' shmaux_out=',shmaux_out
           write(6,*) 'keyaux_out=',keyaux_out
C Try again ( three times )
           if ( atcount.le.3 ) then
             call sleep(10)
             goto 173
           endif
          CALL errmsg('(O)Error getting addraux_out','CLI_FZIO',
     &    'Unable to get an outaux shared memory address','F')
          GOTO 999
        endif

C-Zero the offset
        offset = 0

C-Reading from auxiliary buffer
        flgmain = .false.

      ENDIF

C-Calculate the checksum for the RECO Buffer
      checksum = 0
      do i = 1, precl
        checksum = ieor (checksum, ibuf(i))
      enddo
      
C-Set the equivalen array first element to be the checksum
      ibuf(precl+1) = checksum

C-Store the next record in shmem
      if (flgmain) then
        call shm_write(%val(addr_out(stream_nr)), ibuf, 
     &                 %val(precl+1), 
     &                 %val(offset), %val(nspec))
      else
        call shm_write(%val(addraux_out), ibuf, %val(precl+1), 
     &                 %val(offset), %val(0))
      endif

C-Increment number of records (offset)
      offset = offset + 1
      numrec = numrec - 1

      if(numrec.eq.0.and..not.flgmain) then
        status = shm_dtch(%val(addraux_out))
	if(status.eq.-1) write(6,*) '(O) Detach failed'
      endif

C-In case of last record
      IF (ltype .EQ. 2 .or. ltype .EQ. 3 ) then

C-Set a flag for myself that outmem is busy 
        numword = 1
        call put_word(%val(addr_out(stream_nr)),%val(numword),
     &                %val(2))

C-Set a flag for the outwriter that outmem is ready
        numword = 2
        call put_word(%val(addr_out(stream_nr)),%val(numword),
     &                %val(1))

C-Send a SIGUSR2 to outwriter that it can start sending event out
        status = kill(pid_out(stream_nr), sig_list(stream_nr))
        if(status.ne.0) then
          CALL errmsg('(O)Send signal error','CLI_FZIO',
     &    'Unable to send a signal to outwriter','W')
          goto 999
        endif

      ENDIF


C-Success
  998 iquest(1)=0

  999 RETURN
      END
