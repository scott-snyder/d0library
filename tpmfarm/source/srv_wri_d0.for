	SUBROUTINE SRV_WRI(INBUF,BYTES_PER_RECORD,IERR)
C
C	Server component of the farm server using TCP sockets for
C	interprocessor communication. Initializes the server-client
C       communications. There is 1 server per each RECO node and one
C       client that receives the data and writes it into the local
C       file.
C
C
C	Files:
C	   5  standard in     operating parameters
C	   6  standard out    progress reports, error messages
C	  11  client.start    client startup command files
C	  12  server.address  TCP address and port number of server
C
C	File server.address must be valid when the
C	program is started.

C kirill denisenko, 1992

	implicit  none

        include 'd0$inc:farms.inc'

C--     Status returned from the subroutine
        integer*4 ierr

C--	Status returned from socket operations
	integer*4  status

C--	Number of longwords of data to be transmitted
	integer*4  data_length

C--	Write, read and exception masks for select operation
	integer*4  write_mask, read_mask, exc_mask

C--	Number of ready descriptors found by select
	integer*4  n_found

C--	Socket descriptor for client being processed
	integer*4  s_descr

C--	Number of longwords of data to transmit to client
	integer*4  longs_to_write




C++	BUFFERS FOR INTER-PROCESSOR COMMUNICATION


C--	Buffer to receive header packet requests
	integer*4  header_request (10)

C--	Buffer to build and transmit header data packets
	integer*4  header_data (10)


C--	Buffer to receive event packet requests
	integer*4  event_request (10)

C--     Buffer to hold a data
        integer*4 inbuf(1)
        integer*4 bytes_per_record


C--	Character strings to hold time and date
	character*8 time_string
	character*9 date_string


C++	DECLARE FUNCTION TYPES
	integer*4  read_socket, write_socket
	integer*4  select_socket


C--	D A T A   S E R V I N G 

C--     Set error flag to zero
        ierr = 0

C--	Wait for a client to be ready to receive data
 110	write_mask = 0
	exc_mask = 0
	read_mask = select_mask
	n_found = select_socket (select_bits, read_mask, write_mask,
     +	exc_mask, 5)
	if (n_found .eq. 0) then
	  write (6,827)
 827	  format ('SELECT_SOCKET, HEADER REQUEST: 5-second timeout')
	  go to 110
        end if

C--	We've got a client number; get its socket descriptor
 120	s_descr = descr_by_cli 


C--	Read the header-request packet from the chosen client
	status = read_socket (s_descr, header_request, 10)

C--	If there is a problem, report it and try another client
	if (status .ne. 10) then
	  write (6,833) status, s_descr
 833	  format (/'READ_SOCKET, HEADER REQUEST: CLIENT FAILED'/
     +	       '  status returned . . . . . ', i6/
     +	       '  socket descriptor . . . . ', i6)
          ierr = -1
          goto 100
	end if

C--	Send the data longword count to the  client
	longs_to_write = bytes_per_record / 4
	header_data (1) = longs_to_write
	status = write_socket (s_descr, header_data, 10)

C--	If there is a problem, report it and try another client
	if (status .ne .10)  then
	  write (6,836) status, s_descr
 836	  format (/'WRITE_SOCKET, HEADER DATA: CLIENT FAILED'/
     +	             '  status returned . . . . . ', i6/
     +	             '  socket descriptor . . . . ', i6)
          ierr = -1
          goto 100
	end if

C--     If the size of the transmission is zero - exit
        if ( longs_to_write .eq. 0 ) goto 100

C--	Read the data-request packet from the client
	status = read_socket (s_descr, event_request, 10)

C--	If there is a problem, report it and stop
	if (status .ne. 10) then
	  write (6,839) status, s_descr
 839	  format (/'READ_SOCKET, EVENT REQUEST: CLIENT FAILED'/
     +	       '  status returned . . . . . ', i6/
     +	       '  socket descriptor . . . . ', i6)
          ierr = -1
          goto 100
	end if
	   

C--	Send the data to the client
	status = write_socket (s_descr, inbuf,
     +	longs_to_write)

C--	If there is a problem, report it and stop
	if (status .ne. longs_to_write) then
 
	  write (6,842) status, s_descr
 842	  format (/'WRITE_SOCKET, EVENT DATA: CLIENT FAILED'/
     +	       '  status returned . . . . . ', i6/
     +	       '  socket descriptor . . . . ', i6)
          ierr = -1
          goto 100
        end if


C--	Increment the record counter
	tot_recs = tot_recs + 1

100     continue

        return
        end
