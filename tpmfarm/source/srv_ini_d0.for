	SUBROUTINE SRV_INI
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
 
C--     Descriptor from socket clone connected to a client
        integer*4  new_descr
 
C--	Status returned from socket operations
	integer*4  status
 
C--     Index of the string position
        integer*4 i
 
C++	MISCELLANEOUS VARIABLES
 
C--	Command to tell client to execute the startup file
        character*20  cli_node
        character*20  srv_node
	character*130 rsh_com
        character*80  cli_area
        character*80  prodir
        character*80  prodat
        character*130 output_name, filter_name
        character*20  queid
 
C--	Character strings to hold time and date
	character*8 time_string
	character*9 date_string
 
 
C++	DECLARE FUNCTION TYPES
	integer*4  make_socket, bind_socket, listen_socket
	integer*4  accept_socket
	integer*4  select_socket
	integer*4  makex, system
        integer*4  trulen
 
 
C--	I N I T I A L I Z A T I O N
 
 
C--     G E T  E N V I R O N M E N T  V A R I A B L E S
        call getenv('STAFILE',    output_name)
        call getenv('FERMICSHRC', srv_node   )
        call getenv('CLI_AREA',   cli_area   )
        call getenv('PRODIR',     prodir     )
        call getenv('PRODAT',     prodat     )
        call getenv('QUEID',      queid      )
 
C--	Zero the select bitmap and bit count
	select_mask = 0
	select_bits = 0
 
C--     Zero the number of records transferred
        tot_recs = 0
 
C--     Get the port number from the RCP file
        call inrcp('REMOTE_RCP', status)
        if ( status .ne. 0 ) then
          write(6,*) 'Unable to open REMOTE_RCP'
          stop
        endif
        call ezpick('REMOTE_RCP')
        call ezget ('SERVER_PORT',server_port,status)
        if ( status .ne. 0 ) then
          write(6,*) 'Unable to get SERVER_PORT value'
          stop
        endif
        call ezgets ('SERVER_NODE', 1, cli_node, i, status)
        if ( status .ne. 0 ) then
          write(6,*) 'Unable to get SERVER_NODE value'
          stop
        endif
 
        call ezrset
 
C--     Make filter file name fro sta name by swapping NONE to WZ01
        i = index(output_name,'NONE')
        if(i.eq.0) then
          write(6,*) 'WRONG STA NAME GIVEN'
          stop
        endif
        filter_name = output_name
        filter_name(i:i+3) = 'CL04'
        filter_name = '$PRODAT/proman/dst/'//
     +  filter_name(1:trulen(filter_name))
 
C--	Create a socket for event serving
	socket_descr = make_socket ()
	write (6,730) socket_descr
 730	format (/' MAKE_SOCKET:   socket descriptor / status ', i5)
	if (socket_descr .le. 0)  stop
 
C--	Bind a system-chosen local address and the specified port number
C--	to the socket; report the results
	status = bind_socket (socket_descr, server_port)
	write (6,733) status
 733	format (' BIND_SOCKET:   status returned ', i5)
	if (status .lt. 0)  stop
 
C--	Listen for connection requests; allow a backlog of 1 request only
	status = listen_socket (socket_descr, 1)
	write (6,736) status
 736	format (' LISTEN_SOCKET: status returned ', i5)
	if (status .lt. 0)  stop
 
 
C--	Report client-starting
	write (6,739)
 739	format (/' ABOUT TO START CLIENTS ON FARM ')
 
 
C--	C L I E N T   S T A R T I N G
 
C--	Open the startup file to be executed by the client
	open (unit=11,
     +  file=cli_area(1:trulen(cli_area))//'/client_start.'//
     +  srv_node,
     +  status='unknown')
 
C--	Compose the script to be executed in the client
	write (11,745) prodir(1:trulen(prodir)),
     +                 prodat(1:trulen(prodat)),
     +                 output_name(1:trulen(output_name)),
     +                 filter_name(1:trulen(filter_name)),
     +                 srv_node(1:trulen(srv_node)),
     +                 cli_area(1:trulen(cli_area)),
     +                 queid(1:trulen(queid)),
     +                 srv_node(1:trulen(srv_node))
 745	format ('#!/bin/csh -f'                  /
     +          'setenv PRODIR ',  a,            /
     +          'setenv PRODAT ',  a,            /
     +          'setenv STA_DATA ',a,            /
     +          'setenv STA_FILTER ',a,          /
     +          'setenv STA_NODE ',a,            /
     +          'setenv CLI_AREA ',a,            /
     +          'setenv QUEID ',   a,            /
     +	        'cd $PRODAT/',     a, '/run',    /
     +	        'nohup $PRODIR/exe/cli_d0.x &'   )
 
C--	Close the startup file
	close (unit=11)
 
C--	Make the startup file world-executable
	status = makex (cli_area(1:trulen(cli_area))
     +  //'/client_start.'//
     +  srv_node)
 
C--	Prepare a command telling the client to execute the startup file
	write (rsh_com,748) cli_node(1:trulen(cli_node)),
     +  cli_area(1:trulen(cli_area)), srv_node(1:trulen(srv_node))
 748	format ('nohup rsh ',a,' ',a,
     +  '/client_start.',a,' &')
 
C--	Send the startup command to the client, and wait for it to complete
        status = system (rsh_com)
        write(6,*) 'System call to submit client job
     +status=',status
 
C--	Waiting for socket connection from the client
	write (6,754)
 754	format (' Waiting for TCP connection from client . . .')
 
C--	Accept a connection from the client; produce a new socket descriptor
	new_descr = accept_socket (socket_descr)
	write (6,757) new_descr
 757	format (' ACCEPT SOCKET: new descriptor / status ', i5)
	if (new_descr .lt. 0)  stop
 
 
C--	Record the new descriptor
	descr_by_cli  = new_descr
        mask_by_cli   = ishft(1, new_descr)
        select_mask   = mask_by_cli
        select_bits   = max ( select_bits, new_descr + 1 )
 
C--	Report successful starting of this client
	write (6,760)  descr_by_cli
 760	format (/' Started ',
     +	  'socket descriptor ', i2 )
 
        return
        end
