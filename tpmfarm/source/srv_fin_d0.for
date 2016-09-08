        SUBROUTINE SRV_FIN
C
C       Server component of the farm server using TCP sockets for
C       interprocessor communication. Initializes the server-client
C       communications. There is 1 server per each RECO node and one
C       client that receives the data and writes it into the local
C       file.
C
C  kirill denisenko, 1992
C

        implicit  none

        include 'd0$inc:farms.inc'

C--     Status returned from socket operations
        integer*4  status

C--     Number of longwords of data to be transmitted
        integer*4  data_length

C--     Write, read and exception masks for select operation
        integer*4  write_mask, read_mask, exc_mask

C--     Number of ready descriptors found by select
        integer*4  n_found

C--     Socket descriptor for client being processed
        integer*4  s_descr


C++     BUFFERS FOR INTER-PROCESSOR COMMUNICATION

C--     Buffer to receive header packet requests
        integer*4  header_request (10)

C--     Buffer to build and transmit header data packets
        integer*4  header_data (10)



C++     MISCELLANEOUS VARIABLES
C--     Starting and elapsed time recorders
        real*4     start_time, elapsed_time

C--     Character strings to hold time and date
        character*8 time_string
        character*9 date_string

C--     Events served per second
        real*4  recs_per_sec


C++     DECLARE FUNCTION TYPES
        integer*4  read_socket, write_socket
        integer*4  select_socket, shutdown_socket, close_socket


C--     E N D   O F   D A T A  --  T E R M I N A T I O N   P R O C E S S I N G

        write (6,856)
 856    format (//'TERMINATING CLIENT PROCESS - -'/)

C--     Socket descriptor for this client
        s_descr = descr_by_cli

C--     Wait for this client to transmit a header request packet
        write_mask = 0
        exc_mask = 0
        read_mask = mask_by_cli
        n_found = select_socket (select_bits, read_mask, write_mask,
     +  exc_mask, 20)

C--     If no packet arrives in 20 seconds, pronounce the client dead
        if (n_found .eq. 0)  then
          write (6,857) 
 857      format (/'SELECT_SOCKET, FINAL HEADER REQUEST: client ',
     +          'pronounced dead after 20 seconds')

C--       If there is a packet ready to read . . .
        else

C--       Read the packet from the chosen client
          status = read_socket (s_descr, header_request, 10)
          if (status .ne. 10)  then
            write (6,860) status, s_descr
 860        format (/'READ_SOCKET, FINAL HEADER REQUEST - -'/
     +            '  status returned . . . . . ', i6/
     +            '  socket descriptor . . . . ', i6)
          end if


C--       Send a header to the client with the data length = 0
          header_data (1) = 0
          status = write_socket (s_descr, header_data, 10)
          if (status .ne. 10)  then
            write (6,863) status, s_descr
 863        format (/'WRITE_SOCKET, FINAL HEADER DATA - -'/
     +         '  status returned . . . . . ', i6/
     +         '  socket descriptor . . . . ', i6)
          end if


          write (6,866)  s_descr
 866      format ('TERMINATION REQUEST sent to client ', 
     +      ', descriptor ', i2)

          write (6,869)
 869      format (/'WAITING 20 SECONDS for client to terminate')

        endif

C--     Wait for the clients to terminate
        call sleep (20)

        write (6,872)
 872    format (/'CLOSING DOWN CONNECTIONS AND SOCKETS TO CLIENTS')

C--     Shutdown this client's connection
        status = shutdown_socket (descr_by_cli)
        write (6,875) status
 875    format (/'SHUTDOWN_SOCKET , status ', i5)

C--     Close this client's socket
        status = close_socket (descr_by_cli )
        write (6,878) status
 878    format ('CLOSE_SOCKET',
     +       ', status ', i5)



C--     Write a final report, records per client
        write (6,880)
 880    format (//'FINAL RECORD COUNT - -'/)

        write (6,885)  tot_recs
 885    format (i7, ' records')



C--     For the founding socket, just close it
        status = close_socket (socket_descr)
        write (6,890) status
 890    format (/'CLOSE_SOCKET: founding socket, status ', i5)



        return

        end
