C       COMMON BLOCK TO HOLD THE IPC VARIABLES
C

C++	PARAMETERS DEFINING MAXIMUM PROGRAM CONFIGURATION


C++	TABLES INDEXED BY CLIENT NUMBER

C--	Socket descriptor attached to client
	integer*4  descr_by_cli

C--	Select bit mask for client's descriptor
	integer*4  mask_by_cli


C--	VARIABLES ASSOCIATED WITH INTERPROCESSOR COMMUNICATION (SOCKETS)

C--	Bit map and bit count for select operations on sockets
	integer*4  select_mask, select_bits

C--	Socket descriptor, founding socket
	integer*4  socket_descr


C++	MISCELLANEOUS VARIABLES

C--	Server TCP address, port number
	integer*4  server_addr
        integer*4  server_port

C--     Total number of records transferred
        integer*4 tot_recs

        COMMON/FARMIPC/ DESCR_BY_CLI, MASK_BY_CLI,
     +  SELECT_MASK, SELECT_BITS, SOCKET_DESCR,
     +  SERVER_ADDR, SERVER_PORT, TOT_RECS
