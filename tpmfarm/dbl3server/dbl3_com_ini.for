      SUBROUTINE DBL3_COM_INI

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens the socket for DBL3 communications
C-
C-   Created  25-JAN-1994  Kirill Denisenko
C-
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER socket_dbl3
      COMMON /DBL3COM/ socket_dbl3
      CHARACTER*1 null
      CHARACTER*20 cli_node
      CHARACTER*20 inport, outport
      LOGICAL  first_rcp,first_in,first_out,ezerr
      INTEGER  lun,ioway,choseo,status
      INTEGER  get_host_address
      INTEGER*2 leno
      INTEGER*2 srvr_port
      LOGICAL   str$trim, cond
      INTEGER  socket_descr,cli_port,cli_addr
      INTEGER  make_socket,connect_socket
      INTEGER  shutdown_socket, close_socket
      INTEGER  dummy, t_count
      INTEGER  trulen

      LOGICAL  first /.false./
      SAVE     first
C----------------------------------------------------------------------

C-- Do it only once
      if (first) return
      first = .true.

      CALL inrcp('REMOTE_RCP',status)
      IF (status .NE. 0) THEN
        CALL errmsg('RCP error','CLI_INI',
     &      'Unable to find the REMOTE_RCP','F')
      ENDIF

C-Pick REMOTE_RCP bank
      CALL ezpick('REMOTE_RCP')
      IF (ezerr(status)) THEN
        CALL errmsg('CLI_INI','CLI_INI',
     &    'Unable to find REMOTE RCP bank','F')
      ENDIF

C-Read in input node parameters
      CALL ezget('INPUT_NODE',cli_node,status)
      IF (status .NE. 0) THEN
        CALL errmsg('RCP reading error','CLI_INI',
     &    'Unable to find the parameter CLI_NODE_IN','F')
      ENDIF

      CALL getenv('CLI_PORT_IN',inport)
      CALL ezget(inport(1:trulen(inport)),cli_port,status)
      IF (status .NE. 0) THEN
        CALL errmsg('RCP reading error','CLI_INI',
     &    'Unable to find the parameter CLI_PORT_IN','F')
      ENDIF

      CALL ezrset

C-Get the address of the corresponding node
      null = char(0)
      cli_node=cli_node(1:trulen(cli_node))
      cond = str$trim(cli_node, cli_node, leno)
      cli_node(leno+1:leno+1) = null
      cli_addr = get_host_address(cli_node)

C-Make a socket
      t_count = 0
21    socket_descr = make_socket()
      IF (socket_descr .LE. 0) THEN
        CALL errmsg('Make socket error','CLI_INI',
     &                'Unable to make a socket','F')
      ENDIF

C-Connect the socket to the port and the corresponding node address
      srvr_port = cli_port
      status = connect_socket(socket_descr,srvr_port,cli_addr)
      IF (status .NE. 0) THEN
        CALL errmsg('Connect socket error','CLI_INI',
     &              'Unable to connect the socket','W')
        status = shutdown_socket(socket_descr)
        status = close_socket(socket_descr)
        if(t_count.ge.3) then
          CALL errmsg('Connect socket error','CLI_INI', 
     &              'Unable to connect the socket','F')
          CALL D0_ABORT('Abort the RECO')
        endif
        call sleep(20)
        t_count = t_count + 1
        goto 21
      ENDIF

      t_count = 0

C-Store input node address and its connected socket
      socket_dbl3 = socket_descr

  999 RETURN
      END
