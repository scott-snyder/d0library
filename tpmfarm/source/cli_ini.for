      SUBROUTINE CLI_INI(lun,choseo,ioway,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LUN   = file unit No.
C-             IOWAY = 0 input
C-                   = 1 output
C-   Outputs : NONE
C-   Controls:
C-
C-   Created   5-MAR-1993   Hailin Li and Kirill Denisenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'pfarms.inc'
      EXTERNAL cli_fzio
      CHARACTER*20 cli_node, cli_ports
      CHARACTER*20 inport, outport, pids
      CHARACTER*20 inkey, outkey
      CHARACTER*200 command
      LOGICAL  first_rcp, first_in, first_out, ezerr
      INTEGER  pid, cli_port
      INTEGER  lun, ioway, choseo, status
      LOGICAL  OK
      INTEGER  trulen, getpid
      SAVE     first_rcp, first_in, first_out
      DATA     first_rcp/.true./,first_in/.true./,first_out/.true./
C----------------------------------------------------------------------
      OK = .FALSE.

      IF (first_rcp) THEN
        first_rcp = .false.
        CALL inrcp('REMOTE_RCP',status)
        IF (status .NE. 0) THEN
          CALL errmsg('RCP error','CLI_INI',
     &      'Unable to find the REMOTE_RCP','F')
        ENDIF

C Disables interrupts 
        call sig_ign(SIGINT )
        call sig_ign(SIGUSR1)
        call sig_ign(SIGUSR2)

C Get the pid
	pid = getpid()
	write(pids,*) pid

      ENDIF

      IF( (.NOT.first_in) .AND. (.NOT.first_out)) GOTO 99

C-Pick REMOTE_RCP bank
        CALL ezpick('REMOTE_RCP')
        IF (ezerr(status)) THEN
          CALL errmsg('CLI_INI','CLI_INI',
     &      'Unable to find REMOTE RCP bank','F')
        ENDIF

C-Read in input node parameters
      IF (first_in .AND. (ioway .EQ. 0)) THEN

        first_in = .false.

        CALL ezget('INPUT_NODE',cli_node,status)
        IF (status .NE. 0) THEN
          CALL errmsg('RCP reading error','CLI_INI',
     &      'Unable to find the parameter CLI_NODE_IN','F')
        ENDIF

        CALL getenv('CLI_PORT_IN',inport)
        CALL ezget(inport(1:trulen(inport)),cli_port,status)
        IF (status .NE. 0) THEN
          CALL errmsg('RCP reading error','CLI_INI',
     &      'Unable to find the parameter CLI_PORT_IN','F')
        ENDIF
        write(cli_ports,'(I5)') cli_port

        CALL ezrset

C Convert key_in to string
	write(inkey,'(I3)') key_in

C forks the inreader.x 
        command = 'inreader.x '//inkey(1:trulen(inkey))//' '
     &            //pids(1:trulen(pids))//' '
     &            //cli_node(1:trulen(cli_node))//' '
     &            //cli_ports(1:trulen(cli_ports))
     &            //'> inreader.log &'
	call system(command(1:trulen(command)))

C sleep for 5 second for the shared memories to be created
        call sleep(5)

      ENDIF

      IF (first_in) THEN
        CALL errmsg('I/O order error','CLI_INI',
     &              'Output before input','W')
        GOTO 999
      ENDIF

      IF (first_out .AND. (ioway .EQ. 1)) THEN

        first_out = .false.

C-Read in output node parameters
        CALL ezget('OUTPUT_NODE',cli_node,status)
        IF (status .NE. 0) THEN
          CALL errmsg('RCP reading error','CLI_INI',
     &        'Unable to find the parameter CLIENT_NODE_OUT','F')
        ENDIF

        CALL getenv('CLI_PORT_OUT',outport)
        CALL ezget(outport(1:trulen(outport)),cli_port,status)
        IF (status .NE. 0) THEN
          CALL errmsg('RCP reading error','CLI_INI',
     &        'Unable to find the parameter CLI_PORT_OUT','F')
        ENDIF
	write(cli_ports,'(I5)') cli_port

        CALL ezrset

C Convert key_out to string
	write(outkey,'(I3)') key_sta

C forks the outwriter.x 
        command = 'outwriter.x '//outkey(1:trulen(outkey))//' '
     &            //pids(1:trulen(pids))//' '
     &            //cli_node(1:trulen(cli_node))//' '
     &            //cli_ports(1:trulen(cli_ports))
     &            //' > outwriter.log &'
	call system(command(1:trulen(command)))

C sleep for 5 second for the shared memories to be created
	call sleep(5)

      ENDIF

   99 CONTINUE

      IF (ioway .EQ. 0 ) THEN
        CALL fzfile(lun,precl,'CSI')
      ELSE
        CALL fzfile(lun,precl,'CSO')
C- Record the lun for STA's ( choseo=1 ) and DST's ( choseo=2 )
        lunout(choseo) = lun
      ENDIF

C-Hook the unit No. to a USER routine
      CALL fzhook(lun,cli_fzio,precl)

      OK = .TRUE.

  999 RETURN
      END
