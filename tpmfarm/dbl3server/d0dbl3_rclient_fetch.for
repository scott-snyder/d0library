      SUBROUTINE D0DBL3_RCLIENT_FETCH(NSEND,IBUF,IERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by the client of the database reading server
C-                         to request and get the database contenets from the
C-                         server node to worker node.
C-    Note. This routine should always fill and send back to client
C-          IBUF array, except when something goes wrong in which case
C-          only IERROR flag needs to be set.
C-
C-   Inputs  : NSEND         The dimension of IBUF expected to be sent
C-             IBUF(1-10)    Path name to data
C-             IBUF(11)      Legth of character PATH
C-             IBUF(12)      Run nubmer
C-             IBUF(13)      Crate or Module number
C-             IBUF(14-18)   Reserved
C-   Outputs :
C-             IBUF(19-49)   Some basic links
C-             IBUF(50)      For problem diagnostics, 0=everything OK
C-             IBUF(51-100)  Reserved
C-             IBUF(101-NND) Contents of database store
C-             IERROR        0=OK, otherwise trouble
C-   Controls:
C-
C-   Created   7-JAN-1994   S. ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER socket_dbl3
      COMMON /DBL3COM/ socket_dbl3
      INTEGER NSEND,IBUF(NSEND)
      INTEGER IERROR

C
C -
C - Kirill will write this network communication routine and then on
C - server node will activate a call to D0DBL3_RSERVER_FETCH(NSEND,IBUF,IER)
C - which I am providing. If IER is non-zero then no need to send the IBUF
C - and only IERROR is needed to be set.
C -
C
C----------------------------------------------------------------------
C My part starts here - K. Denisenko 01/25/94

      integer event_rqst(10) /10 * 0/
      integer status, nreceive

      integer write_socket, read_socket

C Initilaize ierror
      ierror = -7

C Send a request to the I/O server to send the path and run info
      event_rqst(1) = 6
      event_rqst(2) = nsend
      status = write_socket(socket_dbl3,event_rqst,10)
      if (status.ne.10) then
        ierror = -1
        goto 99
      endif

C Receive a confirmation
      status = read_socket(socket_dbl3,event_rqst,10)
      if (status.ne.10) then
        ierror = -2
        goto 99
      endif

C Send the auxil buffer
      status = write_socket(socket_dbl3,ibuf,nsend)
      if (status.ne.nsend) then
        ierror = -3
        goto 99
      endif

C Read the confirmation for the auxil buffer
      status = read_socket(socket_dbl3,event_rqst,10)
      if (status.ne.10.or.event_rqst(2).ne.0) then
        ierror = -4
        goto 99
      endif
      nreceive = event_rqst(3)

C Send a request for a big buffer
      event_rqst(1) = 16
      status = write_socket(socket_dbl3,event_rqst,10)
      if (status.ne.10) then
        ierror = -5
        goto 99
      endif

C Receive a big dbl3 buffer of nreceive words
      status = read_socket(socket_dbl3,ibuf(19),nreceive)
      if(status.ne.nreceive) then
        ierror = -6
        goto 99
      endif

C Successful completion
      nsend = nreceive
      ierror = 0
      goto 999

   99 CONTINUE
      call dbl3_com_fin

  999 RETURN
      END
