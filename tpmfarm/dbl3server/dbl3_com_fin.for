      SUBROUTINE DBL3_COM_FIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Created   25-JAN-1993   Kirill Denisenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER socket_dbl3
      COMMON /DBL3COM/ socket_dbl3
      INTEGER  status,words
      PARAMETER (words = 10)
      INTEGER  terminate_request(words)
      INTEGER  socket,close_socket, write_socket,shutdown_socket
      DATA     terminate_request/9,9*0/

      LOGICAL  first /.false./
      SAVE     first
C----------------------------------------------------------------------

C-- Do it only once
      if (first) return
      first = .true.

C-Shut down the socket and close it
      status = shutdown_socket(socket_dbl3)
      status = close_socket(socket_dbl3)

C-Send a message if it is closed successfully
      IF (status .EQ.0 ) THEN
        CALL errmsg('Closed ouput socket','DBL3_COM_FIN',
     &    'The output socket is closed successfully','I')
      ENDIF

  999 RETURN
      END
