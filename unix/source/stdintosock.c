#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>

int main(int argc, char **argv)

/*
C----------------------------------------------------------------------
C-
C-   Name: stdintosock
C-
C-   Purpose: This program copies characters from standard input to the
C-            named socket specified as the first argument.  If the second
C-            argument is non-zero, this program sends signal SIGUSR1
C-            to that process each time it writes data to the socket.
C-            Before sending characters from standard input, stdintosock 
C-            sends the name of the local terminal.
C-
C-   Usage:
C-
C-     stdintosock sockname pid
C-
C-   Arguments: 
C-
C-     sockname - Socket name.
C-     pid      - Process id to receive signal 22.
C-
C-   Created   9-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/
{
  char ch;                  /* Single character buffer */
  char *tty;                /* Pointer to terminal name */
  int sock;                 /* Socket file descriptor */
  pid_t pid;                /* Process id to receive signal. */
  struct sockaddr sockname; /* Named socket address structure */
  struct termios attr;      /* POSIX terminal attributes structure */

/* Check for a valid argument. */

  if(argc < 2)
    abort();
  if(strlen(argv[1]) > 14)
    abort();

/* Get process id. */

  if(argc >= 3)
    pid = atoi(argv[2]);
  else
    pid = 0;

/* Create an unnamed socket. */

  if((sock = socket(PF_UNIX, SOCK_STREAM, 0)) < 0)
    abort();

/* Connect our socket to the socket named in the first argument. */

  sockname.sa_family = PF_UNIX;
  strcpy(sockname.sa_data, argv[1]);
  if(connect(sock, &sockname, 
	     strlen(argv[1]) + sizeof(sockname.sa_family)) < 0)
    abort();

/* Get terminal name associated with standard output */

  tty = ttyname(1);
  if(tty == NULL)
    abort();
  if(write(sock, tty, strlen(tty)) < 0)
    abort();
  if(write(sock, "", 1) < 0)          /* Send null terminator */
    abort();

/* Turn off echo & terminal line processing. */

  if(tcgetattr(0, &attr) < 0)
    abort();
  attr.c_lflag &= ~(ICANON | ECHO);
  attr.c_cc[VMIN] = 1;
  attr.c_cc[VTIME] = 0;
  if(tcsetattr(0, TCSANOW, &attr) < 0)
    abort();

/* Copy single characters. */

  for(;;) {
    if((read(0, &ch, 1)) < 0)
      break;
    if(write(sock, &ch, 1) < 0)
      break;
    if(pid != 0)
      if(kill(pid, SIGUSR1) < 0)
	break;
    }
  close(sock);
  exit(0);
}
