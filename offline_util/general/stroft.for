      SUBROUTINE STROFT(TIMSTR_ARG, ITIM, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts TIMSTR from ASCII to D0 time.  If 
C-                         TIMSTR_ARG ends in 'D0', then it is assumed to
C-                         refer to Batavia time.  If the 'D0' tag is 
C-                         absent, then TIMSTR_ARG is assumed to be a
C-                         a time in the local timezone.
C-
C-   Inputs  : TIMSTR_ARG  ASCII version of current time
C-   Outputs : ITIM        32-bit D0 standard time
C-             IER         Integer with status 0=D0 tag found, 1=Not found
C-   Controls: None
C-
C-   Created  28-JUN-1989   Jason McCampbell (MSU)
C-   Updated  11-DEC-1991   Herbert Greenlee
C-      UNIX compatible version
C-   Updated  17-Apr-1991   Herbert Greenlee
C-      Clean up.  Get rid of machine blocks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITIM,D0_TIME,D0TLOCT
      INTEGER IER, IN
      INTEGER  CURTIM(2)
      CHARACTER*(*) TIMSTR_ARG
      CHARACTER*23 TIMSTR
      LOGICAL OK, SYS$BINTIM
C----------------------------------------------------------------------
C-
C-  Check for D0 tag
C-
      TIMSTR = TIMSTR_ARG
      IN = INDEX(TIMSTR,'D0')
      IF(INDEX(TIMSTR,'D0') .NE. 0) THEN
        IER=0
        TIMSTR(IN:) = ' '
      ELSE
        IER=1
      ENDIF
C-
C-  First convert TIMSTR to 64-bit binary
C-
      OK = SYS$BINTIM(TIMSTR, CURTIM)
C-
C-  Now convert to 32-bit and subtract base time
C-
      CALL VAXOFT(CURTIM,D0_TIME)
C-
C- If the D0 tag is present, then call D0TLOCT to cancel the VAXOFT timezone
C- correction.  Otherwise, just return the answer.
C-
      IF(IER.EQ.0)THEN
        ITIM = D0TLOCT(D0_TIME)
      ELSE
        ITIM = D0_TIME
      ENDIF
  999 RETURN
      END
