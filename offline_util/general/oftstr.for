      SUBROUTINE OFTSTR(ITIM, TIMSTR_ARG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert D0 time to character string string 
C-                         with the time and D0 appended.
C-
C-   Inputs  : ITIM        32-bit D0 standard time
C-   Outputs : TIMSTR      String with the current time in it
C-   Controls: None
C-
C-   Created  28-JUN-1989   Jason McCampbell (MSU)
C-   Updated  11-DEC-1991   Herbert Greenlee
C-      UNIX compatible version.
C-   Updated  17-Dec-1992   Herbert Greenlee
C-      Get rid of machine blocks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4 ITIM, IVAX(2)
      INTEGER*2 TIMLEN
      CHARACTER*(*) TIMSTR_ARG
      CHARACTER*26 TIMSTR
      LOGICAL OK, SYS$ASCTIM
      INTEGER LOCTD0T
C----------------------------------------------------------------------
C-
C-  Call the routine to add the base time and convert to 64-bit form.
C-  Call LOCTD0T to cancel OFTVAX timezone correction.
C-
      CALL OFTVAX(LOCTD0T(ITIM), IVAX)
      OK = SYS$ASCTIM(TIMLEN, TIMSTR, IVAX, 0)
C-
C- Drop hundredths (if present) and append tag.
C-
      IF(TIMSTR(TIMLEN-2:TIMLEN-2).EQ.'.')TIMLEN = TIMLEN - 3
      TIMSTR=TIMSTR(1:TIMLEN)//' D0'
      TIMSTR_ARG = TIMSTR
  999 RETURN
      END
