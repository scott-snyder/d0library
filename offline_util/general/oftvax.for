      SUBROUTINE OFTVAX(ITIM, IVAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert D0 time to VAX time.
C-
C-   Inputs  : ITIM        D0 time in 32-bits
C-   Outputs : IVAX      Time in VAX format
C-   Controls: None
C-
C-   Created 11-JUN-1990   Steve Adler 
C-   Update  11-DEC-1991   Herbert Greenlee
C-      UNIX compatible version
C-   Update  17-Apr-1992   Herbert Greenlee
C-      Clean up.  Get rid of machine blocks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      INTEGER ITIM, LOCAL_TIME, IVAX(2)
      INTEGER JVAX(4)
      INTEGER D0_BASE_VAXTIM(2)
      DOUBLE PRECISION DBASE, DVAX
      CHARACTER*23 ASCTIM
      LOGICAL OK, SYS$BINTIM
      INTEGER D0TLOCT
      INCLUDE 'D0$PARAMS:BASE_DATE.PARAMS'
C----------------------------------------------------------------------
C-
C- Convert time to local timezone.
C-
      LOCAL_TIME = D0TLOCT(ITIM)
C-
C- Get base time in 64-bits from system.
C-
      ASCTIM = BASDAT
      OK = SYS$BINTIM(ASCTIM, D0_BASE_VAXTIM)
C-
C- Convert base time from 64-bit integer to double precision.
C-
      IF(D0_BASE_VAXTIM(1).GE.0)THEN
        DBASE = D0_BASE_VAXTIM(1) + 2.D0**32*D0_BASE_VAXTIM(2)
      ELSE
        DBASE = D0_BASE_VAXTIM(1) + 2.D0**32*(D0_BASE_VAXTIM(2)+1)
      ENDIF
C-
C- Calculate double precision VAX time.
C-
      DVAX = DBASE + 1.D7*LOCAL_TIME
C-
C- Convert to 64-bit integer seconds (initially store as 4 16-bit words to 
C- avoid overflow).
C-
      JVAX(4) = 2.D0**(-48) * DVAX
      DVAX = DVAX - 2.D0**48 * JVAX(4)
      JVAX(3) = 2.D0**(-32) * DVAX
      DVAX = DVAX - 2.D0**32 * JVAX(3)
      JVAX(2) = 2.D0**(-16) * DVAX
      JVAX(1) = DVAX - 2.D0**16 * JVAX(2)
      IVAX(2) = IOR(ISHFT(JVAX(4),16),JVAX(3))
      IVAX(1) = IOR(ISHFT(JVAX(2),16),JVAX(1))
  999 RETURN
      END
