      SUBROUTINE VAXOFT(IVAX, ITIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert VAX time to D0 time.
C-
C-   Inputs  : IVAX      Current time in 64-bit VAX form
C-   Outputs : ITIM        D0 time in 32-bits
C-   Controls: None
C-
C-   Created  11-JUN-1990   Steve Adler 
C-   Updated  10-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  14-APR-1992   Jan S. Hoftun  ELN version
C-   Updated  17-Apr-1992   Herbert Greenlee
C-       Simplify offline code (left level 2 alone)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LOCTD0T
      INTEGER ITIM,IVAX(2)
      INTEGER D0_BASE_DAY, D0_BASE_VAXTIM(2)
      DOUBLE PRECISION DVAX, DBASE
      INTEGER D0_TIME_SEC, D0_HOUR
      CHARACTER*24 ASCTIM
      LOGICAL OK, SYS$BINTIM
C&IF VAXELN
C&      INCLUDE 'ELN$:FORTRAN_DEFS.FOR'
C&      RECORD/ELN$TIME_RECORD/ TIME_REC
C&      RECORD/LARGE_INTEGER/ BASE_TIME,NOW_TIME,DIFF_TIME
C&ENDIF
      INCLUDE 'D0$PARAMS:BASE_DATE.PARAMS'
C----------------------------------------------------------------------
      ASCTIM = BASDAT
C&IF VAXELN
C&      CALL ELN$TIME_VALUE_LIST(BASE_TIME,ASCTIM)
C&      NOW_TIME.ELN$LI1=IVAX(1)
C&      NOW_TIME.ELN$LI2=IVAX(2)
C&      CALL ELN$SUB_LARGE_INTEGERS_LIST(BASE_TIME,NOW_TIME,DIFF_TIME)
C&      CALL ELN$TIME_FIELDS_LIST(DIFF_TIME,TIME_REC)
C&      D0_BASE_DAY=TIME_REC.DAY
C&      D0_HOUR=TIME_REC.HOUR
C&      D0_TIME_SEC=D0_BASE_DAY*3600*24+D0_HOUR*3600+
C&     &  TIME_REC.MINUTE*60+TIME_REC.SECOND
C&      ITIM=D0_TIME_SEC        ! IGNORE any possible offset to FNAL standard time
C&ELSE
C
C     Get base time in 64-bits from system.  Choice of base date determins
C     range in 32 bits.  Sign bit flips in 2^31 seconds (=68 years).
C
      OK = SYS$BINTIM(ASCTIM, D0_BASE_VAXTIM)
C-
C- Convert VAX time to double precision seconds.
C-
      IF(IVAX(1).GE.0)THEN
        DVAX = 1.D-7*IVAX(1) + (2.D0**32*1.D-7)*IVAX(2)
      ELSE
        DVAX = 1.D-7*IVAX(1) + (2.D0**32*1.D-7)*(IVAX(2)+1)
      ENDIF
C-
C- Convert base time to double precision seconds.
C-
      IF(D0_BASE_VAXTIM(1).GE.0)THEN
        DBASE = 1.D-7*D0_BASE_VAXTIM(1) + 
     &    (2.D0**32*1.D-7)*D0_BASE_VAXTIM(2)
      ELSE
        DBASE = 1.D-7*D0_BASE_VAXTIM(1) + 
     &    (2.D0**32*1.D-7)*(D0_BASE_VAXTIM(2)+1)
      ENDIF
C-
C- Calculate integer time offset.
C-
      D0_TIME_SEC = DVAX - DBASE
C-
C- Convert to D0 timezone.
C-
      ITIM=LOCTD0T(D0_TIME_SEC)
C&ENDIF
  999 RETURN
      END
