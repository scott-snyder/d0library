      SUBROUTINE L2_INZHST(NSIZE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Initialize /ZEBHST/, working area not written out
C-
C-   Created  23-FEB-1990   Jan Guida, Srini Rajagopalan
C-   Updated  17-JUL-1992   Herb Greenlee  Modifications for Unix
C-   Updated   7-APR-1993   Dan Claes  Call MZEBRA if NSIZE.EQ.0(rather than LT)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBHST.INC'
      INTEGER NSIZE,ENDZH,LOCF
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN 
        IF (NSIZE.EQ.0) CALL MZEBRA(0)
        ENDZH = ABS(NSIZE) - LOCF(ZHSTR(1)) + LOCF(IXHST)
        IF (ENDZH.LE.NNH) ENDZH = NNH + 1
        CALL MZSTOR (IXHST,'/ZEBHST/','Q',FENHST,LCLBH,LRHST,ZHSTR(1),
     &   ZHSTR(10000),ZHSTR(ENDZH))
        IDVHST=IXHST+2
        FIRST=.FALSE.
      ENDIF
C
  999 RETURN
      END
