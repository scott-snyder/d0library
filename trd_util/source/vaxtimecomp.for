      FUNCTION VAXTIMECOMP (T1,T2)
c-------------------------------------------------------------------------------
c Purpose and methods   : compares two times in standard VAX 64 bits format
c Returned value        : VAXTIMECOMP (integer)
c                          +1 if T1<T2    0 if T1=T2   -1 if T1>T2
c Inputs                : T1,T2 (integer)  
c Outputs               : T1 T2 unchanged
c Created               : 15-jul-1991   Alain PLUQUET
c-------------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4 T1(2),T2(2),VAXTIMECOMP
      IF (T2(2).GT.T1(2)) THEN
        VAXTIMECOMP=1
      ELSE
        IF (T2(2).LT.T1(2)) THEN
          VAXTIMECOMP=-1
        ELSE
          IF (T2(1).GT.T1(1)) THEN
            VAXTIMECOMP=1
          ELSE
            IF (T2(1).LT.T1(1)) THEN
              VAXTIMECOMP=-1
            ELSE
              VAXTIMECOMP=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF      
      END
