      FUNCTION ZVAXTIMECOMP (T1,T2)
c-------------------------------------------------------------------------------
c Purpose and methods   : compares two times in standard VAX 64 bits format
c Returned value        : ZVAXTIMECOMP (integer)
c                          +1 if T1<T2    0 if T1=T2   -1 if T1>T2
c Inputs                : T1,T2 (integer)  
c Outputs               : T1 T2 unchanged
c Created               : 15-jul-1991   Alain PLUQUET
C-   Updated   9-SEP-1992   Robert E. Avery  Change name to ZVAXTIMECOMP,
C-                              for move CD_UTIL. 
c-------------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4 T1(2),T2(2),ZVAXTIMECOMP
      IF (T2(2).GT.T1(2)) THEN
        ZVAXTIMECOMP=1
      ELSE
        IF (T2(2).LT.T1(2)) THEN
          ZVAXTIMECOMP=-1
        ELSE
          IF (T2(1).GT.T1(1)) THEN
            ZVAXTIMECOMP=1
          ELSE
            IF (T2(1).LT.T1(1)) THEN
              ZVAXTIMECOMP=-1
            ELSE
              ZVAXTIMECOMP=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF      
      RETURN
      END
