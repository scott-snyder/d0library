      FUNCTION MRPADR(CRATIO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inverse of function MSPADR.
C-                         Calculate Z position from given Charge Ratio.
C-
C-   Returned value  : MRPADR ; Z-coordinate in one pad pattern cycle (cm).
C-   INPUTS  : CRATIO ; Charge Ratio (QA-QB)/(QA+QB) 
C-
C-   Created   31-MAY-1991   Susumu Igarashi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ND
      REAL    CRATIO,MRPADR

      PARAMETER(ND=32)
      REAL Z_00(ND)
      REAL PADRAT(ND)
      DATA Z_00/
     &  0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0,
     & 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
     & 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0,
     & 30.0, 30.48/
      DATA PADRAT/
     &0.616,0.600,0.556,0.500,0.430,0.360,0.310,0.248,0.192,0.140,
     &0.082,0.030,-0.016,-0.07,-0.11,-0.152,-0.2,-0.25,-0.294,-0.33,
     &-0.364,-0.4,-0.44,-0.48,-0.512,-0.544,-0.58,-0.61,-0.63,-0.65,
     &-0.655,-0.656/
C----------------------------------------------------------------------
        IF(CRATIO.GT.PADRAT(1)) THEN
          MRPADR=Z_00(1)
          GOTO 999
        ELSEIF(CRATIO.LE.PADRAT(32)) THEN
          MRPADR=Z_00(32)
          GOTO 999
        ELSE
          I=0
   10     I=I+1
          IF(CRATIO.LE.PADRAT(I) .AND. CRATIO.GT.PADRAT(I+1))THEN
            MRPADR=Z_00(I)+(Z_00(I+1)-Z_00(I))
     &            *(CRATIO-PADRAT(I))/(PADRAT(I+1)-PADRAT(I))
            GOTO 999
          ELSE 
            GOTO 10
          ENDIF
        ENDIF

  999 RETURN
      END
