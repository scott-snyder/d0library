      SUBROUTINE PTSURW(WIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the FADC traces of a specific wire 
C-                 and its surrounding wires.
C-
C-   Inputs  : WIRE
C-   Outputs : NTFADC  NUMBER OF TOTAL  FADC HITS
C-             TLAY    ARRAY WITH LAYER NUMBER
C-             TWIR    ARRAY WITH WIRE NUMBERS 
C-             TFADC   ARRAY WITH ENERGY RELEASE PER WIRE
C-
C-   Created   6-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER WIRE
      INTEGER IWIRE     ! WIRES DISPLAYED
      INTEGER I,TMAX
      PARAMETER(TMAX=15)
      INTEGER TLAY(TMAX),TWIR(TMAX),NTFADC,NUMLAY
      REAL TFADC(128,TMAX)
      LOGICAL NHITWI
C----------------------------------------------------------------------
      NUMLAY=0
      IF ((WIRE.LT.1).AND.(WIRE.GT.256))   ! ILEGAL WIRE NUM
     X  GO TO 999      
      IWIRE = WIRE+1
      DO 10 I=1, 3
        IF(IWIRE.EQ.257)   ! Special case
     X    IWIRE=1
        IF(IWIRE.EQ.0)     ! Special case
     X    IWIRE=256
        WRITE(2,*)' IN PTSUR**'
        WRITE(2,*)' IWIRE=',IWIRE
        CALL PTGTRD(TMAX,NTFADC,IWIRE,TLAY,TWIR,TFADC,NHITWI)  
        IF (.NOT.NHITWI) THEN  
          CALL PUHEAD('PTADC9')
          CALL PTCHAN(NTFADC,TLAY,TWIR,TFADC,NUMLAY)
          CALL JPAUSE(1)
        ENDIF
        IWIRE=ABS(1-IWIRE)
  10  CONTINUE
  999 RETURN
      END
