      SUBROUTINE ZFVWDA ( LAYER, SECTOR, HITLST, NPULSE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VWDA bank with raw hits from array
C-                         HITLST
C-
C-   Inputs  : LAYER, SECTOR = cell location in VTX
C-             HITLST(*)    = Combined hit data
C-             NPULSE(0:7,0:1)= nb of hits per wire, end 
C-
C-   Created  17-JUL-1987   Olivier Callot
C-   Updated  18-FEB-1988   Olivier Callot  CDLOCA for layer add 
C-   Modified 01-FEB-1989   Peter Grudberg  for use for VTX
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'

      INTEGER KPVWDA, NPULSE (0:7,0:1)
      INTEGER IPTR, N, NHTOT, LPULS, NFADC
      INTEGER IFADC, LAYER, SECTOR, WIRE, END

      REAL HITLST(*)
C----------------------------------------------------------------------
      KPVWDA = LVWDA( SECTOR, LAYER )
      N=1
      NHTOT=0
      NFADC = IQ( KPVWDA + 2 )
      LPULS = IQ( KPVWDA + 3 )
      IPTR = 2 * NFADC + 4
      DO 1 IFADC = 0 , NFADC-1
        WIRE = IFADC / 2
        END = MOD( IFADC, 2 )
        IQ( KPVWDA+IFADC+4 ) = NPULSE (WIRE,END)
        IF ( NPULSE(WIRE,END) .GT. 0) THEN
          IQ (KPVWDA+IFADC+NFADC+4) = IPTR
          CALL UCOPY(HITLST(N),Q(KPVWDA+IPTR),LPULS*NPULSE(WIRE,END))
          N    = N    + LPULS * NPULSE(WIRE,END)
          IPTR = IPTR + LPULS * NPULSE(WIRE,END)
          NHTOT = NHTOT + NPULSE (WIRE,END)
        ELSE
          IQ (KPVWDA+IFADC+NFADC+4)=0
        ENDIF
    1 CONTINUE
      IQ (KPVWDA+1)=NHTOT
      RETURN
      END
