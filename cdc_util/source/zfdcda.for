      SUBROUTINE ZFDCDA ( HITLST, NPULSE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill DCDA bank with one cell information
C-                         from array HITLST.
C-
C-   Inputs  : HITLST(*)    = Hits infos for all wires
C-             NPULSE(*)    = nb of hits per wire
C-
C-   Created  17-JUL-1987   Olivier Callot
C-   Updated  18-FEB-1988   Olivier Callot  CDLOCA for layer add 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'

      INTEGER KPDCDA, NPULSE (0:*)
      INTEGER IPTR, N, NHTOT, LPULS, NFADC

      REAL HITLST(*)
C----------------------------------------------------------------------
      KPDCDA = LDCDA( SECTOR, LAYER )
      N=1
      NHTOT=0
      NFADC = IQ( KPDCDA + 2 )
      LPULS = IQ( KPDCDA + 3 )
      IPTR = 2 * NFADC + 3
      DO 1 WIRE = 0 , NFADC-1
        IQ( KPDCDA+WIRE+4 ) = NPULSE (WIRE)
        IF ( NPULSE(WIRE) .NE. 0) THEN
          IQ (KPDCDA+WIRE+NFADC+4) = IPTR
          CALL UCOPY( HITLST(N),Q(KPDCDA+IPTR+1),LPULS*NPULSE(WIRE))
          N    = N    + LPULS * NPULSE(WIRE)
          IPTR = IPTR + LPULS * NPULSE(WIRE)
          NHTOT = NHTOT + NPULSE (WIRE)
        ELSE
          IQ (KPDCDA+WIRE+NFADC+4)=0
        ENDIF
    1 CONTINUE
      IQ (KPDCDA+1)=NHTOT
      RETURN
      END
