      SUBROUTINE ZFVSEC ( LAYER, SECTOR, VSCHIT, NPAIR, DROPPED )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VSEC bank with combined wire hits
C-                         from array VSCHIT.
C-
C-   Inputs  : LAYER, SECTOR: location in VTX
C-             VSCHIT(*)    = Combined hit data
C-             NPAIR(4,*)   = nb of hits per wire: total, matched,
C-                            unmatched (+z), unmatched (-z)
C-   Outputs : DROPPED(4)   = number of hits dropped from VSEC due to pileup at
C-                            cathode and anode
C-
C-   Created  17-JUL-1987   Olivier Callot
C-   Updated  18-FEB-1988   Olivier Callot  CDLOCA for layer add
C-   Modified 01-FEB-1989   Peter Grudberg  for use for VTX
C-   Updated  14-JAN-1990   Peter Grudberg  order hits in time
C-   Updated   7-JUN-1992   Ed Oltman  suppress hit pile-up at anode,cathode
C-   Updated   3-AUG-1992   M. Pang  Fix hit-counting bug
C-   Updated  19-OCT-1992   Peter M. Grudberg Check both cathode regions for
C-                                      uniqueness
C-   Updated  15-NOV-1993   Peter Grudberg   Add DROPPED argument
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'

      INTEGER KPVSEC, NPAIR (4,0:*)
      INTEGER IPTR, N, NHTOT, LPULS, NFADC
      INTEGER LAYER, SECTOR, WIRE
      INTEGER DROPPED(4)
      INTEGER STATUS, STAT
      REAL VSCHIT(*)
      INTEGER MXHWIR, IPAIR
      PARAMETER ( MXHWIR = 100 )
      REAL TIME(MXHWIR)
      INTEGER ORDER(MXHWIR), OFFSET , NHITS, OFF1
      REAL LAST1, LAST2
C----------------------------------------------------------------------
      KPVSEC = LVSEC( SECTOR, LAYER )
      N=1
      NHTOT=0
      NFADC = IQ( KPVSEC + 2 )
      LPULS = IQ( KPVSEC + 3 )
      IPTR = 2 * NFADC + 4
      CALL VZERO(DROPPED,4)
      DO WIRE = 0 , NFADC-1
        NHITS = 0
        IF ( NPAIR(1,WIRE) .GT. 0) THEN
          IQ (KPVSEC+WIRE+NFADC+4) = IPTR
C
C ****  Fill TIME array with hit times and sort in increasing order:
C
          DO IPAIR = 1, NPAIR(1,WIRE)
            TIME(IPAIR) = VSCHIT(N + 8 + LPULS*(IPAIR-1))
          ENDDO
          CALL SORTZV(TIME, ORDER, NPAIR(1,WIRE), 1, 0, 0)
C
C ****  Now fill VSEC with hits in time order
C
          LAST1 = 9.999E9
          LAST2 = 9.999E9
          DO IPAIR = 1, NPAIR(1,WIRE)
            OFFSET = N + LPULS*(ORDER(IPAIR) - 1)
C
C ****  Get rid of hit pile-up at anode
C
            IF (IPAIR .LT. NPAIR(1,WIRE)) THEN
              OFF1 = N + LPULS*(ORDER(IPAIR+1) - 1)
              IF (VSCHIT(OFFSET+8) .LE. 0. .AND.
     &            VSCHIT(OFF1  +8) .LE. 0.) THEN
                DROPPED(1) = DROPPED(1) + 1
                CALL UCOPY(VSCHIT(OFFSET+9),STATUS,1)
                STAT = IBITS(STATUS,0,2)
                DROPPED(5-STAT) = DROPPED(5-STAT) + 1
                GO TO 40
              ENDIF
            ENDIF
C
C ****  Get rid of hit pile-up at cathode (skip the rest of the hits for this
C ****  wire if the drift distances for BOTH sides of the wire are the same as
C ****  for the previous hit).
C
            IF (VSCHIT(OFFSET + 1) .EQ. LAST1 .AND.
     &          VSCHIT(OFFSET + 2) .EQ. LAST2 .AND.
     &          VSCHIT(OFFSET + 8) .GT. 0.0) THEN
              DROPPED(1) = DROPPED(1) + 1
              CALL UCOPY(VSCHIT(OFFSET+9),STATUS,1)
              STAT = IBITS(STATUS,0,2)
              DROPPED(5-STAT) = DROPPED(5-STAT) + 1
              GO TO 40
            ENDIF
            LAST1 = VSCHIT(OFFSET + 1)
            LAST2 = VSCHIT(OFFSET + 2)
            NHTOT = NHTOT + 1
            NHITS = NHITS + 1
            CALL UCOPY( VSCHIT(OFFSET), Q(KPVSEC+IPTR), LPULS)
            IPTR = IPTR + LPULS
   40       CONTINUE
          ENDDO
   10     N    = N    + LPULS * NPAIR(1,WIRE)
        ELSE
          IQ (KPVSEC+WIRE+NFADC+4)=0
        ENDIF
        IQ( KPVSEC+WIRE+4) = NHITS
      ENDDO
      NHITS = 0
      IQ (KPVSEC+1)=NHTOT
      RETURN
      END
