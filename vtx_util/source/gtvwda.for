      SUBROUTINE GTVWDA(LAYER, SECTOR, WIRE, END, NDATA, VDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch wire data from VWDA for one channel 
C-                         of the vertex chamber
C-
C-   Inputs  : LAYER, SECTOR, WIRE, END: VTX channel
C-   Outputs : NDATA: number of pulses on this channel
C-             VDATA: data for this channel
C-   Controls: 
C-
C-   Created  31-JAN-1989   Peter Grudberg (from Chris Klopfenstein)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER LAYER, SECTOR, WIRE, END
      INTEGER MXHTOT, NWVWDA
      PARAMETER ( MXHTOT = 50 )
      PARAMETER ( NWVWDA = 8 )
C
      INTEGER NDATA
      REAL VDATA(NWVWDA, MXHTOT)
      INTEGER KPVWDA, IPVWDA
      INTEGER NBFADC
C----------------------------------------------------------------------
      KPVWDA = LVWDA( SECTOR, LAYER )
C  How many hits on this channel?(return if no hits)
      NDATA = IQ( KPVWDA + 2*WIRE + END + 4 )
      IF ( NDATA .LE. 0 ) GO TO 999
C  Find pointer to data
      NBFADC = IQ( KPVWDA + 2 )
      IPVWDA = IQ( KPVWDA + NBFADC + 2*WIRE + END + 4 )
C  Load data into VDATA array
      IF ( NDATA .LE. MXHTOT ) THEN
        CALL UCOPY( Q(KPVWDA+IPVWDA), VDATA, NWVWDA*NDATA )
      ELSE
        CALL UCOPY( Q(KPVWDA+IPVWDA), VDATA, NWVWDA*MXHTOT )
      ENDIF
  999 RETURN
      END
