      SUBROUTINE MKVTXW( ILAY, ISEC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make the 'data' and 'hits' for LAYER, SECTOR of the
C-                         VTX.
C-
C-   Inputs  : ILAY, ISECT
C-   Outputs : fills hit bank VSEC if SVTX(2) = 1.
C-             fills data banks VWDA if SVTX(1)=1. or SVTX(3)=1.
C-   Controls: 
C-
C-   Created  26-JUN-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:VTLOCA.INC/LIST'
C
      INTEGER NVDIM, NHDIM, NHMAX
      PARAMETER (NVDIM = 2, NHDIM = 11, NHMAX = 50 )
      INTEGER NUMV(NVDIM), ITRAV(NHMAX), NUMBV(NVDIM,NHMAX)
      INTEGER NHITSV, ILAY, ISEC, CELL
      REAL HITSV(NHDIM,NHMAX)
      INTEGER WIMAX, LVRFT, GZVRFT
      INTEGER IFADC(3)
      CHARACTER*4 NAMESV(0:2)
      DATA NAMESV /'VCL0','VCL1','VCL2'/
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LVRFT = GZVRFT()
        IF ( LVRFT .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** MKVTX:  bank VRFT not defined'
          CALL EXIT(1)
        ENDIF
        WIMAX = IC( LVRFT + 3 )
      ENDIF
C
      LAYER = ILAY
      SECTOR = ISEC
C
      DO 100 CELL = 1, WIMAX
        WIRE = CELL - 1
C
C **** Get stored hits for this cell
C
        NUMV(1) = SECTOR + 1
        NUMV(2) = CELL
        CALL GFHITS('VTX ', NAMESV(LAYER), NVDIM, NHDIM, NHMAX,
     &              0, NUMV, ITRAV, NUMBV, HITSV, NHITSV)
        IF ( NHITSV .LE. 0 ) GO TO 100
        IF ( NHITSV .GT. NHMAX ) THEN
          WRITE (LOUT,10) NHITSV, NHMAX
   10     FORMAT(/,' Subroutine MKVTX: The number of hits found in one',
     &       ' cell is: ',I4,' set to the maximum allowed: ',I3)
          NHITSV = NHMAX
        ENDIF
C
C **** Create the hits Zebra structure VTXH -- VLAY -- VSEC
C
        IF ( SVTX(2) .EQ. 1. ) THEN
          CALL BLVSEC( HITSV, NHITSV )
        ENDIF
C
C **** Create the "data" Zebra structure VTXH -- VLAY -- VSEC -- VWDA
C
        IF ( SVTX(1) .EQ. 1. .OR. SVTX(3) .EQ. 1. ) THEN
          CALL BLVWDA( HITSV, NHITSV )
        ENDIF
C
  100 CONTINUE
C
C **** Trim unused portion of banks
C
      CALL PUVSEC( LAYER, SECTOR )
      CALL PUVWDA( LAYER, SECTOR )
C
  999 RETURN
      END
