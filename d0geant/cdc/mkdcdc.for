      SUBROUTINE MKDCDC(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 'hits' and 'data' for one sector 
C-
C-   Inputs  : LAYER, SECTOR
C-   Outputs : fills the hits bank DSEC if SCDC(2)=1
C-             fills the "data" bank DSEC if SCDC(1)=1 or SCDC(3)=1
C-   Controls: 
C-
C-   Created  20-MAR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZDRFT.LINK/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CDCPAR.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
C
      INTEGER    NVDIM, NHDIM, NHMAX
      PARAMETER (NVDIM = 2, NHDIM = 10, NHMAX = 50 )
      INTEGER NUMV(NVDIM),ITRAV(NHMAX),NUMBV(NVDIM,NHMAX)
      INTEGER NHITSV, LAYER, SECTOR, CELL
      REAL HITSV(NHDIM,NHMAX)
      INTEGER WIMAX, LDRFT
      INTEGER IFADC(3)
C
C----------------------------------------------------------------------
      LDRFT = LC( LDGEH - IZDRFT )
      IF ( LDRFT .LE. 0 ) THEN
        WRITE(LOUT,*) '**** MKDCDC : bank LDRFT not defined'
        CALL EXIT(1)
      ENDIF
      WIMAX = IC ( LDRFT + 6)
      DO 100 CELL = 1, WIMAX
        IFADC(1) = LAYER 
        IFADC(2) = SECTOR
        IFADC(3) = CELL - 1
C
C ****  Get stored hits for this cell
C
        NUMV(1) = SECTOR + 1
        IF ( NUCDIM(CELL) .EQ. 2 ) NUMV(2) = CELL - 1
        CALL GFHITS('CDC ', NAMESW(CELL, LAYER+1), NUCDIM(CELL),
     &            NHDIM,NHMAX, 0, NUMV, ITRAV, NUMBV, HITSV, NHITSV)
C
        IF (NHITSV.GT.NHMAX) THEN
          WRITE (LOUT,10) NHITSV, NHMAX
   10     FORMAT(/,' Subroutine DIGCDC : The # of hits found in one',
     &       ' cell  is: ',I4,' set to the maximum allowed: ',I3)
          NHITSV=NHMAX
        ENDIF
C
C
C ****  Create the hits Zebra Structure CDCH -- DLYR -- DSEC
C
        IF ( SCDC(2) .EQ. 1. ) THEN
          CALL BLDSEC( HITSV, NHDIM, NHITSV, IFADC )
        ENDIF
C
C ****  Create the "DATA" Zebra structure CDCH -- DLYR -- DSEC -- DCDA
C
        IF ( SCDC(1) .EQ. 1. .OR. SCDC(3) .EQ. 1. ) THEN
          CALL BLDCDA( HITSV, NHDIM, NHITSV, IFADC )
        ENDIF
  100   CONTINUE
C
C ****  Correct the size of the HITS bank DSEC
C
        IF ( SCDC(2) .EQ. 1. ) THEN
          IF ( LDSEC( SECTOR, LAYER) .NE. 0 )
     &              CALL PUDSEC( IFADC(1), IFADC(2) )
        ENDIF
C
C ****  Correct the size of the DATA bank DCDA
C
        IF ( SCDC(1) .EQ. 1. .OR. SCDC(3) .EQ. 1. ) THEN
          IF ( LDCDA( SECTOR, LAYER) .NE. 0 )
     &              CALL PUDCDA( IFADC(1), IFADC(2) )
        ENDIF
C
  999 RETURN
      END
