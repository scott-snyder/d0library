C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_GETMAPS.FOR
C *1     4-NOV-1993 10:57:40 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_GETMAPS.FOR
      SUBROUTINE FHIT_GETMAPS(
     &      NUM_TH_HITS,NUM_PHI_HITS,NUM_DL_HITS,
     &      PTR_TH_HITS,PTR_PHI_HITS,PTR_DL_HITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return number of hits per sector 
C-      and number of first hit in sector in FHIT bank.
C-
C-   Outputs : 
C       NUM_TH_HITS(SECTOR,QUAD,HALF)
C       NUM_DL_HITS(SECTOR,QUAD,HALF)
C       NUM_PHI_HITS(SECTOR,HALF)
C       PTR_TH_HITS(SECTOR,QUAD,HALF)
C       PTR_DL_HITS(SECTOR,QUAD,HALF)
C       PTR_PHI_HITS(SECTOR,HALF)
C-
C-   Created  10-OCT-1991   Robert E. Avery
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  19-OCT-1993   Robert E. Avery  New format of FHIT, 
C-                                              separate delay line hits.
C-      NOTE: Argument list has changed!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C Output:
      INTEGER NUM_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_PHI_HITS(0:MXSECP,0:MXHALF)
      INTEGER PTR_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_PHI_HITS(0:MXSECP,0:MXHALF)
C Local:
      INTEGER IQFHIT(2)
      INTEGER IHIT 
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UBIT
      INTEGER ADDRESS
      INTEGER MASK11
      PARAMETER( MASK11 =  4095 )        ! 2**12 - 1
      INTEGER NPHI_SECTS 
      PARAMETER( NPHI_SECTS  =  (MXSECP+1)*(MXHALF+1) )
      INTEGER NTH_SECTS 
      PARAMETER( NTH_SECTS = (MXSECT+1)*(MXQUAD+1)*(MXHALF+1) )
C
C----------------------------------------------------------------------
      CALL VZERO(NUM_TH_HITS,NTH_SECTS)
      CALL VZERO(NUM_DL_HITS,NTH_SECTS)
      CALL VZERO(NUM_PHI_HITS,NPHI_SECTS)
      CALL VZERO(PTR_TH_HITS,NTH_SECTS)
      CALL VZERO(PTR_DL_HITS,NTH_SECTS)
      CALL VZERO(PTR_PHI_HITS,NPHI_SECTS)
C
      CALL GTFHIT(0,IQFHIT)
      IF ( IQFHIT(1) .GT. 0 ) THEN
        DO IHIT =  1, IQFHIT(1) 
          CALL GTFHIT(IHIT,IQFHIT)
          ADDRESS = IAND(MASK11, IQFHIT(1) )
          CALL FCODER(ADDRESS, HALF,UNIT,QUAD,SECTOR,WIRE,UBIT,1)
C
          IF ( UNIT .EQ. 0) THEN
            IF ( WIRE.LT.8 ) THEN
              NUM_TH_HITS(SECTOR,QUAD,HALF) = 
     &          NUM_TH_HITS(SECTOR,QUAD,HALF) + 1
              IF ( NUM_TH_HITS(SECTOR,QUAD,HALF).EQ.1 ) THEN
                PTR_TH_HITS(SECTOR,QUAD,HALF) = IHIT
              ENDIF
            ELSE
              NUM_DL_HITS(SECTOR,QUAD,HALF) = 
     &          NUM_DL_HITS(SECTOR,QUAD,HALF) + 1
              IF ( NUM_DL_HITS(SECTOR,QUAD,HALF).EQ.1 ) THEN
                PTR_DL_HITS(SECTOR,QUAD,HALF) = IHIT
              ENDIF
            ENDIF
          ELSE
            NUM_PHI_HITS(SECTOR,HALF) = 
     &          NUM_PHI_HITS(SECTOR,HALF) + 1
            IF ( NUM_PHI_HITS(SECTOR,HALF).EQ.1 ) THEN
              PTR_PHI_HITS(SECTOR,HALF) = IHIT
            ENDIF
          ENDIF
C
        ENDDO
      ENDIF
  999 RETURN
      END
