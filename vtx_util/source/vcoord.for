      SUBROUTINE VCOORD( LAYER, SECTOR, NHITSC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Combine wire data for one sector from VWDA
C-                         into hits, then store in VSEC (both ends of
C-                         the wires are read out)
C-
C-   Inputs  : LAYER, SECTOR = cell location in VTX
C-             Data in VWDA
C-   Outputs : Combined hits in VSEC
C-             NHITSC(4): total # of hits, hits with both ends,
C-                        (-z) hits, (+z) hits
C-
C-   Controls: 
C-
C-   Created  31-JAN-1989   Peter Grudberg (from Chris K's routine)
C-   Updated   8-APR-1991   Peter Grudberg  Fix indexing bug 
C-   Updated  15-NOV-1993   Peter Grudberg  VCOMBN --> VTX_COMBINE
C-                                          Add DROPPED argument to ZFVSEC call 
C-   Updated   2-DEC-1993   Liang-Ping Chen match the change to VTX_COMBINE  
C-   Updated   1-SEP-1994   Liang-ping Chen replace 1-wd UCOPY with EQUIVALENCE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER MXHTOT, NWVSEC
      PARAMETER( MXHTOT = 50 )          ! Max hits/wire-end
      PARAMETER( NWVSEC = 10 )
      REAL VSCHIT(NWVSEC,16*MXHTOT)
      REAL TIME(0:1), PEAK(0:1)
      INTEGER NHITSC(4)
      INTEGER LAYER, SECTOR, WIRE, NWVWDA
      PARAMETER ( NWVWDA = 8 )
      INTEGER HIT(0:1), STAT(0:1), STATUS
      INTEGER IE, IRAW(0:1), END
      REAL    R_STAT(0:1),R_STATUS, RRAW(0:1)
      EQUIVALENCE (STAT,R_STAT),(STATUS,R_STATUS),(IRAW,RRAW)
      INTEGER DROPPED(4)
      INTEGER NDATA1, NDATA2
      REAL VTDAT(NWVWDA,MXHTOT,0:1)
      LOGICAL MATCHED
C
      INTEGER I, IPAIR, NPAIR(4,0:7), PAIR(2*MXHTOT,2)
C----------------------------------------------------------------------
C
      CALL VZERO( NHITSC, 4 )
C
      DO 100 WIRE = 0 , 7
C
C **** Get data for wire end=0 (negative z)
C
        CALL GTVWDA( LAYER, SECTOR, WIRE, 0, NDATA1, VTDAT(1,1,0) )
C
C **** Get data for wire end=1 (positive z)
C
        CALL GTVWDA( LAYER, SECTOR, WIRE, 1, NDATA2, VTDAT(1,1,1) )
C
C **** Match pairs of hits
C
        CALL VMATCH( NDATA1, VTDAT(1,1,0), NDATA2, VTDAT(1,1,1), 
     &    NPAIR(1,WIRE), PAIR )
C
        DO 110 IPAIR = 1 , NPAIR(1,WIRE)
C
C **** Combine pair into a single hit
C
          HIT(0) = PAIR( IPAIR, 1 )
          HIT(1) = PAIR( IPAIR, 2 )
          MATCHED = HIT(0).GT.0 .AND. HIT(1).GT.0
          DO IE = 0, 1
            IF ( HIT(IE) .GT. 0 ) THEN
              TIME(IE) = VTDAT(2,HIT(IE),IE)
              PEAK(IE) = VTDAT(5,HIT(IE),IE)
              RRAW(IE) = VTDAT(6,HIT(IE),IE)
              R_STAT(IE)=VTDAT(8,HIT(IE),IE)
            ELSE
              TIME(IE) = 0.
              PEAK(IE) = 0.
              IRAW(IE) = 0
              STAT(IE) = 0
            ENDIF
          ENDDO
          END = 0
          IF ( PEAK(1) .GT. PEAK(0) ) END = 1
C
          CALL VTX_COMBINE(LAYER,SECTOR,WIRE,END,MATCHED,
     &      TIME,PEAK,STAT,IRAW,VSCHIT(1,NHITSC(1)+IPAIR))
C
C ****  Put VWDA info into status word
C
          R_STATUS=VSCHIT(10,NHITSC(1)+IPAIR)
          DO IE = 0, 1
            CALL MVBITS(HIT(IE),0,8,STATUS,16+8*IE)
          ENDDO
          VSCHIT(10,NHITSC(1)+IPAIR)=R_STATUS
C
  110   CONTINUE
        DO 105 I = 1, 4
          NHITSC(I) = NHITSC(I) + NPAIR(I,WIRE)
  105   CONTINUE
  100 CONTINUE
C
C **** Store hits in VSEC.  Subtract dropped hits from the total
C
      CALL ZFVSEC( LAYER, SECTOR, VSCHIT, NPAIR, DROPPED )
C
      DO I = 1, 4
        NHITSC(I) = NHITSC(I) - DROPPED(I)
      ENDDO
  999 RETURN
      END
