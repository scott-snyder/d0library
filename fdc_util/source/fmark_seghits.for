      SUBROUTINE FMARK_SEGHITS(MODULE,SEGNUM,MARKER,ITHBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mark the hits belonging to this segment
C-                         with marker number in selected 8 bits.
C-
C-   Inputs  : HALF,LAYER = Logical address of segment
C-             SEGNUM     = Segment number in FSGn bank
C-             MARKER     = Segment number or track number
C-   Outputs : none
C-   Controls: ITHBITS    = 0(already used), 1=bits 8-15 for FDC segment
C-                          number, 2=bits 16-23 for FDC track number
C-
C-   Created   8-APR-1991   Jeffrey Bantly
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  13-OCT-1993   Robert E. Avery  Call routine FGET_SEGHITS,
C-                              simplify, and do right thing for GEAN hits.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C  INPUT:
      INTEGER MODULE,SEGNUM,MARKER,ITHBITS
C  LOCAL:
      INTEGER MAX_SEGHIT
      PARAMETER( MAX_SEGHIT = 16 )
C
      INTEGER HALF,LAYER,UNIT
      INTEGER PTR_SC(MAX_SEGHIT),PTR_DA(MAX_SEGHIT)
      INTEGER NHITS,STATWORD,STARTBIT,IHIT
      INTEGER WIRE,LR(NBPSEN)
C
      REAL    CONT(63)
C
C----------------------------------------------------------------------
      STARTBIT=ITHBITS*8
      IF(STARTBIT.LT.0 .OR. STARTBIT.GT.31) THEN
        CALL ERRMSG('FTRAKS','FMARK_SEGHITS',
     &                           ' ITHBITS control input bad value','W')
        GOTO 999
      ENDIF
C
      HALF=MODULE/3
      LAYER = MODULE-3*HALF
      UNIT = LAYER/2
C
      CALL GTFSEG(MODULE,SEGNUM,CONT)
      CALL FGET_SEGHITS(HALF,LAYER,SEGNUM,NHITS,PTR_SC,PTR_DA)
      IF (NHITS.LE.0) THEN
        CALL ERRMSG('FTRAKS','FMARK_SEGHITS',' No hits on segment','W')
      ENDIF
C
C  Mark hits in FxSC bank with either segment, track number, and LR used
C  Uses only the lower 8 bits  of the segment or track number.
C
      DO IHIT=1,NHITS
        WIRE     =INT(CONT(3+IHIT)/2.)
        LR(IHIT)  =INT(CONT(3+IHIT))-WIRE*2
C
        IF ( PTR_SC(IHIT).GT.0 ) THEN
          STATWORD=IQ(PTR_SC(IHIT)+9)
          CALL MVBITS(MARKER,0,8,STATWORD,STARTBIT)
          CALL MVBITS(LR(IHIT),0,1,STATWORD,24)
          IQ(PTR_SC(IHIT)+9)=STATWORD
        ENDIF
C
        IF ( PTR_DA(IHIT).GT.0 ) THEN
          STATWORD=IQ(PTR_DA(IHIT)+8)
          CALL MVBITS(MARKER,0,8,STATWORD,STARTBIT)
          CALL MVBITS(LR(IHIT),0,1,STATWORD,24)
          IQ(PTR_DA(IHIT)+8)=STATWORD
          IF ( UNIT.EQ.0 .AND. WIRE.EQ.0 ) THEN
            IF( PTR_DA(NHITS+1).NE.0) THEN
              STATWORD=IQ(PTR_DA(NHITS+1)+8)
              CALL MVBITS(MARKER,0,8,STATWORD,STARTBIT)
              CALL MVBITS(LR(IHIT),0,1,STATWORD,24)
              IQ(PTR_DA(NHITS+1)+8)=STATWORD
            ENDIF
            IF( PTR_DA(NHITS+2).NE.0) THEN
              STATWORD=IQ(PTR_DA(NHITS+2)+8)
              CALL MVBITS(MARKER,0,8,STATWORD,STARTBIT)
              CALL MVBITS(LR(IHIT),0,1,STATWORD,24)
              IQ(PTR_DA(NHITS+2)+8)=STATWORD
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C  Done.
C
C----------------------------------------------------------------------
  999 RETURN
      END
