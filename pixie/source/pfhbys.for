      SUBROUTINE PFHBYS(UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays number of hits in sectors
C-
C-   Inputs  : none
C-   Outputs : display hits
C-   Controls:
C-
C-   Created  23-OCT-1989   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  add Hits bank check 
C-   Updated  29-APR-1991   Robert E. Avery  Use INTMSG, so can scroll. 
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South Halves 
C-   Updated  10-OCT-1991   Robert E. Avery  Get numbers from FHIT bank
C-                              if necessary. 
C-   Updated  20-OCT-1993   Robert E. Avery  New arguments for FHIT_GETMAPS.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,U,WIRE,UBIT
      INTEGER IQUAR,IBEG,IEND
      INTEGER LHALF,LTHET,LKFTQD,LKFXSC,LPHI,LKFDCH
      INTEGER GZFHLF,GZFDUN,GZFTQD,GZFTSC,GZFPSC,GZFXSC,GZFDCH
      INTEGER EVONUM,RUNNO
      INTEGER EVTNUM,RUNNUM,EVTNUM_LAST,RUNNUM_LAST 
      INTEGER NUM_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_PHI_HITS(0:MXSECP,0:MXHALF)
      INTEGER PTR_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_PHI_HITS(0:MXSECP,0:MXHALF)
      LOGICAL USE_FHITS 
C
      INTEGER IQFHIT(2)
C
      CHARACTER*80 FTEXT
C
C----------------------------------------------------------------------
C
      LKFDCH=GZFDCH()
      IF(LKFDCH.LE.5) THEN
        CALL INTMSG(' No FDC Hits banks present')
        GOTO 999
      ENDIF
      RUNNUM = RUNNO()
      EVTNUM = EVONUM()
C
C Get numbers from FHIT bank if necessary:
C 
      IF ( (RUNNUM.NE.RUNNUM_LAST) 
     &  .OR. (EVTNUM .NE. EVTNUM_LAST) ) THEN
C
        USE_FHITS = .FALSE.
        CALL GTFHIT(1,IQFHIT)
        IF ( IQFHIT(1) .NE. -1 ) THEN
          CALL FCODER(IQFHIT(1),HALF,U,QUAD,SECTOR,WIRE,UBIT,1)
          LKFXSC = GZFXSC(HALF,U,QUAD,SECTOR)
          IF ( LKFXSC.LT.5 ) THEN
            CALL FHIT_GETMAPS(
     &        NUM_TH_HITS,NUM_PHI_HITS,NUM_DL_HITS,
     &        PTR_TH_HITS,PTR_PHI_HITS,PTR_DL_HITS)
            USE_FHITS = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      RUNNUM_LAST = RUNNUM 
      EVTNUM_LAST = EVTNUM 
C
      CALL INTMSG(' ')
      WRITE(FTEXT,100) RUNNUM,EVTNUM 
  100 FORMAT(' FDC Hits for Run',I9,' Event',I5)
      CALL INTMSG(FTEXT)
C
      DO 10 HALF=0,1
        LHALF=GZFHLF(HALF)
        IF( LHALF .LE. 0 ) GOTO 10
        IF(UNIT.EQ.0) THEN
          LTHET=GZFDUN(HALF,UNIT)
          IF ( ( LTHET .LE. 0 ) .OR. ( IQ(LTHET+1) .LE. 0 ) ) THEN
            IF (HALF.EQ.0) WRITE(FTEXT,*) 'No Theta Hits in North FDC'
            IF (HALF.EQ.1) WRITE(FTEXT,*) 'No Theta Hits in South FDC'
            CALL INTMSG(FTEXT)
            GOTO 10
          ENDIF
          IF ( HALF .EQ. 0 ) THEN
            WRITE(FTEXT,*) ' HITS IN NORTH FDC IN THETA SECTORS -->',
     &        '   0     1     2     3     4     5'
          ELSE
            WRITE(FTEXT,*) ' HITS IN SOUTH FDC IN THETA SECTORS -->',
     &        '   0     1     2     3     4     5'
          ENDIF
          CALL INTMSG(FTEXT)
C
          DO 30 QUAD=0,7
            IF ( .NOT.USE_FHITS ) THEN
              LKFTQD=GZFTQD(HALF,QUAD)
              IF( LKFTQD .LE. 0 ) GOTO 30
              IF( IQ(LKFTQD+1) .LE. 0 ) GOTO 30
              DO 40 SECTOR=0,5
                LKFXSC=GZFTSC(HALF,QUAD,SECTOR)
                IF( LKFXSC .GT. 0 ) THEN
                  NUM_TH_HITS(SECTOR,QUAD,HALF) = IQ( LKFXSC+1 )
                ELSE
                  NUM_TH_HITS(SECTOR,QUAD,HALF) = 0
                ENDIF
   40         CONTINUE
              WRITE(FTEXT,102) QUAD,
     &          (NUM_TH_HITS(SECTOR,QUAD,HALF),SECTOR=0,5)
  102         FORMAT('      QUAD',I3,'  HITS BY SECTOR (0-5) = ',6I6)
              CALL INTMSG(FTEXT)
            ELSE
              WRITE(FTEXT,102) QUAD,
     &              (NUM_TH_HITS(SECTOR,QUAD,HALF),SECTOR=0,5)
              DO SECTOR=0,5
                IF ( NUM_TH_HITS(SECTOR,QUAD,HALF).GT.0 ) THEN
                  CALL INTMSG(FTEXT)
                  GOTO 30
                ENDIF
              ENDDO
            ENDIF
   30     CONTINUE
C
        ELSE
          LPHI=GZFDUN(HALF,UNIT)
          IF ( ( LPHI .LE. 0 ) .OR. ( IQ(LPHI+1) .LE. 0 ) ) THEN
            IF (HALF.EQ.0) WRITE(FTEXT,*) 'No Phi Hits in North FDC'
            IF (HALF.EQ.1) WRITE(FTEXT,*) 'No Phi Hits in South FDC'
            CALL INTMSG(FTEXT)
            GOTO 10
          ENDIF
          IF ( HALF .EQ. 0 ) THEN
            WRITE(FTEXT,*) ' HITS IN NORTH FDC IN PHI SECTORS',
     &        '    0+  1+  2+  3+  4+  5+  6+  7+  8+'
          ELSE
            WRITE(FTEXT,*) ' HITS IN SOUTH FDC IN PHI SECTORS',
     &        '    0+  1+  2+  3+  4+  5+  6+  7+  8+'
          ENDIF
          CALL INTMSG(FTEXT)
          IF ( .NOT.USE_FHITS ) THEN
            DO 50 SECTOR=0,35
              LKFXSC=GZFPSC(HALF,SECTOR)
              IF( LKFXSC .GT. 0 ) THEN
                NUM_PHI_HITS(SECTOR,HALF) = IQ( LKFXSC+1 )
              ELSE
                NUM_PHI_HITS(SECTOR,HALF) = 0
              ENDIF
   50       CONTINUE
          ENDIF
          DO 60 IQUAR=0,3
            IBEG=IQUAR*9
            IEND=IQUAR*9 + 8
            WRITE(FTEXT,104) IBEG,IEND,
     &        (NUM_PHI_HITS(SECTOR,HALF),SECTOR=IBEG,IEND)
  104       FORMAT('      HITS BY SECTOR (',I3,' to',I3,') = ',9I4)
            CALL INTMSG(FTEXT)
   60     CONTINUE
        ENDIF
   10 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
