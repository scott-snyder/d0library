      SUBROUTINE PFGETD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the delay line hit information
C-
C-   Inputs  : none
C-   Outputs : fill the FDDELP.INC common with DL hit info
C-   Controls: 
C-
C-   Created  28-FEB-1989   Lupe Rosas & Jeffrey Bantly 
C-   Updated   7-FEB-1990   Jeffrey Bantly  remove temp fixes 
C-   Updated  30-APR-1991   Jeffrey Bantly  only use DL hits on segments 
C-   Updated   4-OCT-1991   Robert E. Avery  Use FHIT bank if necessary. 
C-              Change structure of FDDELP common.
C-   Updated  25-JAN-1992   Robert E. Avery  Only need to fill arrays once 
C-              per event.
C-   Updated  18-FEB-1992   Lupe Howell  Remove machine block 
C-   Updated  20-OCT-1993   Robert E. Avery   Allow for new version of 
C-                                              FHIT bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDDELP.INC/LIST'
C
      INTEGER RUN,ID
      INTEGER RUNSAV,IDSAV
      INTEGER LWIRE0, LHIT0, I
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER STATWORD,JHIT,IHIT
      INTEGER GZFTSC,LFTSC
      INTEGER IQFHIT(2),NHIT
      INTEGER LFHIT ,GZFHIT
      INTEGER NSECT 
      PARAMETER( NSECT = (MXUNIT+1)*(MXQUAD+1)*(MXSECT+1) )
      LOGICAL VERSION1, GOOD_DL 
      LOGICAL DL
      INTEGER LR
      LOGICAL ON_SEG
      INTEGER TRK_FDCT,TRK_ZTRK
      REAL    DRIFTD, DRIFTD_M
      REAL    Z_POS
      REAL    IONIZATION
C
      LOGICAL FTSC_FOUND 
C----------------------------------------------------------------------
C
C  Only fill arrays once per event.
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 999
      ENDIF
C      
      CALL VZERO(N_DL_HITS,NSECT)
C
C  Look for FHIT banks first
C
      CALL GTFHIT(0,NHIT)
      IF ( NHIT .GT. 0 ) THEN
C
        LFHIT = GZFHIT()
        VERSION1 = IQ(LFHIT+1).EQ.1
C
        DO IHIT =  1, NHIT
          CALL GTFHIT(IHIT,IQFHIT)
          STATWORD = IQFHIT(1)
C
C  Require good Delay Line and on segment.
C
          IF ( VERSION1 ) THEN
            GOOD_DL = .NOT.BTEST(STATWORD,10)   ! Unit = 0
     &               .AND. BTEST(STATWORD,3)    ! DL hit
     &               .AND. BTEST(STATWORD,14)   ! On segment
          ELSE
            GOOD_DL = BTEST(STATWORD,13)        ! Associated DL hit
     &          .AND. BTEST(STATWORD,14)        ! On segment
          ENDIF
          IF ( GOOD_DL ) THEN
C
            CALL FHIT_DECODE( 
     &        IQFHIT,
     &        HALF,UNIT,QUAD,SECTOR,WIRE,
     &        DL,LR,ON_SEG,TRK_FDCT,TRK_ZTRK,
     &        DRIFTD, DRIFTD_M, Z_POS, IONIZATION )
C
            N_DL_HITS(HALF,QUAD,SECTOR) =
     &        N_DL_HITS(HALF,QUAD,SECTOR) + 1
            JHIT = N_DL_HITS(HALF,QUAD,SECTOR) 
C
            DRIFT_DIST(HALF,QUAD,SECTOR,JHIT) = DRIFTD 
            DL_DIST(HALF,QUAD,SECTOR,JHIT) = Z_POS
            ON_TRK(HALF,QUAD,SECTOR,JHIT) = TRK_FDCT.GT.0
          ENDIF
        ENDDO
        GOTO 999
      ENDIF
C
C  Else look for FTSC banks 
C
      DO HALF = 0,1
        DO QUAD = 0,7
          DO SECTOR = 0,5
            JHIT = 0
            LFTSC = GZFTSC(HALF,QUAD,SECTOR)
            IF(LFTSC.GT.5) THEN
              LWIRE0 = LFTSC+4+IQ(LFTSC+2)
              DO IHIT = 1,IQ(LFTSC+4)
                LHIT0 = LFTSC+IQ(LWIRE0)+IQ(LFTSC+3)*(IHIT-1)-1
                STATWORD = IQ(LHIT0+9)
                IF( BTEST(STATWORD,2) ) THEN
                  IF ( IAND(STATWORD,3) .GT. 0 ) THEN
                    CALL MVBITS(STATWORD,24,1,LR,0)
                    IF (LR.GE.0) THEN
                      JHIT = JHIT + 1
                      DRIFT_DIST(HALF,QUAD,SECTOR,JHIT) = Q(LHIT0+2+LR)
                      DL_DIST(HALF,QUAD,SECTOR,JHIT) = Q(LHIT0+4)
                      CALL MVBITS(STATWORD,16,8,TRK_FDCT,0)
                      ON_TRK(HALF,QUAD,SECTOR,JHIT) = TRK_FDCT.GT.0
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
              N_DL_HITS(HALF,QUAD,SECTOR) = JHIT
            ELSE
              N_DL_HITS(HALF,QUAD,SECTOR) = 0
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
