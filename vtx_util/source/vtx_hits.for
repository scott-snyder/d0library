      SUBROUTINE VTX_HITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Full hit processing for the VTX.  Either find hits
C-                         starting from raw data (CDD1), or allow the use of
C-                         the compressed hits bank VCHT.
C-                      
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  31-OCT-1993   Peter Grudberg
C-   Updated  20-FEB-1994   Liang-Ping Chen
C-                          redefine RAW_EXISTS=.TRUE. if CDD1 or CDH1 exists
C-   Updated  11-MAR-1994   liang-ping Chen  REDOCD supersedes REDOVTX
C-   Updated  15-AUG-1994   Justin Bendich
C-                          don't drop VTXH; just drop almost everything else
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      INTEGER NDrop
      PARAMETER (NDrop = 6)
      INTEGER RUN, EVT, RUNSAV, EVTSAV
      INTEGER IER
      INTEGER LCDD1, LVCHT, GZVCHT, STATWORD, FULLHIT_BIT
      INTEGER LCDH1, GZCDH1
      PARAMETER ( FULLHIT_BIT = 31 )
      INTEGER LVTXH, GZVTXH, LVTRH, GZVTRH, LVLAY, GZVLAY, Ptr, I
      INTEGER LAY, SEC, SECMAX(0:2), GZVWDA, LVWDA, ToDrop(NDrop), LRCP
      LOGICAL FIRST, EZERROR, REDOVTX, DROP_VLAY, DROP_VWDA
      LOGICAL REDOCD,RAW_EXISTS, FULLHIT_EXISTS, CREATE_VCHT, GARB
      DATA FIRST / .TRUE. /
      DATA RUNSAV, EVTSAV / -1, -1 /
      DATA SECMAX / 15, 31, 31 /
      DATA ToDrop /10,            ! VHIT
     &              9,            ! VCHT
     &              6,            ! VCAL
     &              3, 2, 1/      ! VLAY(2, 1, 0)
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('RCP error', 'VTX_HITS',
     &      'VTRAKS_RCP not found - default used','W')
          REDOVTX = .FALSE.
          DROP_VLAY = .FALSE.
          DROP_VWDA = .FALSE.
        ELSE
          CALL EZGET('REDOVTX',REDOVTX,IER)
          IF ( IER .NE. 0 ) REDOVTX = .FALSE.
          CALL EZGET('DROP_VLAY',DROP_VLAY,IER)
          IF ( IER .NE. 0 ) DROP_VLAY = .FALSE.
          CALL EZGET('DROP_VWDA',DROP_VWDA,IER)
          IF ( IER .NE. 0 ) DROP_VWDA = .FALSE.
        ENDIF
        CALL EZRSET
C
        CALL EZLOC('ZTRAKS_RCP',LRCP)
        IF (LRCP .NE. 0) THEN
          CALL EZPICK('ZTRAKS_RCP')
          CALL EZGET('REDOCD',REDOCD,IER)
          IF (IER.EQ.0) THEN
             REDOVTX = REDOCD
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
C ****  Has this event already been processed?
C
      CALL EVNTID(RUN,EVT)
      IF ( RUN .NE. RUNSAV .OR. EVT .NE. EVTSAV ) THEN
        RUNSAV = RUN
        EVTSAV = EVT
      ELSE
        GO TO 999
      ENDIF
C
C ****  Find out what exists
C
      LCDD1 = LQ(LHEAD - IZCDD1)

      LCDH1 = GZCDH1()
      RAW_EXISTS = LCDD1 .GT. 0 .OR. LCDH1.GT. 0
C
      FULLHIT_EXISTS = .FALSE.
      LVCHT = GZVCHT()
      IF ( LVCHT .GT. 0 ) THEN
        STATWORD = IQ(LVCHT+5)
        FULLHIT_EXISTS = BTEST(STATWORD,FULLHIT_BIT)
      ENDIF
C
C ****  Next - decide what needs to be done
C
      IF ( FULLHIT_EXISTS .AND. .NOT.REDOVTX ) GO TO 999 ! Nothing to do
C
      CREATE_VCHT = .FALSE.
      IF ( RAW_EXISTS ) THEN
        IF ( REDOVTX .OR. .NOT.FULLHIT_EXISTS ) CREATE_VCHT = .TRUE.
      ELSEIF ( .NOT. FULLHIT_EXISTS ) THEN
        CALL ERRMSG('VTX-NO-DATA','VTX_HITS',
     &    'Cannot fully reconstruct VTX','W')
      ENDIF
C
      IF ( CREATE_VCHT ) THEN
C
C ****  Do hitfinding from raw data; first, drop any old banks
C
        GARB = .FALSE.
        LVTXH = GZVTXH()
        IF ( LVTXH .GT. 0 ) THEN
C
C ****  Drop all banks other than BMXY hanging from VTXH
C
          DO 100 I = 1, NDrop
            Ptr = LQ(LVTXH - ToDrop(I))
            IF(Ptr .GT. 0) CALL MZDROP(IXCOM, Ptr, ' ')
  100     CONTINUE
          GARB = .TRUE.
          CALL VTXLNK_CLR(1)  ! Make sure arrays in VTXLNK are cleared
C
C ****  Clear VTXH hit counters and hitfinding status bits
C
          IQ(LVTXH+1)  = 0
          IQ(LVTXH+2)  = 0
          IQ(LVTXH+10) = 0
          IQ(LVTXH+11) = 0
          IQ(LVTXH+12) = 0
        ENDIF
        LVTRH = GZVTRH()
        IF ( LVTRH .GT. 0 ) THEN
          CALL MZDROP(IXCOM,LVTRH,' ')
          GARB = .TRUE.
        ENDIF
        IF ( GARB ) CALL MZGARB(IXMAIN,0)
C
        CALL VTX_DYNADJ
        CALL VTHITS
C
C ****  Transfer hit data to compressed hits bank VCHT, and set bit to indicate
C ****  full hitfinding
C
        CALL VWDA_TO_VCHT
        LVCHT = GZVCHT()
        IF ( LVCHT .GT. 0 ) 
     &    IQ(LVCHT+5) = IBSET(IQ(LVCHT+5),FULLHIT_BIT)
C
C ****  If requested, drop the full-size hit banks to save memory (they will be
C ****  recreated on an as-needed basis later in VRHITS).
C
        IF ( DROP_VLAY .OR. DROP_VWDA ) THEN
          IF ( DROP_VLAY ) THEN   ! Clear VTXH hit counters
            LVTXH = GZVTXH()
            IQ(LVTXH+1) = 0
            IQ(LVTXH+2) = 0
          ENDIF
          DO LAY = 0, 2
            IF ( DROP_VLAY ) THEN ! Clear hitfinding status word
              LVTXH = GZVTXH()
              IQ(LVTXH+10+LAY) = 0
              LVLAY = GZVLAY(LAY)
              IF ( LVLAY .GT. 0 ) CALL MZDROP(IXCOM,LVLAY,' ')
            ELSE
              DO SEC = 0, SECMAX(LAY)
                LVWDA = GZVWDA(LAY,SEC)
                IF ( LVWDA .GT. 0 ) CALL MZDROP(IXCOM,LVWDA,' ')
              ENDDO
            ENDIF
          ENDDO
          IF ( DROP_VLAY ) THEN
            CALL VTXLNK_CLR(1)  ! Clear LVLAY and below in VTXLNK
          ELSE
            CALL VTXLNK_CLR(3)  ! Clear LVWDA in VTXLNK
          ENDIF
        ENDIF
      ELSEIF ( REDOVTX ) THEN
C
C ****  Use VCHT bank for reconstruction (VSEC banks will be produced on an
C ****  as-needed basis in VRHITS).  Drop any existing VLAY banks, as well as
C ****  the VTX track structure (if it exists)
C
        GARB = .FALSE.
        DO LAY = 0, 2
          LVLAY = GZVLAY(LAY)
          IF ( LVLAY .GT. 0 ) THEN
            CALL MZDROP(IXCOM,LVLAY,' ')
            GARB = .TRUE.
          ENDIF
        ENDDO
C
C ****  Clear VTXH hit counters and hitfinding status bits
C
        LVTXH = GZVTXH()
        IQ(LVTXH+1)  = 0
        IQ(LVTXH+2)  = 0
        IQ(LVTXH+10) = 0
        IQ(LVTXH+11) = 0
        IQ(LVTXH+12) = 0
C
        CALL VTXLNK_CLR(1)  ! Make sure VTXLNK area is cleared
C
        LVTRH = GZVTRH()
        IF ( LVTRH .GT. 0 ) THEN
          CALL MZDROP(IXCOM,LVTRH,' ')
          GARB = .TRUE.
        ENDIF
C
        IF ( GARB ) CALL MZGARB(IXMAIN,0)
C
      ENDIF
C
  999 RETURN
      END
