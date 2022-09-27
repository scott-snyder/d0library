      FUNCTION XYVERT_EVT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill Ntuples with X and Y of vertex
C-
C-   Returned value  : .TRUE.
C-   Inputs  : Z of vertex, VTX segments and tracks
C-   Outputs : X and Y of vertex
C-
C-   Created  05-OCT-1992   Alexandre Zinchenko
C-   Updated  13-JAN-1993   A. Zinchenko - use flag for segments from 
C-                                         tracks or all segments
C-   Updated  01-MAR-1993   A. Zinchenko - keep in Ntuple number of 
C-                                         vertices
C-   Updated  26-MAR-1993   A. Zinchenko - check Ntuple existence
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:XYVCOM.INC/LIST'
      LOGICAL XYVERT_EVT, FIRST, IDOVTX, VERTXZ, OK, VTRAKS
      LOGICAL EZERROR, FLUSED, HEXIST, XYVERT_PAR
      INTEGER LVERT, GZVERT, IUSED, LOC, STAT, LZFIND, LAY, ITRA
      INTEGER NTRAK, LVTRH, GZVTRH, NSEGML, LSEGM, NZBANK, ISEG
      INTEGER IER, NSTAMX, NVERT, IVERT, NVMAX, NSTAT(0:3)
      INTEGER NPOINT
      PARAMETER (NVMAX = 5)
      REAL XDATA(20), ZVERT(NVMAX), DZVERT(NVMAX), CONT(31), QTRAK(21)
      REAL QHSEC(4,24), QHZLA(3,6)
      EQUIVALENCE (QTRAK(2), NPOINT)
      DATA FIRST /.TRUE./, NSTAT /4*0/
C----------------------------------------------------------------------
      XYVERT_EVT = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('XYVERT','XYVERT_EVT',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('IUSED',IUSED,IER) ! define "USED" bit for segments
        CALL EZRSET
        CALL EZPICK('XYVERT_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('XYVERT','XYVERT_EVT',
     &    'Unable to find bank XYVERT_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VERTEX_Z_EXIST',VERTXZ,IER)
        CALL EZGET_i('NSTAMX',NSTAMX,IER)
        CALL EZGET('DO_VTX_TRACK',IDOVTX,IER)
        CALL EZGET('USED_SEGMENT',FLUSED,IER)
        IF (IER.NE.0) CALL ERRMSG(' Error reading RCP-file',
     &      'XYVERT_EVT',' Exit','F')
        CALL EZRSET
      ENDIF
C
C ****  Do all processing
C
      ZVERT(1) = 0.
      NVERT = 1
      IF (VERTXZ) THEN ! vertex Z has been reconstructed already
        CALL ZVERTE(NVERT,ZVERT,DZVERT)
      ENDIF
C
C ****  Reconstruct VTX segments and tracks
C
      IF (IDOVTX) OK = VTRAKS()
C
      CALL HCDIR('//AAABBB',' ')
      CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
C ****  Check, if Ntuples are booked
      IF (.NOT.HEXIST(4)) THEN
        OK = XYVERT_PAR()
      ENDIF
C
      XDATA(6) = NVERT
      DO 10 IVERT = 1, 1
        XDATA(5) = ZVERT(IVERT)
C
C ****  Loop over all VTX segments
C
        DO 20 LAY = 0,2
          LVTRH = GZVTRH()
          LSEGM = LQ(LVTRH-3-LAY)
          NSEGML = NZBANK(0,LSEGM)
          IF (NSEGML.EQ.0) GO TO 20
          DO 30 ISEG = 1, NSEGML
            LVTRH = GZVTRH()
            LSEGM = LQ(LVTRH-3-LAY)
            LOC = LZFIND(0,LSEGM,ISEG,-5)
            STAT = IQ(LOC)
            IF (FLUSED.AND.(.NOT.BTEST(STAT,IUSED))) GO TO 30 ! use 
                                                    ! only segments from track
            CALL GTSEGM(LAY,ISEG,CONT)
            CALL XYFIND(0,CONT(20),XDATA)
            IF (NSTAT(LAY).LT.NSTAMX) THEN
              CALL HFN(LAY+1,XDATA) ! fill Ntuple
              NSTAT(LAY) = NSTAT(LAY) + 1
            ENDIF
   30     CONTINUE
   20   CONTINUE
C
C ****  Loop over VTX tracks
C
        LVTRH = GZVTRH()
        NTRAK = IQ(LVTRH+2)
        IF (NTRAK.EQ.0) GO TO 999
        DO 40 ITRA = 1, NTRAK
          CALL GTVTXT(ITRA,QTRAK,QHSEC,QHZLA)
          CALL XYFIND(1,QTRAK(6),XDATA)
          XDATA(3) = XDATA(3)/(NPOINT-2)
          IF (NSTAT(3).LT.NSTAMX) THEN
            CALL HFN(4,XDATA) ! fill Ntuple
            NSTAT(3) = NSTAT(3) + 1
          ENDIF
   40   CONTINUE
   10 CONTINUE
      CALL HCDIR('//PAWC',' ')
C-----------------------------------------------------------------------
  999 RETURN
      END
