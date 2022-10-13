      FUNCTION MTCEVT()
C----------------------------------------------------------------------
C- MTCEVT: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Event processing routine for the package MTC.
C-   If FULL_MTC is .true. then this program will
C-   find all tracklike objects in the calorimeter emerging from the
C-   vertex with
C-      initial scanning MTC_ETAMIN .le.abs(eta).le. MTC_ETAMAX and
C-      scanning hadronic fraction of layers hit .gt. MTC_HFRACSCAN and
C-      final hadronic fraction of layers hit .gt. MTC_HFRACFND and
C-      final total fraction of layers hit .gt. MTC_FRACFND.
C-      MUcal finding status messages are printed if MTC_IPSTATUS=0.
C-
C-   Flag IMTC_IERROR in MTC.INC indicates final cal track finding status
C-   It is = 0 if mtc track finding successful
C-         = -1 if track finding unsuccessful
C-         = +1 if too many tracks were found
C-
C-   Outputs : block /MTC_FIND/ and bank MTCF filled
C-
C-   Created   7-MAR-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MTCEVT
C-
      INCLUDE 'D0$INC:MTC_FIND.INC'
C- vertex
      INTEGER NVERT
      REAL    ZVERT(10), DZVERT(10)
C- local
      INTEGER IER,IPSTATUS
      REAL    ETAMIN,ETAMAX,HFRACSCAN,HFRACFND,FRACFND
C- save the values of these variables
      INTEGER IFIRST
      LOGICAL FULL_MTC
      DATA    FULL_MTC/.TRUE./
      DATA    IFIRST/0/
C----------------------------------------------------------------------
      MTCEVT = .TRUE.      
C----------------------------------------------------------------------
C- read MTC.RCP
      IF (IFIRST.EQ.0) THEN
        IFIRST=1
        CALL INRCP('mtc_rcp',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('MTC_RCP')
          CALL EZGET_l('FULL_MTC',     FULL_MTC, IER)
          CALL EZGET('MTC_ETAMIN',   ETAMIN,   IER)
          CALL EZGET('MTC_ETAMAX',   ETAMAX,   IER)
          CALL EZGET('MTC_HFRACSCAN',HFRACSCAN,IER)
          CALL EZGET('MTC_HFRACFND', HFRACFND, IER)
          CALL EZGET('MTC_FRACFND',  FRACFND,  IER)
          CALL EZGET_i('MTC_IPSTATUS', IPSTATUS, IER)
          CALL EZRSET
C- store the mtc finding parameters in MTC_FIND.INC
          IMTC_FULL      = 0
          if(full_mtc) IMTC_FULL = -1
          XMTC_ETAMIN    = ETAMIN
          XMTC_ETAMAX    = ETAMAX
          XMTC_HFRACSCAN = HFRACSCAN
          XMTC_HFRACFND  = HFRACFND
          XMTC_FRACFND   = FRACFND
          IMTC_IPSTATUS  = IPSTATUS
        ELSE
          WRITE(6,*) ' MTCEVT error: no MTC_RCP found'
          FULL_MTC       = .TRUE.
          XMTC_ETAMIN    = 0.0
          XMTC_ETAMAX    = 5.0
          XMTC_HFRACSCAN = 0.57
          XMTC_HFRACFND  = 0.66
          XMTC_FRACFND   = 0.50
          IMTC_IPSTATUS  = -1
        END IF
      END IF
      IF(imtc_full.eq.0) GO TO 999
C----------------------------------------------------------------------
C- get vertex
      CALL ZVERTE( NVERT,ZVERT,DZVERT)
      XMTC_VTXFND(1) = 0.
      XMTC_VTXFND(2) = 0.
      XMTC_VTXFND(3) = ZVERT(1)           ! use the first zvertex found
      XMTC_DVTXFND(1) = 0.
      XMTC_DVTXFND(2) = 0.
      XMTC_DVTXFND(3) = DZVERT(1)         ! get the uncertainty in zvtx
C----------------------------------------------------------------------
      CALL MTC_MUCALFFIND
C----------------------------------------------------------------------
      CALL DHDIR('MTC_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('MTCEVT','MTCEVT',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
