      SUBROUTINE VSTP_FETCH(CRUN,CALTYPE,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Processed STP structures from DBL3 into
C-                         memory
C-
C-   Inputs  : CRUN = Run number
C-             CALTYPE = what calibration type to process
C-   Input/Output : DB_OPENED: indicates if database has been opened yet
C-   Controls: returns IOK = .true. if all goes well
C-
C-   Created  14-OCT-1992   Srini Rajagopalan
C-   Updated  19-OCT-1992   Peter M. Grudberg  Change arguments, handle VDTM
C-   Updated  28-JAN-1993   Liang-ping Chen (P.G) ZSHUNT chains of VDTM 
C-   Updated  21-DEC-1993   Ed Oltman  fix DBCLB_FETCH_OFFLINE error detection  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
C
      INTEGER CRUN
      INTEGER LENGTH,IER
      INTEGER LDATA,LKEY
      INTEGER LAYER, SECTOR, NITEMS, NWIRES, NSECTS
      INTEGER GZVPDH, GZVGNH, GZVTMH, LVDTM, OFFSET
      INTEGER LVTMH_OLD, LVTMW, LVTMW_OLD
      INTEGER TYPE, LORUN(3), HIRUN(3)
C
      CHARACTER*25 PATH
      CHARACTER*80 DBCALIB
      CHARACTER*(*) CALTYPE
C
      LOGICAL IOK,DB_OPENED
      LOGICAL FIRST,BYPASS_DBL3_ERROR
      CHARACTER*1 SEVER
C
      SAVE FIRST, LORUN, HIRUN
C
      DATA FIRST /.TRUE./
      DATA LORUN / 3*999999 /
      DATA HIRUN / 3*-1 /
C----------------------------------------------------------------------
C
C  init...
C
      IOK = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGETS('DBCALIB$VTX',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
      ENDIF
C
C  Check top level STP bank
C
      IF (LSVTX.LE.0) THEN
        CALL ERRMSG('VTRAKS','VSTP_FETCH',
     &    'VTX constants header bank does not exist',SEVER)
        IOK = .FALSE.
        GO TO 999
      ENDIF
C
C  Open DBL3 database if not already opened (only if necessary!)
C
      IF ( CALTYPE(1:8) .EQ. 'PEDESTAL' ) TYPE = 1
      IF ( CALTYPE(1:5) .EQ. 'GAINS' ) TYPE = 2
      IF ( CALTYPE(1:5) .EQ. 'TIMES' ) TYPE = 3
      IF ( CRUN .GE. LORUN(TYPE) .AND. 
     &     CRUN .LE. HIRUN(TYPE) ) GO TO 999 ! No need to open DB
      IF (.NOT.DB_OPENED) THEN
        CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
        IF (.NOT.IOK) THEN
          CALL ERRMSG('VTRAKS','VSTP_FETCH',
     &      'Error initializing DBL3 database',SEVER)
          GO TO 999
        ELSE
          DB_OPENED = .TRUE.
        ENDIF
      ENDIF
C
C  Check if Pedestals have been requested, If so read structure under SVTX
C
      IF ( CALTYPE(1:8) .EQ. 'PEDESTAL' ) THEN
        TYPE = 1
        CALL INTMSG('Reading VTX PEDS from DBL3 - offline path')
        CALL DBCLB_PATH('PROC_PEDESTALS','VTX',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('VTRAKS','VSTP_FETCH',
     &      'Error fetching Processed pedestals from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(TYPE) = IC(LKEY + 3) 
        HIRUN(TYPE) = IC(LKEY + 4) 
C
C Check if VPDH already exists, If so drop structure.
C Shunt new structure under SVTX.
C
        LVPDH = GZVPDH()
        IF (LVPDH.GT.0) CALL MZDROP(IXSTP,LVPDH,' ')
        CALL ZSHUNT(IXSTP,LDATA,LSVTX,-IZVPDH,0)
        LVPDH = GZVPDH() ! Redefine value in ZEBSTP
      ENDIF
C
C  Check if Gains have been requested, If so read structure under SVTX
C
      IF ( CALTYPE(1:5) .EQ. 'GAINS' ) THEN
        TYPE = 2
        CALL INTMSG('Reading VTX GAINS from DBL3 - offline path')
        CALL DBCLB_PATH('PROC_GAINS','VTX',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('VTRAKS','VSTP_FETCH',
     &      'Error fetching Processed Gains from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(TYPE) = IC(LKEY + 3) 
        HIRUN(TYPE) = IC(LKEY + 4) 
C
C Check if VGNH already exists, If so drop structure.
C Shunt new structure under SVTX.
C
        LVGNH = GZVGNH()
        IF (LVGNH.GT.0) CALL MZDROP(IXSTP,LVGNH,' ')
        CALL ZSHUNT(IXSTP,LDATA,LSVTX,-IZVGNH,0)
        LVGNH = GZVGNH() ! Redefine value in ZEBSTP common
      ENDIF
C
C  Check if T0's have been requested, If so read structure under SVTX
C
      IF ( CALTYPE(1:5) .EQ. 'TIMES' ) THEN
        TYPE = 3
        CALL INTMSG('Reading VTX TIMES from DBL3 - offline path')
        CALL DBCLB_PATH('PROC_TIMES','VTX',PATH)
        CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
        IF (LDATA .EQ. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRMSG('VTRAKS','VSTP_FETCH',
     &      'Error fetching Processed Times from DBL3',SEVER)
          IOK = .FALSE.
          GO TO 999
        ENDIF
        LORUN(TYPE) = IC(LKEY + 3) 
        HIRUN(TYPE) = IC(LKEY + 4) 
C
C ****  Shunt the new VTMH structure under SVTX (making a linear structure of
C ****  VTMH banks, where the bank right under SVTX is the new VTMH, then shunt
C ****  the VDTM banks to hang under the new VTMW banks.  Copy the mapping
C ****  of sectors onto electrostatic categories from old to new VTMW banks.
C ****  Finally, drop the old VTMH structure.
C
        CALL ZSHUNT(IXSTP,LDATA,LSVTX,-IZVTMH,0)
        LVTMH = GZVTMH()
        IF ( LVTMH .GT. 0 ) LVTMH_OLD = LC(LVTMH)
        IF ( LVTMH .GT. 0 .AND. LVTMH_OLD .GT. 0 ) THEN
          DO LAYER = 0, 2
            LVTMW = LC(LVTMH-1-LAYER)
            LVTMW_OLD = LC(LVTMH_OLD-1-LAYER)
            NITEMS = IC(LVTMW+3)
            NWIRES = IC(LVTMW+4)
            NSECTS = IC(LVTMW+5)
            OFFSET = NITEMS*NWIRES*NSECTS + 6
            DO SECTOR = 0, NSECTS - 1
              IC(LVTMW+OFFSET+SECTOR) = IC(LVTMW_OLD+OFFSET+SECTOR)
              LVDTM = LC(LVTMW_OLD-1-SECTOR)
              IF ( LVDTM .GT. 0 ) 
     &          CALL ZSHUNT(IXSTP,LVDTM,LVTMW,-SECTOR-1,1)
            ENDDO
          ENDDO
          CALL MZDROP(IXSTP,LVTMH_OLD,' ')
          LVTMH = GZVTMH() ! Make sure link is correct in ZEBSTP
        ELSE
          CALL ERRMSG('VTMH bank missing','VSTP_FETCH',
     &      'VTMH structure missing from VTX STP structure',SEVER)
        ENDIF
      ENDIF
C
  999 RETURN
      END
