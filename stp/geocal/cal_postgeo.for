      SUBROUTINE CAL_POSTGEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Calorimeter SRCP geometry files into
C-                         ZEBRA banks and write them out. The names of
C-                         the SRCP files to be read and other details
C-                         are read from a text file. This also handles
C-                         Level 0. The banks SRCP_UCAL, SRCP_ECAL and
C-                         SRCP_REST are hung below CGEH. The structure
C-                         from SCAL down is written to CAL_STPFILE.
C-                         Use CAISTP to read structure back into ZEBSTP.
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-OCT-1988   Harrison B. Prosper
C-   Updated  12-JAN-1989   Chip Stewart   LV0 ADDED
C-   Updated  15-JAN-1989   Harrison B. Prosper
C-                          Output directed to CAL_STPFILE.DAT
C-   Updated  10-MAR-1989   A.M.JONCKHEERE
C-                              Add READONLY to OPEN's
C-   Updated   7-APR-1989   Harrison B. Prosper, Steve Kahn
C-                          Add call to TOWGEO
C-   Updated   8-DEC-1989   Harrison B. Prosper
C-      Make compatible with new RCP
C-   Updated  20-SEP-1990   Chip Stewart
C-      Added CADT calorimeter address lookup table
C-   Updated  13-Feb-1992   Herbert
C-      Changed OPEN to D0OPEN
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ACTIVE
      INTEGER      I,J,K,L,M,N,II,JJ,NN,NMAX,WRDIDS,IZLINK(3)
      INTEGER      LUN,LUNLST,PRUNIT,ERROR,NBANKS,LSTPC,LCADT,ISTAT
      INTEGER      LCCPH,LCCPT,LCCPC,LCCUA
      PARAMETER( NMAX   = 10 )
      PARAMETER( LUNLST = 10 )
      PARAMETER( LUN    = 80 )
C
      LOGICAL DO_POSTGEO, DO_CALORIMETER, DO_LEVEL_ZERO
      LOGICAL DO_TOWGEO, DO_CAD_TABLE,DONE_PREGEO
      LOGICAL DO_CCPT_TABLE,DO_CCPC_TABLE,DO_CCUA_TABLE
      LOGICAL OK
C
      CHARACTER*32 BKNAME(0:NMAX)
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCUCL.LINK'
      INCLUDE 'D0$LINKS:IZCECL.LINK'
      INCLUDE 'D0$LINKS:IZCRST.LINK'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INCLUDE 'D0$LINKS:IZCCPH.LINK'
      INCLUDE 'D0$LINKS:IZCCPT.LINK'
      INCLUDE 'D0$LINKS:IZCCPC.LINK'
      INCLUDE 'D0$LINKS:IZCCUA.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      CALL INPAWC
C
C ****  Read control file - AFTER ZEBSTP MZWIPE in CAWSTP
C
      CALL INRCP ('CAWSTP_RCP',ERROR)
      IF(ERROR.NE.0) THEN
        CALL ERRMSG('NO CAWSTP_RCP FILE','CAL_POSTGEO','THUD','F')
      END IF
      CALL EZPICK('CAWSTP_RCP')
C
C ****  Get some switches from CONTROL bank
C
      CALL EZGSET ('PREGEO',DONE_PREGEO,1)
      CALL EZGSET ('POSTGEO',DO_POSTGEO,1)
      IF ( .NOT. DO_POSTGEO ) GOTO 999        ! Exit if POSTGEO switch not set
C
      CALL EZGSET ('CALORIMETER',DO_CALORIMETER,1)
      CALL EZGSET ('LEVEL_ZERO',DO_LEVEL_ZERO,1)
      CALL EZGSET ('TOWGEO',DO_TOWGEO,1)
      CALL EZGSET ('CAD_TABLE',DO_CAD_TABLE,1)
      CALL EZGSET ('CCPT_TABLE',DO_CCPT_TABLE,1)
      CALL EZGSET ('CCPC_TABLE',DO_CCPC_TABLE,1)
      CALL EZGSET ('CCUA_TABLE',DO_CCUA_TABLE,1)
      CALL EZGSET ('WORDS_PER_RECORD',WRDIDS,1)
      CALL EZRSET
C
C ****  Create SCAL bank
C
      CALL BKSCAL ('STPC',LSCAL)
C
C ****  Create CGEH bank
C
      CALL BKCGEH ('STPC',LCGEH)

      IF ( LCGEH .LE. 0 ) THEN
        WRITE (6,50) LCGEH
        GOTO 999
      ENDIF
C
      BKNAME(0) = 'CONTROL'
      BKNAME(1) = 'SRCP_UCAL'
      BKNAME(2) = 'SRCP_ECAL'
      BKNAME(3) = 'SRCP_REST'
      BKNAME(4) = 'SRCP_LV0'
      NBANKS = 4
C
      IZLINK(1) = IZCUCL        ! Link for SRCP_UCAL
      IZLINK(2) = IZCECL        ! Link for SRCP_ECAL
      IZLINK(3) = IZCRST        ! Link for SRCP_REST
C
C ****  BUILD SRCP BANKS
C
      IF ( DO_CALORIMETER ) THEN
C
C ****  DO CALORIMETER
C
        IF ( DO_TOWGEO  ) THEN
          DO 200 I =1,3
C
            WRITE (6,300) BKNAME(I)
C
            LSTPC = LC(LSTPH-IZSTPC)
            LSCAL = LC(LSTPC-IZSCAL)
            LCGEH = LC(LSCAL-IZCGEH)
C
            CALL D0OPEN (LUN,BKNAME(I),'I',OK)
            IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
            CALL EZREAD (LUN,BKNAME(I),WRDIDS,LCGEH,IZLINK(I))
            CLOSE(UNIT=LUN)
C
  200     CONTINUE
C
C ****  Create CELXYZ geometry banks
C
          CALL TOWGEO
          CALL CAL_SURVEY
C
C ****  Write out banks starting at SCAL
C
          CALL ZZOPEN (LUN,'CAL_STPFILE',ERROR,'OUTPUT')
          LSTPC = LC(LSTPH-IZSTPC)
          LSCAL = LC(LSTPC-IZSCAL)
          CALL FZOUT  (LUN,IDVSTP,LSCAL,1,' ',1,0,0)
          CALL ZZCLOS (LUN,ERROR,'OUTPUT')
        ENDIF
C
        IF ( DO_CAD_TABLE ) THEN
          CALL CADTFL(ISTAT)
C
C ****  Write out banks starting at CADT
C
          IF(ISTAT.EQ.0) THEN
            CALL ZZOPEN (LUN,'CAD_STPFILE',ERROR,'OUTPUT')
            LSTPC = LC(LSTPH-IZSTPC)
            LSCAL = LC(LSTPC-IZSCAL)
            LCGEH = LC(LSCAL-IZCGEH)
            LCADT = LC(LCGEH-IZCADT)
            CALL FZOUT  (LUN,IDVSTP,LCADT,1,'L',1,0,0)
            CALL ZZCLOS (LUN,ERROR,'OUTPUT')
          ENDIF
        ENDIF
        IF ( DO_CCPT_TABLE ) THEN
          CALL CCPTFL(ISTAT)
C
C ****  Write out banks starting at CCPT
C
          IF(ISTAT.EQ.0) THEN
            CALL ZZOPEN (LUN,'CCPT_STPFILE',ERROR,'OUTPUT')
            LSTPC = LC(LSTPH-IZSTPC)
            LSCAL = LC(LSTPC-IZSCAL)
            LCCPH = LC(LSCAL-IZCCPH)
            LCCPT = LC(LCCPH-IZCCPT)
            CALL FZOUT  (LUN,IDVSTP,LCCPT,1,'L',1,0,0)
            CALL ZZCLOS (LUN,ERROR,'OUTPUT')
          ENDIF
        ENDIF
        IF ( DO_CCPC_TABLE ) THEN
          CALL CCPCFL(ISTAT)
C
C ****  Write out banks starting at CCPC
C
          IF(ISTAT.EQ.0) THEN
            CALL ZZOPEN (LUN,'CCPC_STPFILE',ERROR,'OUTPUT')
            LSTPC = LC(LSTPH-IZSTPC)
            LSCAL = LC(LSTPC-IZSCAL)
            LCCPH = LC(LSCAL-IZCCPH)
            LCCPC = LC(LCCPH-IZCCPC)
            CALL FZOUT  (LUN,IDVSTP,LCCPC,1,'L',1,0,0)
            CALL ZZCLOS (LUN,ERROR,'OUTPUT')
          ENDIF
        ENDIF
        IF ( DO_CCUA_TABLE ) THEN
          CALL CCUAFL(ISTAT)
C
C ****  Write out banks starting at CCUA
C
          IF(ISTAT.EQ.0) THEN
            CALL ZZOPEN (LUN,'CCUA_STPFILE',ERROR,'OUTPUT')
            LSTPC = LC(LSTPH-IZSTPC)
            LSCAL = LC(LSTPC-IZSCAL)
            LCCPH = LC(LSCAL-IZCCPH)
            LCCUA = LC(LCCPH-IZCCUA)
            CALL FZOUT  (LUN,IDVSTP,LCCUA,1,'L',1,0,0)
            CALL ZZCLOS (LUN,ERROR,'OUTPUT')
          ENDIF
        ENDIF
      ENDIF

      IF ( DO_LEVEL_ZERO ) THEN
C
C ****  DO LEVEL ZERO
C
        I = 4
        WRITE (6,300) BKNAME(I)
C
        CALL D0OPEN (LUN,BKNAME(I),'I',OK)
        IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
        CALL EZREAD (LUN,BKNAME(I),WRDIDS,0,0)
        CLOSE(UNIT=LUN)
C
        CALL ZZOPEN (LUN,'LV0_STPFILE',ERROR,'OUTPUT')
        CALL EZOUT  (LUN,BKNAME(I))
        CALL ZZCLOS (LUN,ERROR,'OUTPUT')
C
      ENDIF
C
      CALL EZDBUG(6)
C
   50 FORMAT(1X,'ERROR creating CGEH: ',I10/)
  300 FORMAT(1X,'Building SRCP bank : ',A32/)
C
  999 RETURN
      END
