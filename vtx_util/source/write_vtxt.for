      LOGICAL FUNCTION WRITE_VTXT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save selected VTXTs and their matched hits and the
C-   corresponding CDC/FDC track info.
C-
C-   NOTE: THIS ROUTINE ASSUMES A NON-STANDARD USAGE OF VTXT -- IN PARTICULAR,
C-         LQ(LVTXT-2) IS THE REFERENCE LINK TO THE DTRK OR FDCT THAT HAS BEEN
C-         MATCHED TO THE VTXT.  --  SEE SAVE_VTXT
C-
C-   Inputs  : none
C-   Outputs : file 'VTXCALIB' -- for each VTXT, the following info is written:
C-               WORD(1) = bits  0-15: Event number
C-                         bits 16-23: GFAC (gain factor due to P/T)
C-                         bits 24-31: DFAC (distance-time scale factor)
C-               WORD(2) = bits  0-31: dbl3 packed time of event
C-               WORD(3) = bits  0- 4: matched hits on VTXT
C-                         bits  5-14: Luminosity
C-                         bits 15-31: SPARE
C-               WORD(4) = bits  0- 9: theta of VTX track
C-                         bits 10-19: zcog of VTX track
C-                         bits 20-31: ZVTX (vertex from VERT bank)
C-               WORD(5) = bits  0-15: xcog of CDC/FDC track
C-                         bits 16-31: ycog of CDC/FDC track
C-               WORD(6) = bits  0-11: zcog of CDC/FDC track
C-                         bits 12-27: phi of CDC/FDC track
C-                         bits 28-31: SPARE
C-               WORD(7) = bits  0-11: theta of CDC/FDC track
C-                         bits 12-21: sig_theta of CDC/FDC track
C-                         bits 22-31: sig_phi of CDC/FDC track
C-               WORD(8) = bits  0-15: xcog of VTX track
C-                         bits 16-31: ycog of VTX track
C-               WORD(9) = bits  0-15: phi of VTX track
C-                         bits 16-23: sig_phi of VTX track
C-                         bits 24-31: sig_theta of VTX track
C-               WORD(10)= bits  0-31: SPARE
C-               ARRAY(1:5,0:HITS) = VTX matched hits on track info: see
C-                         PACK_VTX_HITS.FOR
C-   Controls: none
C-
C-   Created  16-DEC-1993   Ed Oltman
C-   Updated  21-FEB-1994   Ed Oltman  fix Justin's discovery
C-   Modified subsequently (March 1994) to open correct file in EXAMINE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
c Locals:
      INTEGER LVTXT,LTRAK,WORD(10),ARRAY(5,24),IVAL,ILUM,HITS,I,J,LUN
      INTEGER IUSER,ERR,lumtime,envtime, Run, Store, Status, Sequence
      INTEGER IDate, ITime, MinHits
      REAL    GFAC,DFAC,LUM,ZVTX,pabs,tdegc
      REAL    XZ,YZ,ZZ,PHIZ,THEZ,PHIZS,THEZS
      REAL    XV,YV,ZV,PHIV,THEV,PHIVS,THEVS
      CHARACTER*4 BANK
      CHARACTER*14 DateString
      CHARACTER*34 FileName
      CHARACTER*200 Space
      LOGICAL Closed, OpenError, First
c Externals:
      INTEGER GZVTXT,EVONUM, RUNNO, ISL
      LOGICAL TRNLNM, UserOut
c Data:
      DATA IUSER/315/, Closed/.TRUE./, OpenError/.FALSE./, First/.TRUE./
      DATA MinHits/6/
      COMMON /CALIB_FILE/ LUN, Closed, IUSER, UserOut, Store, DateString
      COMMON /SLATE/ ISL(6)
   10 FORMAT('VTXCALIB_',I6.6,'_',I4.4,'_',I1,'.DAT')
   20 FORMAT('USR$OUT:VTXCALIB_',I6.6,'_',I4.4,'_',I1,'.DAT')
   30 FORMAT(I2.2,'/',I2.2,'/',I2.2,I3.2,':',I2.2)
C----------------------------------------------------------------------
      WRITE_VTXT = .TRUE.
      IF(OpenError) GOTO 999
      IF(Closed) THEN
        UserOut = TRNLNM('USR$OUT', Space, I)
        Run = RUNNO()
        Store = 0
        CALL DATIME(IDate, ITime)         ! Writes into COMMON /SLATE/
        WRITE(DateString,30) ISL(2),ISL(3),MOD(ISL(1),100),ISL(4),ISL(5)
        Sequence = 1
        IF(First) THEN
          First = .FALSE.
          CALL EZPICK('VTRAKS_RCP')
          CALL EZGET('MIN_HITS', MinHits, Err)
          CALL EZRSET
          CALL GTUNIT(IUSER,LUN,ERR)
          IF(ERR .NE. 0) THEN
            CALL ERRMSG('GTUNIT', 'WRITE_VTXT',
     &        'failed; VTXCALIB file will not be written', 'W')
            GOTO 1000
          ENDIF
        ENDIF
        OPEN(LUN, FILE='ONLINE:[COOR_EXEC]CURRENT_ACCEL_STORE.DAT',
     &       STATUS='OLD', READONLY, IOSTAT=Status)
        IF(Status .EQ. 0) THEN
          READ(LUN,*) Store
          CLOSE(LUN)
        ENDIF
        IF(Store .EQ. 0) CALL ERRMSG('Open','WRITE_VTXT',
     &    'Couldn''t get store from COOR', 'W')
        Status = 0
        DO 100 WHILE(Status .EQ. 0)
          IF(UserOut) THEN
            WRITE(FileName,20) Run, Store, Sequence
          ELSE
            WRITE(FileName,10) Run, Store, Sequence
          ENDIF
          OPEN(LUN,FILE=FileName,STATUS='OLD',READONLY,
     &         IOSTAT=Status)
          IF(Status .EQ. 0) THEN
            CLOSE(LUN)
            Sequence = Sequence + 1
          ENDIF
  100   CONTINUE
        OPEN(LUN, FILE=FileName, STATUS='NEW', FORM='UNFORMATTED',
     &       IOSTAT=Status)
        IF(Status .NE. 0) THEN
          CALL ERRMSG('Open', 'WRITE_VTXT',
     &      'failed; CALIB file will not be written', 'W')
          GOTO 1000
        ENDIF
        Closed = .FALSE.
      ENDIF
C
C ****  FIRST GET STUFF FOR EVENT
C
      CALL VTX_GETGFAC(GFAC)
      CALL VTX_GETDFAC(DFAC)
      CALL vdynget(lum,pabs,tdegc,lumtime,envtime)
C-->WORD 1: EVENT(0:15) | GFAC(16:23) | DFAC(24:31)
      WORD(1) = EVONUM()
      IVAL = MIN0(2**8- 1,MAX0(0,NINT( (GFAC-0.5)*250. )))
      CALL MVBITS(IVAL,0,8,WORD(1),16)
      IVAL = MIN0(2**8- 1,MAX0(0,NINT( (DFAC-0.8)*600. )))
      CALL MVBITS(IVAL,0,8,WORD(1),24)
C-->WORD 2: DBL3 PACKED TIME
      CALL D3UPT(Q(LHEAD+4),WORD(2))
      ILUM = MIN0(2**11-1,MAX0(0,NINT( LUM*100.)))
C
C ****  LOOP OVER TRACKS..
C
      LVTXT = GZVTXT(0)
      DO WHILE (LVTXT .GT. 0)
        LTRAK = LQ(LVTXT-2)
        IF (LTRAK .EQ. 0) THEN
          WRITE(*,*) ' WRITE_VTXT: LTRAK = 0!!'
          STOP
        ENDIF
        CALL UHTOC(IQ(LTRAK-4),4,BANK,4)
        IF (BANK .EQ. 'DTRK') THEN
          XZ   = Q(LTRAK+7)
          YZ   = Q(LTRAK+8)
          ZZ   = Q(LTRAK+11)
          PHIZ = Q(LTRAK+6)
          THEZ = Q(LTRAK+9)
          PHIZS= Q(LTRAK+16)
          THEZS= Q(LTRAK+18)
        ELSE
          XZ   = Q(LTRAK+4)
          YZ   = Q(LTRAK+5)
          CALL FGETZ0(IQ(LTRAK-5),ZZ)
          PHIZ = Q(LTRAK+6)
          THEZ = Q(LTRAK+22)
          PHIZS= Q(LTRAK+23)
          THEZS= Q(LTRAK+24)
        ENDIF
        XV   = Q(LVTXT+7)
        YV   = Q(LVTXT+8)
        ZV   = Q(LVTXT+11)
        PHIV = Q(LVTXT+6)
        THEV = Q(LVTXT+9)
        PHIVS= Q(LVTXT+16)
        THEVS= Q(LVTXT+18)
        ZVTX = Q(LVTXT+15)
C-->WORD(4)
        WORD(4) = MIN0(2**10-1,MAX0(0,NINT( THEV*300.      )))
        IVAL    = MIN0(2**10-1,MAX0(0,NINT( (ZV+60.)*8.    )))
        CALL MVBITS(IVAL,0,10,WORD(4),10)
        IVAL    = MIN0(2**12-1,MAX0(0,NINT( (ZVTX+200.)*10.)))
        CALL MVBITS(IVAL,0,12,WORD(4),20)
C-->WORD(5)
        WORD(5) = MIN0(2**16-1,MAX0(0,NINT( (XZ+100.)*300. )))
        IVAL    = MIN0(2**16-1,MAX0(0,NINT( (YZ+100.)*300. )))
        CALL MVBITS(IVAL,0,16,WORD(5),16)
C-->WORD(6)
        WORD(6) = MIN0(2**12-1,MAX0(0,NINT( (ZZ+200.)*10.  )))
        IVAL    = MIN0(2**16-1,MAX0(0,NINT( PHIZ*10000.    )))
        CALL MVBITS(IVAL,0,16,WORD(6),12)
C-->WORD(7)
        WORD(7) = MIN0(2**12-1,MAX0(0,NINT( THEZ*1000.     )))
        IVAL    = MIN0(2**10-1,MAX0(0,NINT( THEZS*5000.    )))
        CALL MVBITS(IVAL,0,10,WORD(7),12)
        IVAL    = MIN0(2**10-1,MAX0(0,NINT( PHIZS*5000.    )))
        CALL MVBITS(IVAL,0,10,WORD(7),22)
C-->WORD(8)
        WORD(8) = MIN0(2**16-1,MAX0(0,NINT( (XV+20.)*1500. )))
        IVAL    = MIN0(2**16-1,MAX0(0,NINT( (YV+20.)*1500. )))
        CALL MVBITS(IVAL,0,16,WORD(8),16)
C-->WORD(9)
        WORD(9) = MIN0(2**16-1,MAX0(0,NINT(  PHIV*10000.   )))
        IVAL    = MIN0(2**8 -1,MAX0(0,NINT(  PHIVS*25000.  )))
        CALL MVBITS(IVAL,0, 8,WORD(9),16)
        IVAL    = MIN0(2**8 -1,MAX0(0,NINT(  THEVS*250.    )))
        CALL MVBITS(IVAL,0, 8,WORD(9),24)
C-->WORD(10) SPARE
        WORD(10) = 0
C
C ****  NOW FILL THE VTX HIT INFO
C
        CALL VTX_PACK_HITS(LVTXT,HITS,ARRAY)
C-->WORD(3)
        WORD(3) = HITS
        CALL MVBITS(ILUM,0,11,WORD(3),5)
C
C ****  AND FILL WRITEOUT THE RECORD
C
        WRITE_VTXT = HITS .GE. MinHits
        IF (WRITE_VTXT) THEN
          WRITE(LUN) WORD
          WRITE(LUN) ((ARRAY(I,J),I=1,5),J=1,HITS)
        ENDIF
        LVTXT= LQ(LVTXT)
      ENDDO
  999 RETURN
 1000 OpenError = .TRUE.
      END
