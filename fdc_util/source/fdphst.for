      SUBROUTINE FDPHST(IFIRST,PEAK,IPEV,FACTOR,FACTOR2,IPULWID,
     &                           PEDESTAL,EXPDAT,TEXT)
C----------------------------------------------------------------------
C-
C-  Description:  Make Topdrawer graphics displays
C-
C-   Created  10-DEC-1990   Jeffrey Bantly  for looking at pulse shapes
C-
C-   **NOTE : This routine is for use in testing purposes only and is not a
C-   **part of FTRAKS package.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER IFIRST,IPEV,EXPDAT(0:LFADC-1),IPULWID
      INTEGER I,BIN,MAXPNTS
      INTEGER IER
      INTEGER LUN,LUN_TDR,LUN_TDR2
      INTEGER COUNT(2),CUTOFF
      INTEGER LENGTH
      INTEGER RUNTYPE
C
      REAL    FACTOR,FACTOR2,FADC,PEAK,PEDESTAL
      REAL    Y_LOWER
      REAL    Y_UPPER
      CHARACTER*44 TEXT
      CHARACTER*27 TDRFILE,TDRFILE2
      LOGICAL OK
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        COUNT(1)=0
        COUNT(2)=0
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZRSET
        CALL EZPICK('FDC_RCP')
        CALL EZGETS('TDRFILE',1,TDRFILE,LENGTH,IER)
        CALL EZGETS('TDRFILE2',1,TDRFILE2,LENGTH,IER)
        CALL EZGET('MAXPNTS',MAXPNTS,IER)
        CALL EZRSET
C
        CUTOFF = 130
        IF(RUNTYPE.GT.0) CUTOFF=210
        CALL GTUNIT(702,LUN_TDR,IER)
        CALL D0OPEN(LUN_TDR,TDRFILE,'OF',OK)
C        CALL GTUNIT(703,LUN_TDR2,IER)
C        CALL D0OPEN(LUN_TDR2,TDRFILE2,'OF',OK)
        LUN_TDR2=LUN_TDR
        FIRST = .FALSE.
      END IF
C
C      IF ( IPULWID*NBPBIN .LT. CUTOFF) THEN
      LUN=LUN_TDR
      I=1
C      ELSE
C        GOTO 999
C      ENDIF
      IF ( COUNT(I) .GT. MAXPNTS .OR. IPULWID.LE.5 ) GOTO 999
      COUNT(I)=COUNT(I)+1
C
      IF(FACTOR.GT.0.) THEN
        I=1
        IF(I.EQ.1) LUN=LUN_TDR
        IF(I.EQ.2) LUN=LUN_TDR2
        WRITE (LUN,101)
  101   FORMAT(' set symbol 4p')
C        WRITE (LUN,102)
C  102   FORMAT('  TITLE  6.5 9.8 center '' ',TEXT,' '' ')
C        WRITE (LUN,103)
C  103   FORMAT('  TITLE  6.5 0.1 center size 0.2',
C    &    ' ''bin - (Leading Edge position)'' ')
        WRITE (LUN,*) ' TITLE  6.5 9.8 center '' ',TEXT,' '' '
        WRITE (LUN,*) ' TITLE  6.5 0.1 center size 0.2',
     &              ' ''bin - (Leading Edge position)'' '
C
        WRITE (LUN,104)
  104   FORMAT(' set order x y  ')
        WRITE (LUN,105)
  105   FORMAT(' set limits x from -5.0 to 30.0')
        WRITE (LUN,106)
  106   FORMAT(' set limits y from 0.0 to 1.25')
        WRITE (LUN,107)
  107   FORMAT(/' set labels size 0.2')
        Y_LOWER = 0.5
        Y_UPPER = 9.5
        WRITE (LUN,108) Y_LOWER,Y_UPPER
  108   FORMAT(/' set window y ', F5.2 , ' to ', F5.2)
        WRITE (LUN,'(A,F8.3,A)')
     &      ' title left size 0.2 ''counts-scaled by peak height  ',
     &      FACTOR,' '' '
        WRITE (LUN,110)
  110   FORMAT('  0.0,0.0 ' / ' plot' / ' set labels bottom off')
      ENDIF
C
      IF(FACTOR.GT.0.) THEN
        DO BIN =  IFIRST,IFIRST+IPULWID+25
          FADC=0.0
          IF((BIN+IPEV).GE.0) FADC=FLOAT(EXPDAT(BIN+IPEV))-PEDESTAL
          IF   ( (FADC*FACTOR  .GE. -1.0)
     &            .OR. (FADC*FACTOR  .LE. 5.0) ) THEN
            WRITE(LUN,121) BIN-PEAK,FADC*FACTOR
  121       FORMAT(1X,2F10.3)
          ENDIF
        ENDDO
        WRITE (LUN,131)
  131   FORMAT(' hist')
      ELSEIF(FACTOR2.GT.0.) THEN
        LUN=LUN_TDR2
        DO BIN =  IFIRST,IFIRST+IPULWID+25
          FADC=0.0
          IF((BIN+IPEV).GE.0) FADC=FLOAT(EXPDAT(BIN+IPEV))-PEDESTAL
          IF   ( (FADC*FACTOR2  .GE. -1.0)
     &            .OR. (FADC*FACTOR2  .LE. 5.0) ) THEN
            WRITE(LUN,121) BIN-PEAK,FADC*FACTOR2
          ENDIF
        ENDDO
        WRITE (LUN,132)
  132   FORMAT(' hist DOTS')
      ENDIF
      IF(FACTOR2.GT.0.) WRITE (LUN,133)
  133 FORMAT(' new frame')
C-------------------------------------------------------------------------
  999 RETURN
      END
