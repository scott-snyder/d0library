      SUBROUTINE ZGEXPD(DCDTYP, LABEL, DATAS, LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode routine for all CD subdetectors. 
C-                         This version extracts a single channel
C-                         from CDDn bank and fills an array, without
C-                         doing any more unpacking.
C-
C-   Inputs  : DCDTYP [I] : decode type (0: decode all CDDn banks,
C-                                       n: decode CDDn bank only)
C-              LABEL [I] : Encoded FADC label, the one found in the datas
C-                         of the FADC ( i.e. 15 bits ).
C-   Outputs : DATAS [I*] : Data of the corresponding channel, in the format
C-                         written originally.
C-             integer length = number of bytes in the data
C-
C-   Created  Sept. 91 C. Klopfenstein, from ZDEXPD (QZ Li).
C-   Mod. March 92 - use different link area to avoid conflict with 
C-                   ZDEXPD. CK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
C      INCLUDE 'D0$INC:ZCDDLK.INC'
      COMMON /ZRECDD/ LCDDN(4)
      INTEGER LCDDN
C
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER DCDTYP, LABEL, DATAS(*), MXLABL
      PARAMETER (MXLABL = 32768)     ! For 15 bits label
      integer length, ifirst, index
      INTEGER IPOINT(0:MXLABL-1,2)   ! bank address pointer for the label
      INTEGER LCLU, LCHA, IPIN, IPOUT, NEVOLD, IPORIG, IBNK, I, J, JCDD
      INTEGER LONG, ILAB, MINLON, LDHEAD, LCRATE, LTRAIL, LDCTRL
      INTEGER PRUNIT, USUNIT, VERSION, VTYPE, IERR, ERRCNT
      INTEGER HIGHOF, LOWOFF
      PARAMETER ( HIGHOF = 16 , LOWOFF = 0 )
      INTEGER IDTYPE
      CHARACTER*3 DTTYPE(0:3)
      LOGICAL PRODUC, PRODFL, BLDPTR, PTDONE(4), FIRST
C
      DATA NEVOLD / -1 /
      DATA DTTYPE/'VTX','CDC','FDC','TRD'/
      DATA FIRST/.TRUE./
      DATA LTRAIL/4/        ! length of the create trailer block
      DATA LDCTRL/16/       ! length of the data cable trailer
C  Because this routine can be called by many packages (e.g. VTRAKS, DTRAKS,
C  FTRAKS, TRD, VERTEX, ZTRAKS...), some hard-coded numbers are better
C  kept in DATA statements of this routine rather than in a xxxxx.RCP file
C
C----------------------------------------------------------------------
C
C   make a link area for all Central Detector FADC banks
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZLINK(IXCOM,'/ZRECDD/',LCDDN(1),LCDDN(4),LCDDN(1))
      ENDIF
C
      IF (LHEAD .EQ. 0) GOTO 990
      IPOUT = 1
      BLDPTR = .FALSE.
      IF ( IQ(LHEAD+9) .NE. NEVOLD ) THEN
        NEVOLD = IQ(LHEAD+9)
        BLDPTR = .TRUE.
        DO 50 I = 1, 4
  50    PTDONE(I) = .FALSE.
        CALL VZERO(IPOINT, MXLABL)
      ELSE
        IF (DCDTYP .NE. 0) THEN
          IF (.NOT. PTDONE(DCDTYP)) BLDPTR = .TRUE.
        ELSE
          BLDPTR = .NOT. (PTDONE(1) .AND. PTDONE(2) 
     &             .AND. PTDONE(3) .AND. PTDONE(4))
        ENDIF
      ENDIF
C
      IF (BLDPTR) THEN
        PRODFL = PRODUC()
        IF (.NOT. PRODFL) PRUNIT = USUNIT()
C 
C ****  Build index tables ...
C
        DO 10 J = IZCDD1, IZCDD4
          JCDD = J - IZCDD1 + 1
          IF ((DCDTYP .NE. 0) .AND. (DCDTYP .NE. JCDD)) GOTO 10
          IF (PTDONE(JCDD)) GOTO 10
          PTDONE(JCDD) = .TRUE.
          LCDDN(JCDD) = LQ(LHEAD - J)
          IF (LCDDN(JCDD) .EQ. 0) GOTO 10
          IPORIG = LCDDN(JCDD) 
          LONG   = IQ(IPORIG - 1)
C
C   find the type of the raw data format
C
          CALL ZRD_VERSION(JCDD, VERSION, VTYPE, IERR)
C
C   To prepare to deal with multi-crates
C   (Only one crate exist in version 0 and version 1 format)
C
          LCRATE = LONG
          IF (VERSION .GE. 3) THEN
            LONG = LONG - LDCTRL
            LCRATE = IQ(IPORIG + LONG - 3)
          ELSE
            IF (VERSION .EQ. 2) THEN
              LCRATE = IQ(IPORIG + LONG - 3) + LTRAIL
            ENDIF
          ENDIF
          IF (VERSION .NE. 0) THEN
            LDHEAD = IQ(IPORIG + 1) + 1 
            MINLON = LDHEAD + LONG - LCRATE
          ELSE
            MINLON = 0
          ENDIF
          IF (VERSION .LE. 2) THEN
            LONG = LONG - 2 * VERSION
          ELSE
            LONG = LONG - LTRAIL
          ENDIF
C
   20     IPIN   = IPORIG + LONG
          IF (LONG .LE. MINLON) GOTO 11
          ILAB = IBITS( IQ(IPIN), HIGHOF, 15 )      ! bits 16-30 are lables
          LCHA = IBITS( IQ(IPIN), LOWOFF, 16 )
          IF (LCHA .GT. LFADC) GOTO 900
          IF (ILAB .GT. MXLABL) GOTO 900
          IPOINT( ILAB, 1 ) = IPIN - IPORIG     ! Pointer rel. to LCDDn
          IPOINT( ILAB, 2 ) = JCDD      ! CDDn bank number (JCDD=n)
          LCHA = LCHA/4 
          IF (VERSION .EQ. 0) LCHA = LCHA + 1       
          IF (LCHA .LE. 0) GOTO 10
          LONG = LONG - LCHA
          GOTO 20
C
   11     IF ((VERSION .GT. 1) .AND. (MINLON .GT. LDHEAD)) THEN
C
C  to prepare to deal with the next crate
C
            LONG = LONG - LDHEAD 
            IF (VERSION .EQ. 3) LONG = LONG - 1
            LCRATE = IQ(IPORIG + LONG - 3) 
            IF (VERSION .EQ. 2) LCRATE = LCRATE + LTRAIL
            MINLON = LDHEAD + LONG - LCRATE
            LONG = LONG - LTRAIL     
            GOTO 20
          ENDIF
   10   CONTINUE
      ENDIF
C
C ****  Here, the pointer table is OK. So access the datas
C
      IF( LABEL .LT. 0 .OR. LABEL .GT. MXLABL ) GOTO 990
      IF( IPOINT( LABEL, 1 ) .EQ. 0 ) then
        length = 4
        datas(1) = ishft(label, 16) + length
        GOTO 999
      endif
C      IF (LCDDN(IPOINT(LABEL,2)) .LE. 0) GOTO 901
      IPIN = IPOINT( LABEL, 1 ) + LCDDN( IPOINT( LABEL, 2 ) )
      length = ibits( IQ(ipin), lowoff, 16)
      do index = 1, length/4
        datas(index) = iq(ipin - length/4 + index)
      enddo
      ipout = length/4
C
C ****  Protect anomalies
C
C      IF (DATAS(IPOUT+1) .LT. 0 .OR. DATAS(IPOUT+1) .GE. LFADC) THEN
C  901   IF (.NOT. PRODFL) THEN
C          IDTYPE = IBITS( LABEL, 13, 2)
C          WRITE( PRUNIT,2000 ) IQ(LHEAD+9), DTTYPE(IDTYPE), 
C     &      LCDDN(IPOINT(LABEL,2)),
C     &        IQ(IPOINT(LABEL,1)+LCDDN(IPOINT(LABEL,2))), LCHA, LABEL,
C     &                       IQ(IPIN), LCLU, DATAS(IPOUT+1)
C 2000     FORMAT(/' **ZDEXPD** anomaly in data, event',I6,', ',
C     &          A3,' data,',' LCDDN =',I8/ 
C     &          10X,' FADC label =',Z9,' Lcha =',I8/
C     &          10X,' for fadc ',I6,' label =',Z9,' lcl,fir =',2I10)
C        ENDIF
C        IPOUT = 1
C        GOTO 990
C      ENDIF
      ipout = length/4 + 1
  990 DATAS(IPOUT) = 0
  999 RETURN
C
C ****  Error, the structure of the data seems to be destroyed
C       suppress error messages during production run
C
  900 CONTINUE
      IF (.NOT. PRODFL) THEN
        WRITE(PRUNIT, 1000) IQ(LHEAD+9), JCDD, IQ(IPORIG-1), VERSION
 1000   FORMAT(/' ** ZDEXPD ** At event',I10,' problem for bank CDD',I1,
     &        ' with length ', I10,' and version # = ',I1)
        IF (VERSION .LE. 2) THEN
          LONG = IQ(IPORIG-1) - 2 * VERSION
        ELSE
          LONG = IQ(IPORIG-1) - LDCTRL - LTRAIL 
        ENDIF
        ERRCNT = 0
  910   IPIN = IPORIG + LONG
        ILAB = IBITS( IQ(IPIN), HIGHOF, 15 )
        LCHA = IBITS( IQ(IPIN), LOWOFF, 16 )
        WRITE( PRUNIT, 1100 ) LONG, ILAB, LCHA
 1100   FORMAT(10X,'At offset ',I10,' found label=',I10,' length=',I10)
        ERRCNT = ERRCNT + 1
        LCHA = LCHA/4 
        IF (VERSION .EQ. 0) LCHA = LCHA + 1       
C
C  to print five error messages in maximum, then skip this CDDn bank
C
        IF (LCHA .LE. 0) GOTO 10
        LONG = LONG - LCHA
        IF (LONG .LE. MINLON) GOTO 10
        IF (ERRCNT .GE. 5) THEN
          WRITE (PRUNIT, 1101)
 1101     FORMAT (1X,'     ............',/)
        ELSE
          GOTO 910
        ENDIF
      ENDIF
      GOTO 10
      END
