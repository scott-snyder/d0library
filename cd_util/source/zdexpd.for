      SUBROUTINE ZDEXPD(DCDTYP, LABEL, DATAS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode routine for all CD subdetectors.
C-              It reads from CDDn raw data bank the FADC content
C-              identified by label LABEL, and returns the unpacked
C-              data in the DATAS array, in the following format :
C-
C-              - Cluster_Length  (0  means end of data)
C-              - First_Bin_Address  (starting at 0)
C-              - Data for this cluster, one FADC bin per word
C-              - Next Cluster_length
C-              - Next Cluster First_bin_address
C-              - ...
C-
C-              The routine fills a two dimension pointer map:
C-              IPOINT(0:MXLABL,2).
C-              For a given FADC LABEL,
C-                      IPOINT(LABEL,1) = relative pointer in bank CDDn
C-                      IPOINT(LABEL,2) = n
C-              The links LCDDN(n), n=1,..,4 are maintained in a Zebra
C-              link area to avoid garbage collection problems.
C-
C-              The pointer map is filled once per event for each CDDn
C-              bank. Which CDDn bank is decoded is controlled by DCDTYP
C-
C-              WARNING : In order to scan only once per event the CDDn
C-              bank to build the pointer map, we rely on the event number
C-              stored in the event header. If this is not changing from
C-              event to event, any kind of error can result...
C-
C-   Inputs  : DCDTYP [I] : decode type (0: decode all CDDn banks,
C-                                       n: decode CDDn bank only)
C-              LABEL [I] : Encoded FADC label, the one found in the datas
C-                          of the FADC ( i.e. 15 bits ).
C-   Outputs : DATAS [I*] : array containing unpacked FADC data for the
C-                          corresponding chanel (given by the input LABEL)
C-                          in the format described previously.
C-
C-   Created  19-MAR-1989   Qizhong Li-Demarteau   from CDEXPD to ZDEXPD
C-   Updated  22-JUN-1989   Qizhong Li-Demarteau / Peter Grudberg
C-                                    use link area to avoid the problems
C-                                    caused by garbage collection
C-   Updated  12-OCT-1989   Qizhong Li-Demarteau   deal with multi crates,
C-                             recognize different format data (version 0,
C-                             1 and 2), add a choice DCDTYP and use USUNIT
C-   Updated  19-DEC-1989   Qizhong Li-Demarteau  make link area permanent
C-                             and move VZERO for IPONIT to once per event
C-   Updated   3-JAN-1990   Qizhong Li-Demarteau  to be able to unpack
C-                                           version 3 format data too
C-   Updated   6-MAR-1990   Qizhong Li-Demarteau upgraded to unpack version
C-                                           3 or 4 format data too
C-   Updated   3-APR-1991   Qizhong Li-Demarteau  modification for TRD
C-   Updated   8-MAY-1992   Qizhong Li-Demarteau  rewritten error printout
C-   Updated  20-MAY-1992   Qizhong Li-Demarteau  separate VERSION for each
C-                                                subdetector
C-   Updated  26-APR-1993   Peter Grudberg  Use separate verion for each CDD
C-                                          bank, not detector (works with test
C-                                          beam data).
C-   Updated  17-AUG-1994   Qizhong Li-Demarteau  remove call to cernlib
C-                                                routine to speed up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:ZCDDLK.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER DCDTYP, LABEL, DATAS(*), MXLABL
      PARAMETER (MXLABL = 32768)     ! For 15 bits label
      INTEGER IPOINT(0:MXLABL-1,2)   ! bank address pointer for the label
      INTEGER LCLU, LCHA, IPIN, IPOUT, NEVOLD, IPORIG, IBNK, I, J, JCDD
      INTEGER LONG, ILAB, MINLON, LDHEAD, LCRATE, LTRAIL, LDCTRL
      INTEGER PRUNIT, USUNIT, VERSION(4), VTYPE, IERR, ERRCNT
      INTEGER NEVPRT(4), ERRCDD
      INTEGER HIGHOF, LOWOFF
      PARAMETER ( HIGHOF = 16 , LOWOFF = 0 )
      INTEGER NWLCLU, NBLCLU, NBUFF, IW, IB, INDEX
      INTEGER IBUFF
      BYTE    BBUFF(4)
      EQUIVALENCE (IBUFF,BBUFF(1))
      INTEGER IDTYPE
      CHARACTER*3 DTTYPE(0:3)
      LOGICAL PRODUC, PRODFL, BLDPTR, PTDONE(4), FIRST, ERR_FADC
C
      DATA NEVOLD/ -1 /
      DATA NEVPRT/-1,-1,-1,-1/
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
        CALL MZLINK(IXCOM,'/ZCDDLK/',LCDDN(1),LCDDN(4),LCDDN(1))
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
        CALL VZERO(IPOINT(0,1), 2*MXLABL)
        ERR_FADC = .FALSE.
      ELSE
        IF (DCDTYP .NE. 0) THEN
          IF (.NOT. PTDONE(DCDTYP)) BLDPTR = .TRUE.
        ELSE
          BLDPTR = .NOT. (PTDONE(1) .AND. PTDONE(2)
     &             .AND. PTDONE(3) .AND. PTDONE(4))
        ENDIF
      ENDIF
C
C  building the two dimension pointer map for fast accessing data later
C
      IF (BLDPTR) THEN
        PRODFL = PRODUC()
        IF (.NOT. PRODFL) PRUNIT = USUNIT()
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
          CALL ZRD_VERSION(JCDD, VERSION(JCDD), VTYPE, IERR)
C
C   To prepare to deal with multi-crates
C   (Only one crate exist in version 0 and version 1 format)
C
          LCRATE = LONG
          IF (VERSION(JCDD) .GE. 3) THEN
            LONG = LONG - LDCTRL
            LCRATE = IQ(IPORIG + LONG - 3)
          ELSE
            IF (VERSION(JCDD) .EQ. 2) THEN
              LCRATE = IQ(IPORIG + LONG - 3) + LTRAIL
            ENDIF
          ENDIF
          IF (VERSION(JCDD) .NE. 0) THEN
            LDHEAD = IQ(IPORIG + 1) + 1
            MINLON = LDHEAD + LONG - LCRATE
          ELSE
            MINLON = 0
          ENDIF
          IF (VERSION(JCDD) .LE. 2) THEN
            LONG = LONG - 2 * VERSION(JCDD)
          ELSE
            LONG = LONG - LTRAIL
          ENDIF
C
   20     IPIN   = IPORIG + LONG
          IF (LONG .LE. MINLON) GOTO 11
          ILAB = IBITS( IQ(IPIN), HIGHOF, 15 )      ! bits 16-30 are lables
          LCHA = IBITS( IQ(IPIN), LOWOFF, 16 )
          IF (LCHA .GT. LFADC .OR. ILAB .GT. MXLABL)THEN
C
C Error: the structure of the data seems to be destroyed
C        suppress error messages during production run
C
            IF (.NOT. PRODFL) THEN
              IF (PRUNIT .GT. 0) THEN
                WRITE(PRUNIT, 1000) IQ(LHEAD+6),IQ(LHEAD+9),
     &            JCDD,IQ(IPORIG-1),VERSION(JCDD)
 1000           FORMAT(/' ** ZDEXPD ** At run/event',I8,'/',I8,
     &            ' problem for bank CDD',I1,
     &            /' with length ', I10,' and version # = ',I1)
                IPIN = IPORIG + LONG
                ILAB = IBITS( IQ(IPIN), HIGHOF, 15 )
                LCHA = IBITS( IQ(IPIN), LOWOFF, 16 )
                WRITE( PRUNIT, 1100 ) LONG, ILAB, LCHA
 1100           FORMAT(10X,'At offset ',I10,' found label=',I10,
     &            ' length=',I10)
              ENDIF
            ENDIF
            GOTO 10
          ENDIF
          IPOINT( ILAB, 1 ) = IPIN - IPORIG     ! Pointer rel. to LCDDn
          IPOINT( ILAB, 2 ) = JCDD      ! CDDn bank number (JCDD=n)
          LCHA = LCHA/4
          IF (VERSION(JCDD) .EQ. 0) LCHA = LCHA + 1
          IF (LCHA .LE. 0) GOTO 10
          LONG = LONG - LCHA
          GOTO 20
C
   11     IF ((VERSION(JCDD) .GT. 1) .AND. (MINLON .GT. LDHEAD)) THEN
C
C  to prepare to deal with the next crate
C
            LONG = LONG - LDHEAD
            IF (VERSION(JCDD) .EQ. 3) LONG = LONG - 1
            LCRATE = IQ(IPORIG + LONG - 3)
            IF (VERSION(JCDD) .EQ. 2) LCRATE = LCRATE + LTRAIL
            MINLON = LDHEAD + LONG - LCRATE
            LONG = LONG - LTRAIL
            GOTO 20
          ENDIF
   10   CONTINUE
      ENDIF
C
C   The pointer map exists already, so access the data
C
      IF (LABEL .LT. 0 .OR. LABEL .GT. MXLABL) GOTO 990
      IF (IPOINT( LABEL, 1 ) .EQ. 0) GOTO 990
      IF (LCDDN(IPOINT(LABEL,2)) .LE. 0) GOTO 901
      IPIN = IPOINT( LABEL, 1 ) + LCDDN( IPOINT( LABEL, 2 ) )
      LCHA = IBITS( IQ(IPIN), LOWOFF, 16 ) / 4 - 1
      IF (LCHA .LE. 0) GOTO 990
      IPIN = IPIN - 1
C
C   IPIN point on last word of a cluster
C   ( i.e. last FADC bin address of a cluster + cluster length )
C
  100 LCLU = IBITS( IQ(IPIN), LOWOFF, 16 ) - 2
      IDTYPE = IBITS(LABEL, 13, 2)
      IF (VERSION(IPOINT(LABEL,2)) .NE. 0) LCLU = LCLU - 2
      IF (LCLU .LT. 0 .OR. LCLU .GT. LCHA*4) GOTO 901
      IF (LCLU .EQ. 0) GOTO 990
      DATAS( IPOUT   ) = LCLU
      DATAS( IPOUT+1 ) = IBITS( IQ(IPIN), HIGHOF, 16 ) - LCLU + 1
C
C  error handling: give some information about what are wrong
C        (don't print more than once per event for one detector)
C
      ERR_FADC = .FALSE.
      IF (DATAS(IPOUT+1) .LT. 0 .OR. DATAS(IPOUT+1) .GE. LFADC) THEN
        ERR_FADC = .TRUE.
        GOTO 901
      ENDIF
      NWLCLU = LCLU/4 + 1
      LCHA = LCHA - NWLCLU
      IPIN = IPIN - NWLCLU
      NBLCLU = LCLU
      NBUFF = 0
      DO 101 IW = 1, NWLCLU
        IBUFF = IQ(IPIN+IW)
        NBLCLU = NBLCLU - NBUFF
        NBUFF = MIN(NBLCLU, 4)
        DO 102 IB=1, NBUFF
          INDEX = (5+(BYTE1-BYTE2)*(2*IB-5))/2
          DATAS(IPOUT+1+IB) = BBUFF(INDEX)
          IF (DATAS(IPOUT+1+IB).LT.0) 
     &      DATAS(IPOUT+1+IB) = DATAS(IPOUT+1+IB) + 256
  102   CONTINUE
        IPOUT = IPOUT + NBUFF
  101 continue
      IPOUT = IPOUT + 2
      IF (LCHA .GT. 0) GOTO 100
      GOTO 990
  901 IF (.NOT. PRODFL) THEN
        IF (PRUNIT .GT. 0) THEN
          ERRCDD = IPOINT(LABEL,2)
          IF (NEVPRT(ERRCDD) .NE. NEVOLD) THEN
            NEVPRT(ERRCDD) = NEVOLD
            IDTYPE = IBITS(LABEL, 13, 2)
            WRITE(PRUNIT,2000) IQ(LHEAD+6),IQ(LHEAD+9),
     &          DTTYPE(IDTYPE),LCDDN(IPOINT(LABEL,2))
            IF (LCDDN(ERRCDD) .GT. 0) THEN
              WRITE(PRUNIT,2001)
     &          IQ(IPOINT(LABEL,1)+LCDDN(IPOINT(LABEL,2))),LCHA,LABEL,
     &          LCLU
              IF (ERR_FADC) THEN
                WRITE(PRUNIT,2002) DATAS(IPOUT+1)
                ERR_FADC = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IPOUT = 1
  990 DATAS(IPOUT) = 0
  999 RETURN
C
 2000 FORMAT(/' **ZDEXPD** problem in run/event',I8,'/',I8,',',
     &          A3,' data, LCDDN=',I8)
 2001 FORMAT(1X,' FADC label =',Z9,' Lcha =',I10,
     &        ' for fadc ',I8,' cluster_length =',I10)
 2002 FORMAT(1X, ' first FADC bin = ', I10)
C
      END
