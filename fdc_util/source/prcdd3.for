      SUBROUTINE PRCDD3 ( PRUNIT, KCDD3, NCDD3, CARFL, IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print digitization banks CDD3
C-
C-   Inputs  : PRUNIT : unit number
C-             KCDD3  : Pointer to one bank ( or first of a linear set )
C-             NCDD3  : Id number of one bank
C-             CARFL  : Character flag, 'ALL' = all CDD3 banks
C-                      'ONE' = only te one identified by KCDD3 OR NCDD3
C-                      Note: since there is only one CDD3 bank per event,
C-                      'ALL' and 'ONE' are the same.
C-             IPRFL  : Level of print
C-                      0 = no printout
C-                      1 = Length of the bank and header information
C-                      2 = FADC # and # of clusters in each one
C-                      3 = Full content of the bank
C-
C-   Created  17-AUG-1987   Ghita Rahal-Callot
C-   Updated   8-DEC-1987   Ploutarchos Roumeliotis
C-   Updated  12-FEB-1988   Ghita Rahal-Callot  : add levels of printout
C-   Updated   5-OCT-1988   Jeffrey Bantly  for use with FDC 
C-   Updated  23-NOV-1988   Ghita Rahal-Callot  : add 1 level of printout 
C-   Updated  25-MAR-1990   Jeffrey Bantly  upgrade for versions 0-4
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER PRUNIT, KCDD3, LCDD3, NCDD3, IPRFL, IVERS, VTYPE, IERR
      INTEGER LONG,LENFADC,LABFADC,ICLUS,NWFADC,I
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,LENCLU,LOCCLU,IP,ITMP
      INTEGER IPORIG,IPIN,LDHEAD,LCRATE,LTRAIL,LDCTRL,DATAS(LFADC)
C
C **** Number of 32-bit words per channel, must work for TB 512 bins
C
      INTEGER MXLABL
      PARAMETER (MXLABL = 32768)     ! For 15 bits label
      INTEGER HIGHOF, LOWOFF
      PARAMETER ( HIGHOF = 16 , LOWOFF = 0 )
      INTEGER LEDGE,NWORDS,DETAILS(5,10),MINLON
C
      CHARACTER*(*) CARFL
      CHARACTER*8 ZFORMA
      CHARACTER*4 PATH
C
      SAVE LTRAIL,LDCTRL
      DATA LTRAIL/4/                ! length of a crate trailer bank
      DATA LDCTRL/16/               ! length of the data cable trailer
C----------------------------------------------------------------------
      LCDD3 = KCDD3
      IF ( IPRFL .LE. 0 ) GO TO 999
      IF ( LCDD3 .LE. 0 ) THEN
        LCDD3 = LQ ( LHEAD - IZCDD3 )
      ENDIF
      IF ( LCDD3 .LE. 0 ) THEN
        CALL ERRMSG('Bank not booked','PRCDD3',
     &              'Bank CDD3 does not exist','I')
        GO TO 999
      ENDIF
C
C   Get bank version number
C
      CALL ZRD_VERSION(3,IVERS,VTYPE,IERR)
      IF( IERR .NE. 0) THEN
        CALL ERRMSG('Bank not booked','PRCDD3',
     &              'Bank CDD3 does not exist or wrong data cable','I')
        GO TO 999
      ENDIF
      LONG=IQ(LCDD3-1)
      WRITE(PRUNIT,101) IVERS,LONG
      IF( IVERS .GT. 4) THEN
        CALL ERRMSG('Bank version 5+','PRCDD3',
     &              'Bank CDD3 version 5+ not yet supported','I')
        GO TO 999
      ENDIF
      IF(IPRFL.LE.1) GOTO 999
C
C   Get header information
C
      CALL CD_DECODE_HEADER(LCDD3,IVERS,NWORDS,DETAILS)
      WRITE(PRUNIT,102) DETAILS(1,1),DETAILS(2,1),(DETAILS(3,I),I=1,4),
     &  (DETAILS(4,I),I=1,3)
C
C   Get cluster information
C
      IPORIG=LCDD3
      LCRATE=LONG
      IF ( IVERS .GE. 3 ) THEN
        LONG = LONG - LDCTRL
        LCRATE = IQ(IPORIG + LONG -3)
      ELSE
        IF ( IVERS .EQ. 2 ) THEN
          LCRATE = IQ(IPORIG + LONG -3) + LTRAIL
        ENDIF
      ENDIF
      IF ( IVERS .EQ. 0 ) THEN
        MINLON = 0
      ELSEIF ( IVERS .NE. 0 ) THEN
        LDHEAD = IQ(IPORIG+1) + 1
        MINLON = LDHEAD + LONG - LCRATE
      ENDIF
      IF ( IVERS .LE. 2 ) THEN
        LONG = LONG - 2 * IVERS
      ELSE
        LONG = LONG - LTRAIL
      ENDIF
C-----------------------------------------------------------------------
C     LABFADC=the fadc channel's logical channel address
C     LENFADC=the fadc's total length(bytes)
C-----------------------------------------------------------------------
   10 CONTINUE
      IPIN = IPORIG + LONG
      IF (LONG .LE. MINLON) GO TO 20
      ITMP=IQ(IPIN)
      LENFADC=IBITS(ITMP,LOWOFF,16)
      LABFADC=IBITS(ITMP,HIGHOF,15)
      IF(LENFADC.GT.LFADC) GOTO 900
      IF(LABFADC.GT.MXLABL) GOTO 900
C
C   Decode the FADC address
C
      CALL FCODER(LABFADC,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      WRITE(PRUNIT,103) ZFORMA(LABFADC),HALF,UNIT,QUAD,SECTOR,WIRE,
     &                                                 LENFADC
      NWFADC=LENFADC/4
      IF ( IVERS.EQ.0 ) NWFADC = NWFADC - 1
      IF ( LENFADC.LE.0) GOTO 910
C
C   Unfold the cluster data from the longwords
C   LENCLU=the length of the cluster in fadc bins
C   LOCCLU=the location of the cluster in fadc bins
C
      CALL ZDEXPD(3,LABFADC,DATAS)
      IP = 1
      ICLUS = 0
   30 CONTINUE
      LENCLU = DATAS(IP)
      IF( LENCLU .NE. 0 ) THEN
        ICLUS = ICLUS + 1
        LOCCLU = DATAS(IP+1)
        IP = IP + 2
        LEDGE=LOCCLU-LENCLU+1
        IF( IPRFL .GE. 2 ) THEN
          WRITE(PRUNIT,104) ICLUS,LEDGE,LOCCLU,LENCLU
        ENDIF
C
        IF ( IPRFL .EQ. 3 ) THEN
          WRITE ( PRUNIT, 105 ) ( DATAS(I), I= IP, IP+LENCLU-1)
C         WRITE ( PRUNIT, 106 ) ( DATAS(I), I= IP, IP+LENCLU-1)
        ENDIF
C
        IP = IP + LENCLU
        GOTO 30
      ENDIF
C
C   Move on to next channel
C
      LONG=LONG-NWFADC
      GOTO 10
C
   20 IF((IVERS.GT.1) .AND. (MINLON.GT.LDHEAD)) THEN
C
C   Prepare to handle next crate
C
        LONG = LONG - LDHEAD
        IF (IVERS.EQ.3) LONG = LONG - 1
        LCRATE = IQ(IPORIG + LONG - 3)
        IF (IVERS.EQ.2) LCRATE = LCRATE + LTRAIL
        MINLON = LDHEAD + LONG - LCRATE
        LONG = LONG - LTRAIL
        GOTO 10
      ENDIF
C
C   Done.
C
      GOTO 999
C
C   Problem channels
C
  900 CONTINUE
      WRITE(PRUNIT,107) LONG,LENFADC,LFADC,LABFADC,MXLABL
      GOTO 999
  910 CONTINUE
      WRITE(PRUNIT,108) LONG,LENFADC
      GOTO 999
C
  101 FORMAT(/10X,' Bank CDD3: Version ',I3,'   Total # of words:',I5)
  102 FORMAT(5X,' Header Length    =',I5,'   Sync Word =',I10,
     &  '  Crate ID      =',I10/
     &       5X,' Num of FADC Cards=',I5,'   Crate Mode=',I10,
     &  '  Data Type     =',I10/
     &       5X,' System word   =',I8,'   User word =',I10,
     &  '  Version Number=',I10)
  103 FORMAT(5X,' FADC#:',A8,2X,'(Half=',I3,2X,'Unit=',I3,2X,
     &'Quad=',I3,2X,'Sect=',I3,2X,
     &'Wire=',I3,')',2X,'# of Bytes:',I3)
  104 FORMAT(7X,'Cluster #:',I2,2X,' Leading Edge:',I3,2X
     &  ,' Trailing Edge:',I3, 2X,' # of time slices:',I3)
  105 FORMAT( (7X,'LSB/MSB  ',4(4(I3,2X),' / ')) )
  106 FORMAT( (7X,'MSB/LSB  ',4(4(I3,2X),' / ')) )
  107 FORMAT(' Error in bank CDD3, offset=',I10,'  Fadc Length=',I10,
     &       '>',I10,' the maximum OR'/40X,' Fadc label=',I10,'>',I10,
     &       ' the maximum')
  108 FORMAT(' Error in bank CDD3, offset=',I10,'  Fadc Length=',I10)
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
