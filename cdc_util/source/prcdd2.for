      SUBROUTINE PRCDD2(PRUNIT, KCDD2, NCDD2, CARFL, IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print digitization banks CDD2
C-
C-   Inputs  : PRUNIT : unit number
C-             LCDD2  : Pointer to one bank ( or first of a linear set )
C-             NCDD2  : Id number of one bank
C-             CARFL  : Character flag, 'ALL' = all CDD2 banks
C-                      'LINEAR' = all linear structure from LCDD2
C-                      'SINGLE' = only the one with LCDD2 OR NCDD2
C-             IPRFL  : Level of print
C-                      0 = no printout
C-                      1 = Length of the bank
C-                      2 = FADC # and # of clusters in each one
C-                      3 = Full content of the bank
C-
C-   Created  17-AUG-1987   Ghita Rahal-Callot
C-   Updated   8-DEC-1987   Ploutarchos Roumeliotis
C-   Updated  12-FEB-1988   Ghita Rahal-Callot  : add levels of printout
C-   Updated  23-NOV-1988   Ghita Rahal-Callot  : add 1 level of minimum
C-                           printout  
C-   Updated  13-JUN-1989   Qizhong Li-Demarteau  modify the logic address
C-   Updated  14-JUL-1989   Qizhong Li-Demarteau  print version # 
C-   Updated  30-JUL-1990   Qizhong Li-Demarteau  upgraded for data format
C-                                                versions 0 - 4 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INTEGER PRUNIT, KCDD2, LCDD2, NCDD2, IPRFL, ITYPE
      INTEGER IA,IB,LEFADC,NWFADC,NFADC,IEND,ICLUS,ITRE,ILENG
      INTEGER NWW
      PARAMETER (NWW = 130)
      INTEGER LONG, IPORIG, LCRATE, LDHEAD, LDCTRL, LTRAIL, MINLON 
      PARAMETER( LTRAIL = 4 )
      PARAMETER( LDCTRL = 16 )
      INTEGER  ITRAIL, LEDGE, NWOR, I, IDATA
      INTEGER  IW1(NWW), IW2(NWW), IW3(NWW), IW4(NWW)
      INTEGER  IWR, ISEC, ILAY, ISIZE, NVERS, VTYPE, IERR
      INTEGER  NWORDS, DETAILS(5,5)
      CHARACTER*(*) CARFL
      CHARACTER*8 ZFORMA, DTYPE
      CHARACTER*3 DTTYPE(0:3)
      DATA DTTYPE/'VTX','CDC','FDC','TRD'/
C----------------------------------------------------------------------
      LCDD2 = KCDD2
      IF ( IPRFL .LE. 0 ) GO TO 999
      IF ( LCDD2 .LE. 0 ) THEN
        LCDD2 = LQ (LHEAD - IZCDD2)
      ENDIF
      IF ( LCDD2 .LE. 0 ) THEN
        CALL ERRMSG('Bank not booked','PRCDD2',
     &              'Bank CDD2 does not exist','I')
        GO TO 999
      ENDIF
C
      CALL ZRD_VERSION(2,NVERS,VTYPE,IERR)
      IF( IERR .NE. 0) THEN
        CALL ERRMSG('Bank not booked','PRCDD2',
     &              'Bank CDD2 does not exist or wrong data cable','I')
        GO TO 999
      ENDIF
      IF( NVERS .GT. 4) THEN
        CALL ERRMSG('Bank version 5+','PRCDD2',
     &              'Bank CDD2 version 5+ not yet supported','I')
        GO TO 999
      ENDIF
C
      LONG = IQ(LCDD2 - 1)
      WRITE(PRUNIT,1030) NVERS, LONG
      IF ( IPRFL .LE. 1 ) GO TO 999
C
      IPORIG=LCDD2
      LCRATE = LONG
      IF (NVERS .GE. 3) THEN
        LONG = LONG - LDCTRL
        LCRATE = IQ(IPORIG + LONG - 3)
      ELSE
        IF (NVERS .EQ. 2) THEN
          LCRATE = IQ(IPORIG + LONG - 3) + LTRAIL
        ENDIF
      ENDIF
      IF (NVERS .NE. 0) THEN
        LDHEAD = IQ(IPORIG + 1) + 1 
        MINLON = LDHEAD + LONG - LCRATE
      ELSE
        MINLON = 0
      ENDIF
      IF (NVERS .LE. 2) THEN
        LONG = LONG - 2 * NVERS
      ELSE
        LONG = LONG - LTRAIL
      ENDIF
   20 CONTINUE
C
      CALL CD_DECODE_HEADER(IPORIG,NVERS,NWORDS,DETAILS)
      DTYPE = ' UNKNOWN'
      IF (DETAILS(4,1) .EQ. 0) THEN
        DTYPE = ' D0 DATA'
      ELSE 
        IF (DETAILS(4,1) .EQ. 32) THEN
          DTYPE = ' MC DATA'
        ELSE
          IF (DETAILS(4,1) .EQ. 64) DTYPE = 'NWA DATA'
        ENDIF
      ENDIF
      WRITE(PRUNIT,1020) DETAILS(1,1),DETAILS(2,1),(DETAILS(3,I),I=1,4),
     &  DTYPE,(DETAILS(4,I),I=2,3)
      ITRE = LCDD2 + LONG
C
C-----------------------------------------------------------------------
C**** "IA" is the last number of every cluster and contains the FADC
C     number and the fadc's total length(bytes)
C**** "IEND" is the first word of every FADC
C-----------------------------------------------------------------------
  886 IA = IQ(ITRE)
      LEFADC = IBITS(IA,0,16)
      NFADC = IBITS(IA,16,16)
      NWFADC = LEFADC/4
      IF (NVERS .GT. 0) NWFADC = NWFADC - 1  !
      IEND = ITRE - NWFADC
      ICLUS = 0
C
C ****  Decode the FADC address
C
      IWR = IBITS(NFADC,0,4)
      ISEC = IBITS(NFADC,4,5)
      ILAY = IBITS(NFADC,9,2)
      ITYPE = IBITS(NFADC,13,2)
      IF (ITYPE .EQ. 1) THEN
        WRITE(PRUNIT,1040) 
     &    ZFORMA(NFADC), DTTYPE(ITYPE), ILAY, ISEC, IWR, LEFADC
      ELSE
        WRITE(PRUNIT, 1041) ZFORMA(NFADC), DTTYPE(ITYPE), LEFADC
      ENDIF
      IF (NWFADC .LE. 1) THEN
        ITRE = ITRE - 1
        GOTO 886
      ENDIF
  887 ICLUS = ICLUS + 1
      ITRE = ITRE - 1
C
C ****  Decode the cluster length, and the trailing edge
C
      IB = IQ(ITRE)
      ILENG = IBITS(IB,0,16)
      ITRAIL = IBITS(IB,16,16)
      ISIZE = ILENG - 2
      IF (NVERS .GT. 0) ISIZE = ISIZE - 2
      LEDGE = ITRAIL - ISIZE + 1
      WRITE(PRUNIT,1050) ICLUS,LEDGE,ITRAIL,ISIZE
      NWOR = ISIZE / 4
      IF ( IPRFL .GE. 3 ) THEN
        DO 310 I = 1, NWOR
C
C ****  Decode the data in the current cluster
C
          IDATA = IQ(ITRE - I)
          IW1(I) = IBITS(IDATA,0,8)
          IW2(I) = IBITS(IDATA,8,8)
          IW3(I) = IBITS(IDATA,16,8)
          IW4(I) = IBITS(IDATA,24,8)
  310   CONTINUE
        WRITE ( PRUNIT, 1060 )
     &    ( IW4(I), IW3(I), IW2(I), IW1(I), I=NWOR, 1, -1)
      ENDIF
      ITRE = ITRE - NWOR
      IF(ITRE .GT. IEND) GO TO 887
      ITRE = ITRE - 1
      IF(ITRE .GT. LCDD2+MINLON) GO TO 886
      IF ((NVERS .GT. 1) .AND. (MINLON .GT. LDHEAD)) THEN
C
C  to prepare to deal with the next crate
C
        LONG = LONG - LDHEAD 
        IF (NVERS .EQ. 3) LONG = LONG - 1
        LCRATE = IQ(IPORIG + LONG - 3) 
        IF (NVERS .EQ. 2) LCRATE = LCRATE + LTRAIL
        MINLON = LDHEAD + LONG - LCRATE
        LONG = LONG - LTRAIL     
        GOTO 20
      ENDIF
 1030 FORMAT(/,10X,' Bank CDD2:  (data format version',I2,
     &  '),      Total # of words:',I5)
 1020 FORMAT(/,5X,' header length =', I5,'           trigger # =',I5,/,
     &  5X,' crate ID =', I3,'   # of FADC cards =', I3,
     &  '   Crate mode =', I3,'   data type', I3,/,
     &  6X,'--', A8,'--    user word =', I4,'   version # =', I5)
 1040 FORMAT(5X,' FADC#:',A8,2X,'(Type = ',A3,2X,'Lay = ',I3,2X,
     &  'Sect = ', I3,2X,'Wire = ',I3,')',2X,'# of Bytes:',I3)
 1041 FORMAT(5X,' FADC#:',A8,2X,'(Type = ',A3,')',2X,'# of Bytes:',I3)
 1050 FORMAT(7X,'Cluster #:',I2,2X,' Leading Edge:',I3,2X
     &  ,' Trailing Edge:',I3, 2X,' # of time slices:',I3)
 1060 FORMAT( (7X,'MSB/LSB  ',4(4(I3,2X),' / ')) )
  999 RETURN
      END
