      SUBROUTINE ZTRHIS
C----------------------------------------------------------------------
C
C  Booking and filling histograms for ZTRAKS package
C
C  Daria Zieminska Nov 1989
C-   Updated  21-NOV-1990   Susan K. Blessing  Change some histogram
C-           titles because '_' comes out as 'z' in displays.
C-   Updated   3-DEC-1990   Susan K. Blessing   
C-           Fix FDC section.  Due to second pass tracking, NFDC may
C-           be larger than the actual number of tracks.  
C-   Updated  17-DEC-1990   Susan K. Blessing  Add a histogram for the
C-           number of CD tracks.  
C-   Updated  18-MAR-1991   Susan K. Blessing  FDC code has been changed
C-           so that NFDC is equal to the number of tracks.  Remove
C-           check.
C-   Updated  20-APR-1991   Qizhong Li-Demarteau  added histograms for 
C-                           theta, phi and beam distribution for cosmics
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  15-NOV-1991   Qizhong Li-Demarteau  added histograms for
C-                  # wires on CD tracks, subdetector's theta, phi and 
C-                  dE/dx distributions and added verification flag check
C-   Updated   2-DEC-1991   Qizhong Li-Demarteau  make sure it called once
C-                                                per event 
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  removed machine_block 
C-   Updated  18-APR-1992   Qizhong Li-Demarteau  added a switch HST_VERIFY 
C-   Updated  27-APR-1992   Qizhong Li-Demarteau  modified the Chi histograms
C-                            and added histogram for (ZTRK-VERTEX) at Z axis
C-   Updated  22-JUN-1992   Qizhong Li-Demarteau  added histograms for CDDn
C-                                                banks
C-        
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
C
      INTEGER IER,ERR,IQTRAK(26),NID(35),ID,ICONT(10)
      INTEGER ITRA,NCD,NVTX,NCDC,NFDC,NZBANK,LTRDT,GZTRDT
      INTEGER LADDER(0:2)
      INTEGER ICD,ISTAT,LZTRK,GZZTRK
      INTEGER LZFIT, NOVRF(35), IB
      INTEGER RUN, EVTID, RUNSAV, IDSAV
      INTEGER IBITS, IHIST
      INTEGER LVERT, GZVERT
      LOGICAL FIRST, VTX, CDC, FDC, CD
      LOGICAL EZERROR
      LOGICAL FLGVAL, VRFFLG, HST_VERIFY
C
      CHARACTER*34 NAME(35)
C
      REAL ZHIST(4,35),CONT(21),QVSEC(4,24),QHZLA(3,6),QFSEC(3,34)
      REAL FCONT(26)
      REAL PHI, THETA, ZBEAM, R0, Z0
      REAL QTRAK(26), ETOT, STATUS, DELPHI
      EQUIVALENCE(QTRAK,IQTRAK)
      REAL CHI_VTX, CHIZ_VTX, CHI_CDC, CHIZ_CDC, CHI_FDC
      REAL ZVTX, ZDIF
C
      SAVE RUNSAV,IDSAV
      SAVE FIRST, NOVRF
      DATA RUNSAV,IDSAV/-1,-1/
      DATA FIRST/.TRUE./
      DATA NAME/' Number CD Track ZTRKs',
     &  ' Number VTX Tracks',' Number CDC Tracks',' Number FDC Tracks',
     &  ' Number Hits on CD Track ZTRKs',
     &  ' Number Hits on VTX Tracks',' Number Hits on CDC Tracks',
     &  ' Number Hits on FDC Tracks',
     &  ' phi of ZTRKs',' phi of VTX tracks',' phi of CDC tracks',
     &  ' phi of FDC tracks',
     &  ' theta of ZTRKs',' theta of VTX tracks',' theta of CDC tracks',
     &  ' theta of FDC tracks',
     &  ' Chi(xy)/dof VTX Tracks',' Chi(z)/dof VTX Tracks',
     &  ' Chi(xy)/dof CDC Tracks',' Chi(z)/dof CDC Tracks',
     &  ' Chi/dof FDC Tracks',
     &  ' dE/dx from VTX tracks',
     &  ' dE/dx from CDC tracks',
     &  ' dE/dx from FDC tracks',
     &  ' Z at beam line from ZTRKs',
     &  ' ZTRK Status  ',
     &  ' (ZTRK - VERTEX) at Z axis',
     &  ' PHI(CDC)-PHI(VTX)',' PHI(FDC)-PHI(VTX)',
     &  ' Theta(CDC)-Theta(VTX)',' Theta(FDC)-Theta(VTX)',
     &  ' size of VTX raw data (words)','size of CDC raw data (words)',
     &  ' size of FDC raw data (words)','size of TRD raw data (words)'/
      DATA NOVRF/9*1,0,0,0,1,22*0/
C
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,EVTID)
      IF (RUN .NE. RUNSAV .OR. EVTID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = EVTID
C
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRHIS',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('ZHIST(1)',ZHIST(1,1),ERR)
        CALL EZGET('HST_VERIFY',HST_VERIFY,ERR)
        CALL EZRSET
        CALL DHDIR('ZTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
        IF (IER .NE. 0) THEN
          CALL ERRMSG('ZTRAKS','ZTRHIS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
C
        IF (FIRST) THEN    ! Book histograms
          FIRST = .FALSE.
          CALL VZERO(NID,35)
C       
C       The histograms 1-35 are for verification.
C       they are booked when the 'VERIFY' flag is TRUE.
C       The histograms 1-9 and 13 are booked when the 'VERIFY' flag
C       is FALSE
C       The histograms 28-31 are switched off in ZTRAKS.RCP at 
C       this moment
C       
          VRFFLG = FLGVAL('VERIFY')
          VRFFLG = VRFFLG .OR. HST_VERIFY
          DO 100 ID = 1, 35
            IF (ZHIST(1,ID).EQ.0.) GO TO 100
            IF (VRFFLG) THEN
              CALL HBOOK1(ID,NAME(ID),
     &      NINT(ZHIST(2,ID)),ZHIST(3,ID),ZHIST(4,ID),0.)
            ELSE
              IF (NOVRF(ID) .EQ. 1) THEN
                CALL HBOOK1(ID,NAME(ID),
     &      NINT(ZHIST(2,ID)),ZHIST(3,ID),ZHIST(4,ID),0.)
              ENDIF
            ENDIF
  100     CONTINUE
        ENDIF
C
        CALL GTZTRH(ICONT)
        NCD = ICONT(2)
        CALL GTVTRH(ICONT)
        NVTX = ICONT(2)
        CALL GTDTRH(ICONT)
        NCDC = ICONT(2)
        CALL GTFTRH(ICONT)
        NFDC = ICONT(2)
C
        IF (ZHIST(1,1) .EQ. 1.) CALL HFF1(1,NID(1),FLOAT(NCD),1.)
        IF (ZHIST(1,2) .EQ. 1.) CALL HFF1(2,NID(2),FLOAT(NVTX),1.)
        IF (ZHIST(1,3) .EQ. 1.) CALL HFF1(3,NID(3),FLOAT(NCDC),1.)
        IF (ZHIST(1,4) .EQ. 1.) CALL HFF1(4,NID(4),FLOAT(NFDC),1.)
C
        CD = ZHIST(1,5).EQ.1..OR.ZHIST(1,9).EQ.1..OR.ZHIST(1,13).EQ.1.
        DO 400 IHIST = 25, 31
          CD = CD .OR. (ZHIST(1,IHIST) .EQ. 1.0) 
  400   CONTINUE
        CD = NCD.GT.0 .AND. CD 
C
        IF (CD) THEN
          DO 200 ICD = 1, NCD
            LZTRK = GZZTRK(ICD)
            IF (LZTRK.LE.0) GOTO 200
            IF (.NOT. VRFFLG) GOTO 210
            IF (ZHIST(1,26) .EQ. 1.) THEN
              DO 211 IB = 0, 12
                ISTAT = IBITS(IQ(LZTRK),IB,1)
                IF (ISTAT .GT. 0) CALL HFF1(26,NID(26),FLOAT(IB),1.)
  211         CONTINUE
            ENDIF
            IF (IQ(LZTRK+2).GT.0 .AND. IQ(LZTRK+3).GT.0) THEN
              IF (ZHIST(1,28) .EQ. 1.) 
     &          CALL HFF1(28,NID(28),Q(LZTRK+6),1.)
              IF (ZHIST(1,30) .EQ. 1.) 
     &          CALL HFF1(30,NID(30),Q(LZTRK+8),1.)
            ELSE 
              IF (IQ(LZTRK+2).GT.0 .AND. IQ(LZTRK+4).GT.0 .AND. 
     &        IBITS(IQ(LZTRK),11,1).EQ.1) THEN
                IF (ZHIST(1,29).EQ.1.) 
     &            CALL HFF1(29,NID(29),Q(LZTRK+6),1.)
                IF (ZHIST(1,31).EQ.1.) 
     &            CALL HFF1(31,NID(31),Q(LZTRK+8),1.)
              ENDIF
            ENDIF
  210       LZFIT = LQ(LZTRK-IZZFIT)
            IF (LZFIT .LE. 0) GOTO 200
            IF (ZHIST(1,5) .EQ. 1.) 
     &      CALL HFF1(5,NID(5),FLOAT(IQ(LZFIT+6)),1.)
            IF (ZHIST(1,9) .EQ. 1.) THEN
              PHI = Q(LZFIT + 10)
              CALL HFF1(9,NID(9),PHI,1.0)
            ENDIF
            IF (ZHIST(1,13) .EQ. 1. .OR. ZHIST(1,25) .EQ. 1.) THEN
              THETA = Q(LZFIT + 13)
              IF (THETA .GT. 0.0) THEN
                IF (ZHIST(1,13) .EQ. 1.) 
     &            CALL HFF1(13,NID(13),THETA,1.0)
                IF (.NOT. VRFFLG) GOTO 200
                IF ((ZHIST(1,25) .EQ. 1.) .OR. 
     &              (ZHIST(1,27) .EQ. 1.)) THEN
                  R0 = Q(LZFIT + 14)
                  Z0 = Q(LZFIT + 15)
                  ZBEAM = Z0 - (R0/TAN(THETA))
                  IF (ZHIST(1,25) .EQ. 1) 
     &              CALL HFF1(25,NID(25),ZBEAM,1.0)
                  IF (ZHIST(1,27) .EQ. 1) THEN
                    LVERT = GZVERT(1)
                    IF (LVERT .GT. 0) ZVTX = Q(LVERT + 5)
                    ZDIF = ZBEAM - ZVTX
                    CALL HFF1(27,NID(27),ZDIF,1.0)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
C
        VTX = ZHIST(1,6).EQ.1..OR.ZHIST(1,10).EQ.1..OR.ZHIST(1,14).EQ.1.
     &      .OR.ZHIST(1,17).EQ.1..OR.ZHIST(1,18).EQ.1.
     &      .OR.ZHIST(1,22).EQ.1.
        VTX = NVTX.GT.0 .AND. VTX 
C
        IF (VTX) THEN
          DO 201 ITRA = 1, NVTX
            CALL GTVTXT(ITRA,CONT,QVSEC,QHZLA)
            CALL UCOPY(CONT,QTRAK,21)
            IF (ZHIST(1,6).EQ.1.) 
     &        CALL HFF1(6,NID(6),FLOAT(IQTRAK(2)),1.)
            IF (.NOT. VRFFLG) GOTO 201
            IF (ZHIST(1,10) .EQ. 1.) CALL HFF1(10,NID(10),QTRAK(6),1.)
            IF (ZHIST(1,14) .EQ. 1. .AND. QTRAK(9) .GT. 0.0) 
     &      CALL HFF1(14,NID(14),QTRAK(9),1.)
            IF (ZHIST(1,17) .EQ. 1. .AND. QTRAK(12) .GT. 0. .AND.
     &          IQTRAK(2) .GT. 2) THEN
              CHI_VTX = QTRAK(12)/(FLOAT(IQTRAK(2))-2.)
              CALL HFF1(17,NID(17),CHI_VTX,1.)
            ENDIF
            IF (ZHIST(1,18) .EQ. 1. .AND. QTRAK(13) .GT. 0. .AND.
     &          IQTRAK(5) .GT. 2) THEN
              CHIZ_VTX = QTRAK(13)/(FLOAT(IQTRAK(5))-2.)
              CALL HFF1(18,NID(18),CHIZ_VTX,1.)
            ENDIF
            IF (ZHIST(1,22) .EQ. 1.) CALL HFF1(22,NID(22),QTRAK(20),1.)
  201     CONTINUE
        ENDIF
C
        CDC = ZHIST(1,7).EQ.1..OR.ZHIST(1,11).EQ.1..OR.ZHIST(1,15).EQ.1.
     &      .OR.ZHIST(1,19).EQ.1..OR.ZHIST(1,20).EQ.1.
     &      .OR.ZHIST(1,23).EQ.1.
        CDC = NCDC.GT.0 .AND. CDC 
C
        IF (CDC) THEN
          DO 202 ITRA = 1, NCDC
            CALL GTDTRK(ITRA,CONT)
            CALL UCOPY(CONT,QTRAK,21)
            IF (ZHIST(1,7).EQ.1.) 
     &        CALL HFF1(7,NID(7),FLOAT(IQTRAK(2)),1.)
            IF (.NOT. VRFFLG) GOTO 202
            IF (ZHIST(1,11) .EQ. 1.) CALL HFF1(11,NID(11),QTRAK(6),1.)
            IF (ZHIST(1,15) .EQ. 1. .AND. QTRAK(9) .GT. 0.0) 
     &      CALL HFF1(15,NID(15),QTRAK(9),1.)
            IF (ZHIST(1,19) .EQ. 1. .AND. QTRAK(12) .GT. 0. .AND.
     &          IQTRAK(2) .GT. 2) THEN
              CHI_CDC = QTRAK(12)/(FLOAT(IQTRAK(2))-2.)
              CALL HFF1(19,NID(19),CHI_CDC,1.)
            ENDIF
            IF (ZHIST(1,20) .EQ. 1. .AND. QTRAK(9) .GT. 0. .AND.
     &          IQTRAK(5) .GT. 2) THEN
              CHIZ_CDC = QTRAK(13)/(FLOAT(IQTRAK(5))-2.)
              CALL HFF1(20,NID(20),CHIZ_CDC,1.)
            ENDIF
            IF (ZHIST(1,23) .EQ. 1.) CALL HFF1(23,NID(23),QTRAK(20),1.)
  202     CONTINUE
        ENDIF
C
        FDC = ZHIST(1,8).EQ.1..OR.ZHIST(1,12).EQ.1..OR.ZHIST(1,16).EQ.1.
     &      .OR.ZHIST(1,21).EQ.1..OR.ZHIST(1,24).EQ.1.
        FDC = NFDC.GT.0 .AND. FDC 
C
        IF (FDC) THEN
          DO 203 ITRA = 1, NFDC
            CALL GTFDCT(ITRA,FCONT,QFSEC,LADDER)
            CALL UCOPY(FCONT,QTRAK,26)
            IF (ZHIST(1,8).EQ.1.) 
     &        CALL HFF1(8,NID(8),FLOAT(IQTRAK(2)),1.)
            IF (.NOT. VRFFLG) GOTO 203
            IF (ZHIST(1,12) .EQ. 1.) CALL HFF1(12,NID(12),QTRAK(6),1.)
            IF (ZHIST(1,16) .EQ. 1.) CALL HFF1(16,NID(16),QTRAK(22),1.)
            IF (ZHIST(1,21) .EQ. 1. .AND. QTRAK(19) .GT. 0. .AND.
     &          IQTRAK(2) .GT. 2) THEN
              CHI_FDC = QTRAK(19)/(FLOAT(IQTRAK(25))-4.)
              CALL HFF1(21,NID(21),CHI_FDC,1.)
            ENDIF
            IF (ZHIST(1,24) .EQ. 1.) CALL HFF1(24,NID(24),QTRAK(20),1.)
  203     CONTINUE
        ENDIF
C
        CALL ZHSCDD
C
      ENDIF
  999 RETURN
      END
