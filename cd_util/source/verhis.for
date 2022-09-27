      SUBROUTINE VERHIS
C----------------------------------------------------------------------
C
C  Booking and filling histogram for VERTEX package
C
C  Daria Zieminska
C  JAN 1990
C-   Updated  19-SEP-1991   Qizhong Li-Demarteau  added histograms 
C-                                                 (ID: 4 - 10)
C-   Updated  15-NOV-1991   Qizhong Li-Demarteau  added verification flag
C-                                                check
C-   Updated  22-JAN-1992   Daria Zieminska   fill histogram 2 if NVER=0
C-   Updated  19-MAR-1992   Qizhong Li-Demarteau  extended status bits 
C-   Updated  28-APR-1992   Qizhong Li-Demarteau  added a switch HST_VERIFY 
C-   Updated  22-SEP-1992   Qizhong Li-Demarteau  added histograms 11 & 12
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INTEGER IER,ERR
      INTEGER NVMAX, NVER, IHIST
      PARAMETER (NVMAX=5)
      INTEGER LISAE1, LISAE2, GZISAE, LISV1, LISV12
      INTEGER LVERH, LVERT, GZVERH, STATUS, FLAG, I
      LOGICAL FIRST, HIST, ISAJET, EZERROR, EVTOK
      LOGICAL FLGVAL, VRFFLG, ISFLG1, ISFLG2, HST_VERIFY
      CHARACTER*44 NAME(11)
      REAL VERHIST(4,11)
      REAL ZVER(NVMAX), EZVER(NVMAX)
      REAL    ZERR1, ZERR2, ZISAJT1, ZISAJT2, ZDIF1, ZDIF2
C
      SAVE FIRST, ISAJET, ISFLG1, ISFLG2
      DATA FIRST/.TRUE./
      DATA ISAJET/.FALSE./
      DATA ISFLG1/.FALSE./, ISFLG2/.FALSE./
      DATA NAME/' Number of primary vertices',
     &          ' Z of primary vertex',
     &          ' vertex reconstruction status',
     &          ' Z error (VERTEX1)',
     &          ' Z error (VERTEX2)',
     &          ' VERTEX1 diff. in Z (reco-isaj)',
     &          ' VERTEX2 diff. in Z (reco-isaj)',
     &          ' Z(reco-isaj)/error for ver1',
     &          ' Z(reco-isaj)/error for ver2',
     &          ' Number of primary vertices (no Jet-trigger)',
     &          ' Z of primary vertex (no Jet-trigger)'/
C----------------------------------------------------------------------
C
C Create/set HBOOK directory VERTEX
C
      CALL DHDIR('VERTEX_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('VERTEX','VERHIS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (FIRST) THEN    
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS/VERTEX','VERHIS',
     &    'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('VERHIST(1)',VERHIST(1,1),ERR) 
        CALL EZGET_l('HST_VERIFY',HST_VERIFY,ERR) 
        CALL EZRSET
        LISAE1 = GZISAE()
        IF (LISAE1 .GT. 0) THEN
          ISFLG1 = .TRUE.
          IF (LQ(LISAE1) .GT. 0) ISFLG2 = .TRUE.
        ENDIF
C       
C       the histograms 2-12 are for verification, they are booked when 
C       the 'VERIFY' flag is TRUE.
C       The histograms 7-10 are not booked when there is no ISAJET 
C       information available.
C       When the 'VERIFY' flag is FALSE, Only the histograms 2-4 
C       and 11-12 are booked 
C
        VRFFLG = FLGVAL('VERIFY')
        VRFFLG = VRFFLG .OR. HST_VERIFY
        DO 100 IHIST = 1, 11
          IF (VERHIST(1,IHIST) .EQ. 0.0) GOTO 100
          IF (.NOT.VRFFLG .AND. IHIST .GT. 3 .AND. IHIST .LT. 10) 
     &      GOTO 100
          IF (VERHIST(1,IHIST) .GT. 0.) THEN
            IF (IHIST .LT. 5 .OR. IHIST .GT. 9) THEN
              CALL HBOOK1(IHIST+1,NAME(IHIST),NINT(VERHIST(2,IHIST)),
     &             VERHIST(3,IHIST),VERHIST(4,IHIST),0.)
              HIST = .TRUE.
            ELSE
              IF (MOD(IHIST,2) .EQ. 0 .AND. ISFLG1) THEN
                CALL HBOOK1(IHIST+1,NAME(IHIST),NINT(VERHIST(2,IHIST)),
     &               VERHIST(3,IHIST),VERHIST(4,IHIST),0.)
                HIST = .TRUE.
              ELSE
                IF (ISFLG2) THEN
                CALL HBOOK1(IHIST+1,NAME(IHIST),NINT(VERHIST(2,IHIST)),
     &                 VERHIST(3,IHIST),VERHIST(4,IHIST),0.)
                  HIST = .TRUE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
  100   CONTINUE
        IF (HIST) THEN
          DO 200 IHIST = 6, 9
            ISAJET = ISAJET .OR. (VERHIST(1,IHIST) .GT. 0.0) 
  200     CONTINUE
        ENDIF
      ELSE
        IF (ISFLG1 .AND. (.NOT. ISFLG2)) THEN
          LISAE1 = GZISAE()
          IF ((LISAE1 .GT. 0) .AND. (LQ(LISAE1) .GT. 0)) THEN
            ISFLG2 = .TRUE.
            DO 101 IHIST = 5, 9, 2
              CALL HBOOK1(IHIST+1,NAME(IHIST),NINT(VERHIST(2,IHIST)),
     &           VERHIST(3,IHIST),VERHIST(4,IHIST),0.)
  101       CONTINUE            
            HIST = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      CALL HIDOPT(0,'STAT')
C
      IF (.NOT.HIST) GOTO 999
      DO 303 I = 1, NVMAX
        EZVER(I) = 9999.9
  303 CONTINUE
      CALL ZVERTE(NVER, ZVER, EZVER)
C
C  extract z coordinate of primary vertex {ZVER(1)} and fill histogram
C
      IF (VERHIST(1,1).EQ.1..OR.VERHIST(1,2).EQ.1.) THEN
        IF (VERHIST(1,1).EQ.1.) CALL HFILL(2,FLOAT(NVER),0.,1.)
        IF (NVER.GT.0.AND.VERHIST(1,2).EQ.1.) 
     &      CALL HFILL(3,ZVER(1),0.,1.)
        CALL ZCOGNG(EVTOK)
        IF (EVTOK) THEN
          IF (VERHIST(1,10) .GT. 0.) 
     &      CALL HFILL(11,FLOAT(NVER),0.,1.)
          IF ((NVER .GT. 0) .AND. (VERHIST(1,11) .GT. 0.))
     &      CALL HFILL(12,ZVER(1),0.,1.)
        ENDIF
      ENDIF
      IF (NVER .LE. 0) GOTO 999
C
      IF (VERHIST(1,3) .GT. 0.0) THEN
        LVERH = GZVERH()
        IF (LVERH .GT. 0) THEN
          LVERT = LQ(LVERH - 1)
  202     IF (LVERT .LE. 0) GOTO 205
          STATUS = IQ(LVERT + 2)
          DO 201 I = 24, 31
            FLAG = IBITS(STATUS,I,1)
            IF (FLAG .GT. 0) CALL HFILL(4,FLOAT(I),0.,1.)
  201     CONTINUE
          LVERT = LQ(LVERT)
          GOTO 202
        ENDIF
      ENDIF
C 
  205 IF (VERHIST(1,4) .GT. 0.0 .AND. EZVER(1) .LT. 999.9)
     &   CALL HFILL(5,EZVER(1),0.,1.)
      IF (VERHIST(1,5) .GT. 0.0 .AND. EZVER(2) .LT. 999.9)
     &   CALL HFILL(6,EZVER(2),0.,1.)
C
C  check reconstruction quality by comparing reconstructed verties with
C  isajet information
C
      IF (ISAJET) THEN
        IF (EZVER(1) .LT. 999.9) THEN
          LISAE1 = GZISAE()
          IF (LISAE1 .LE. 0) GOTO 999
          LISV1 = LQ(LISAE1-IZISV1)
          IF (LISV1 .GT. 0) THEN
            IF (VERHIST(1,6) .GT. 0.0 .OR. VERHIST(1,8) .GT. 0.0) THEN
              ZISAJT1 = Q(LISV1+9)
              ZDIF1 = ZISAJT1 - ZVER(1)
              IF (VERHIST(1,6) .GT. 0.) CALL HFILL(7,ZDIF1,0.,1.)
              IF (VERHIST(1,8) .GT. 0. .AND. EZVER(1) .NE. 0.0) THEN
                ZERR1 = ZDIF1 / EZVER(1)
                CALL HFILL(9,ZERR1,0.,1.)
              ENDIF
            ENDIF
            IF (EZVER(2) .LT. 999.9 .AND.
     &         (VERHIST(1,7) .GT. 0.0 .OR. VERHIST(1,9) .GT. 0.0)) THEN
              LISAE2 = LQ(LISAE1)
              IF (LISAE2 .LE. 0) GOTO 999
              LISV12 = LQ(LISAE2-IZISV1)
              IF (LISV12 .LT. 0) GOTO 999
              ZISAJT2 = Q(LISV12+9)
              ZDIF2 = ZISAJT2 - ZVER(2)
              IF (VERHIST(1,7) .GT. 0.) CALL HFILL(8,ZDIF2,0.,1.)
              IF (VERHIST(1,9) .GT. 0. .AND. EZVER(2) .NE. 0.0) THEN
                ZERR2 = ZDIF2 / EZVER(2)
                CALL HFILL(10,ZERR2,0.,1.)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
