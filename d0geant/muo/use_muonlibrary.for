      SUBROUTINE USE_MUONLIBRARY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOOP THROUGH ISP1 TRACKS
C-   PUT MUONS ON GEANT STACK
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JUN-1990   Jasbir Singh and Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:SHSORT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INTEGER LISAE,LISV1,LISP1,LJET
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
      INCLUDE 'D0$INC:SHLITR.INC'
C
      INTEGER GZISAE,GZISV1
      INTEGER IPART
C
      INTEGER IER
      LOGICAL FIRST,FRSTVX
      DATA FIRST/.TRUE./
      INTEGER ITRA,ITRA1
      INTEGER JQ
      INTEGER CYC,CYC_MAX
      REAL    RNDM
C
      REAL    P4(4)
      REAL    IETAM_SRT(NSRTMX),IPHIM_SRT(NSRTMX)
      INTEGER RKEY(NKEY)
C
      INTEGER I,K,NO_CYC
      REAL    PT_MUON,PT_MUON_FULL_GEANT,FRACTION,F
      INTEGER NVTXG
      INTEGER PRD_KEY,INDX_KEY
      LOGICAL FIL_OPEN,MULBEG_RUN
      EQUIVALENCE(IETAC_SRT,IETAM_SRT)
      EQUIVALENCE(IPHIC_SRT,IPHIM_SRT)
C----------------------------------------------------------------------
C
      FRSTVX = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MULINI
        FIL_OPEN= MULBEG_RUN()
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL EZGET('PT_MUON_FULL_GEANT',PT_MUON_FULL_GEANT,IER)
        CALL ERRMAX ('MUONLIBRARY',-1,9999)
        CALL EZRSET
C
      ENDIF

C
      KEY(1) = 1
      KEY(2) = 1
      KEY(3) = 1
      KEY(4) = 1
C
      LISAE=GZISAE()                  ! GET LINK
C
      LISV1=GZISV1()
      ITRA=0                              ! number of tracks.
      NSRT = 0                          ! SORT COUNTER
      DO WHILE (LISV1 .NE. 0)
        CALL UCOPY(Q(LISV1+7),VERTEX,3)
        CALL ISAJET_GEANT_VTX(LISV1,NVTXG,FRSTVX)       ! store vtx
        FRSTVX = .FALSE.
        LISP1=LQ(LISV1-IZISP1)          ! NO GZISP1!!
        ITRA1 = 0
        DO WHILE (LISP1 .NE. 0)
C
C GET PRIMARY KEY HERE
C
          CALL UCOPY(Q(LISP1+2),P4,4)
          IPART = IQ(LISP1+1)         ! PARTICLE ID
          IF(IABS(IPART).EQ.14)THEN     ! MUON
            PT_MUON = SQRT(Q(LISP1+2)**2 + Q(LISP1+3)**2)
            IF(PT_MUON.GT.PT_MUON_FULL_GEANT)THEN
              ITRA=ITRA + 1                 ! COUNT TRACKS
              ITRA1 = ITRA1 + 1             ! TRACK NUMBER AT THIS VERTEX.
              CALL ISAJET_GEANT_TRACK(LISP1,NVTXG,ITRA,ITRA1)
C PUT IT ON GEANT STACK
              IF ( DTRK.NE.2 .AND. PD0.GT.0 )THEN
                CALL ERRMSG('MUONLIBRARY','USE_MUONLIBRARY',
     &            'MUON PUT ON GEANT STACK ','W')
              ENDIF
              GO TO 190
            ENDIF
          ENDIF
          CALL MUON_GETBIN(IPART,VERTEX,P4)     ! GET PRIMARY KEY
          PRD_KEY = 1                  ! INIT
          DO I = 1 , NKEY
            PRD_KEY = PRD_KEY*KEY(I)
          ENDDO
          IF(PRD_KEY.EQ.0)GO TO 190   ! ONE OF THE KEYS = 0
C
  321     CYC_MAX = CYCLES(INDX_KEY(KEY))
          NO_CYC  = NO_CYCLES(INDX_KEY(KEY))
          IF(CYC_MAX.EQ.0) THEN
            FRACTION = 0.
          ELSE
            FRACTION = FLOAT(CYC_MAX)/FLOAT(NO_CYC+CYC_MAX)
          END IF
C
C ****  DETERMINE IF THIS TRACK HAS MUON HITS _ RANDOMLY
C
          F = RNDM(0)
          IF(F.GT.FRACTION) GOTO 190   !next track
C
          CALL MULIB_MATCH_KEY(LISP1,KEY,CYC_MAX,CYC,IER)
C
          CALL UCOPY_i(KEY,RKEY,NKEY)
C
          NSRT = NSRT + 1
          IF(NSRT.GT.NSRTMX) CALL ERRMSG('MUONLIBRARY',
     &      'USE_MUONLIBRARY',
     &      'TOO MANY TRACKS TO BE SORTED ','W')
C
          CALL UCOPY_i(RKEY,RKEY_SRT(1,NSRT),NKEY)
          CYC_SRT(NSRT) = CYC
          IETAM_SRT(NSRT) = IETAM_PRIMARY
          IPHIM_SRT(NSRT) = IPHIM_PRIMARY
          CALL UCOPY(P4,P4_SRT(1,NSRT),4)
          ITRA_SRT(NSRT) = ITRA
          IMAP(NSRT) = NSRT             ! INITIAL MAP
C
          CALL MULIB_PACK_REC(RKEY,CYC,KEYS_SRT(NSRT))        ! PACK KEY AWAY
C
  190     CONTINUE      ! jump to here if track be skipped...
C        -- going to next linear link (isp1)...
          LISP1=LQ(LISP1)
        END DO
C     -- going to next isv1 linear link...
        LISV1=LQ(LISV1)
      END DO
C
C ****  NOW TO SORT ON KEYS AND DO RZVIN.
C
      IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &    CALL GTIMER('USE_MUONLIBRARY: BEFORE SORTING')
C
      CALL SRTINT(KEYS_SRT,NSRT,IMAP)
C
      DO I = 1 , NSRT
        CALL RZVIN1(MULIB,NMULIB,NDATA,RKEY_SRT(1,IMAP(I)),
     &      CYC_SRT(IMAP(I)),' ')
        IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.CYC_SRT(IMAP(I)))THEN
          CALL ERRMSG('MUONLIBRARY','USE_MUONLIBRARY',
     &        'ERROR DURING RZVIN1','W')
          WRITE(LOUT,605)(RKEY_SRT(K,IMAP(I)),K=1,NKEY),
     &        CYC_SRT(IMAP(I))
  605     FORMAT(' *******  Error during RZ READ ******',(I8))
          DO 606 JQ=1,10
            WRITE(LOUT,607)IQUEST(JQ)
  606     CONTINUE
  607     FORMAT(I8)
        ELSE
          IETAM_PRIMARY = IETAM_SRT(IMAP(I))
          IPHIM_PRIMARY = IPHIM_SRT(IMAP(I))
C
          CALL USE_MULIB(P4_SRT(1,IMAP(I)),ITRA_SRT(IMAP(I)))
        ENDIF
      ENDDO
C
      IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &    CALL GTIMER('USE_MUONLIBRARY: END OF MUON LIBRARY ACCESS')
  999 CONTINUE
      RETURN
      END
