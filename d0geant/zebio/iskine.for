      SUBROUTINE ISKINE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Isjet Data file and fill Geant track and vertex
C-                              banks
C-
C-   Inputs  : Isajet input file, if supplied
C-   Outputs : Geant track and vertex banks
C-   Controls:
C-
C-   Created  ??-MAR-1986   S.Kunori
C-   Updated   1-MAR-1988   Ghita Rahal-Callot  : Add run and event number
C-                           in the header bank
C-   Updated  28-NOV-1988   Alan M. Jonckheere  : Add Parton and Jet numbers
C-                           to UBUF
C-   Updated  26-AUG-1989   Alan M. Jonckheere  : Set Geant's random number
C-                           seed from the ISAJET seed
C-   Updated  26-FEB-1992   K. Wyatt Merritt  Merge last 3.11 test release
C-                           with 3.14 version 
C-   Updated   5-AUG-1993   Jasbir Singh, Chip Stewart - MUONLIBRARY SHWG=6
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCNUM.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
C
      INTEGER LISAE,LISV1,LISP1,NVTX,NT,KPART
      INTEGER LISAQ,LISAJ,NPARTN,NJET
      INTEGER IUHEAD(14),ISTATS
      REAL    UBUF(10),PP(4)
      INTEGER KPART2,NWBUF2,ITRTY2
      CHARACTER*20 NPARTN2
      REAL    AMASS2,CHARG2,TLIFE2,UBUF2(10)
      INTEGER NIO,NIOE,I
      INTEGER NTRAK,NTRAK1
      INTEGER CHGEAN,CHEVNT
      LOGICAL FIRST
      DATA FIRST/.TRUE./        ! First vertex in event
C
      INTEGER NRANDO,NBIT2
      INTEGER IEVT
C
C----------------------------------------------------------------------
C
      NTRAK = 0
C
C --check if input is required...
      IF ( IRDUNI.EQ.0 ) GO TO 900
   10 CONTINUE
C
      CALL RDZEB(-1,IUHEAD,ISTATS)
C
C --check if error and record type in HEAD...
      IF ( ISTATS.NE.0 .OR. MOD(IQ(LHEAD+1),10).LT.3 ) THEN
C  read error or EOF was found...
        IEOTRI = 1        ! flag for end of trigger
        IEORUN = 1        ! flag for end of run
        GOTO 990
      ENDIF
C
C --CHECK IF WE ARE SUPPOSED TO BE SKIPPING EVENTS (ZBIO 3=n)
C
      IF (IEVENT.LE.IZBOPT(1) ) THEN
        CALL MZWIPE(IXCOM+21)
        IEVENT = IEVENT + 1
        GO TO 10
      ENDIF
C
      CALL ISDROP                  ! drop JVERTX and JKINE banks
C
C --put vertices and particles into Geant banks...
C  loop over vertices...
      LISAE = LQ(LHEAD-IZISAE)
   50 IF ( LISAE.LE.0 ) THEN
        WRITE(LOUT,*)
     &    ' *** ERROR IN S/R ISKINE ***   BANK POINTER TO ISAE WAS ',
     &    LISAE
        GOTO 990
      ENDIF
C
C ****  Get Random Number seed used by ISAJET, will use it here too.
      IF ( IQ(LISAE-1).GE.18 ) THEN
        IF ( (IQ(LISAE+17).NE.0) .OR. (IQ(LISAE+18).NE.0) ) THEN
          NRNDM(1) = MOD(IQ(LISAE+18),65536)
          NRNDM(2) = MOD(IQ(LISAE+17),65536)
          NRANDO=0
          NBIT2=NBIT/2
          CALL SBYT(NRNDM(1),NRANDO,NBIT2+1,NBIT2)
          CALL SBYT(NRNDM(2),NRANDO,1,NBIT2)
          CALL RDMIN(NRANDO)
          CALL GRNDMQ(IABS(NRNDM(2)),IABS(NRNDM(1)),1,'S')
        ENDIF
      ENDIF
C
      LISV1 = LQ(LISAE-IZISV1)
      IF ( LISV1.LE.0 ) THEN
        WRITE(LOUT,*)
     &    ' *** ERROR IN S/R ISKINE ***   BANK POINTER TO ISV1 WAS ',
     &    LISV1
        GOTO 990
      ENDIF
      FIRST = .TRUE.
C
C ****  Showerlibrary
      IF ( SHWG.EQ.3 ) THEN
        CALL USE_SHOWERLIBRARY          ! New showerlibrary
C                                       ! fast loop thru traks.
      ELSE IF(SHWG.EQ.6) THEN
        CALL  USE_SHOWERLIBRARY         ! CAL showerlibrary
      ELSE IF(SHWG.EQ.8) THEN
        CALL  USE_SHOWERLIBRARY         ! CAL showerlibrary
        CALL  USE_MUONLIBRARY           ! muon hit library
      ELSE
C
  100   CALL ISAJET_GEANT_VTX(LISV1,NVTX,FIRST)  ! Store vertex
        FIRST = .FALSE.
C  loop over particles...
        LISP1 = LQ(LISV1-IZISP1)
        NTRAK1 = 0
  200   IF ( LISP1.GT.0 ) THEN
          NTRAK = NTRAK + 1                  ! Total Tracks
          NTRAK1 = NTRAK1 + 1                ! Tracks on this vertex
          CALL ISAJET_GEANT_TRACK(LISP1,NVTX,NTRAK,NTRAK1)
          LISP1 = LQ(LISP1)
          GO TO 200
        ENDIF
C
        LISV1 = LQ(LISV1)
        IF ( LISV1.NE.0 ) GO TO 100
C
        LISAE = LQ(LISAE)
        IF ( LISAE.GT.0 ) GOTO 50
      ENDIF
C
  990 CONTINUE
C -- DEBUG -------------------------------------------------------
C       (ZBIO 4=max,5=print_level)
      IF ( IZBOPT(3).NE.0 .AND. IEVENT.LE.IZBOPT(2) ) THEN
        WRITE(LOUT,*) 
     &    ' ----- DEBUG IN S/R ISKINE ----- IEVENT = ', IEVENT
C
        CALL DZSURV('IN ISKINE',IXMAIN,LHEAD)
        IF ( IZBOPT(3).GE.2 ) THEN
C&IF VAXVMS,ETA10,SIUNIX,IBMAIX,ULTRIX,SUNOS,ALFOSF
          IF(LQ(LHEAD-IZISAE).NE.0) CALL PRTEVZ(6)
C&ENDIF
          IF ( IZBOPT(3).GE.3 ) THEN
            CALL GPVERT(0)
            CALL GPKINE(0)
          ENDIF
        ENDIF
C
C -- DEBUG -------------------------------------------------------
      ENDIF
  900 CONTINUE
      IF ( IRDUNI.EQ.0 ) THEN
        CALL MZWIPE(IXCOM+21)
      ENDIF
      FIRST = .TRUE.
C
C ****  Print event #
C
      IF ( DTRK.NE.2 .AND. PD0.GT.0 ) THEN
        IEVT = 0
        IF ( IQ(LHEAD+7).GT.0 ) IEVT = IQ(LHEAD+7)
        WRITE (LOUT,*)
     &    ' ***** EVENT # ',IEVENT,' ISAJET EVENT ',IEVT
      ENDIF
C
  999 END
