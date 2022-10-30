      SUBROUTINE TTRAKS(ILINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User Event Routine for D0USER
C-
C-   Inputs  :ILINK(1) : link to ZTRK (could be  = 0)
C-            ILINK(2) : link to CACL " " " " " " " "
C-   Outputs :ILINK(3) : link to TRDT if ILINK(1),ILINK(2)>0
C-   Controls:
C-
C-
C-
C-   Created  20-FEB-1989   A. Zylberstejn
C-   Updated  21-MAY-1990   J.Fr.Glicenstein: links to ZTRK and CACL
C-                          to allow direct access to calorimeter
C-                          candidate
C-   Updated   7-SEP-1990   A. Zylberstejn   :allow computation of multiplicity
C-                                            in the TRD cells
C-   Updated  15-MAR-1994   A. Zylberstejn : call TRD_ANALYSIS and fill new
C-                                           words in TRDT bank
C-   Updated  11-OCT-1994   Alain PLUQUET  call TRD_ANALYSIS_IN_RECO
C-   Updated  28-FEB-1995   A. Zylberstejn  : check if trd is reqired in
C-                                ZTRAKS_RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PROVL.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:TRTOBN.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
C      INCLUDE 'D0$INC:zlinka.INC'
      INCLUDE 'D0$INC:ttrak_type.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  GZTRDH,GZTRDT,GZZTRH,LPROC,GZPROC,LRECO,GZRECO
      INTEGER  LZTRDH,LZTRH,LTTRH,GZTTRH,LTHIT,GZTHIT
      INTEGER I,IER,IAUX(4),ICH,ILINK(3),LTRDT,LZTRK,LZFIND,NEVOLD
      INTEGER IDENT,ILAY,ITRA,ITRACK,IERR,LINKTR(3),LL,LOUT,NTOTEV
      INTEGER JBIT,TYPTRA,TRUNIT,GOODTR,NGI
      REAL TRGTIM,EFFICIENCY
      LOGICAL ACCEPTANCE,CALL_TRD_ANALYSIS,CALL_TRD, EZERROR,TRDON
      CHARACTER*3 C3
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      LOGICAL DOCLUS,DOPRINT,FIRST,FILL_TLYR,DO_HISTO,TRD_DO_PRINT
      LOGICAL FLGVAL,REDUCED_SET
      DATA FIRST/.TRUE./
      DATA NTOTEV / 0 /
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT = TRUNIT()
        TYPTRA = 0
        CALL_TRD=.TRUE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','TTRAKS',
     &      'Unable to find bank ZTRAKS_RCP','W')
        ELSE
          CALL EZGET_l('CALL_TRD',CALL_TRD,IER)
          CALL EZGET_l('TRDON',TRDON,IER)
          CALL_TRD=CALL_TRD .OR. TRDON
        ENDIF
        CALL EZRSET
        IF(.NOT.CALL_TRD)GO TO 999
        CALL EZPICK('TRD_RCP')
        DOCLUS=.FALSE.
C        CALL EZGET('CLUSTER_RECONS',I,IERR)
C        IF(IERR.EQ.0)THEN
C          CALL UHTOC(I,3,C3,3)
C          DOCLUS=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
C        END IF
        FILL_TLYR=.FALSE.
        CALL EZGET_i('FILL_HITS',I,IERR)
        IF(IERR.EQ.0)THEN
          CALL UHTOC(I,3,C3,3)
          FILL_TLYR=C3.EQ.'Y' .OR. C3.EQ.'y'
     &      .OR. C3.EQ.'YES'
        END IF
        DO_HISTO=.FALSE.
        CALL EZGET_iarr('HSTBOK',IAUX,IERR)
        IF(IERR.EQ.0)THEN
          CALL UHTOC(IAUX(1),3,C3,3)
          DO_HISTO=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
        END IF
        REDUCED_SET=FLGVAL('VERIFY')
        CALL EZGET_i('HISTO_REDUCED_SET',I,IERR)
C        if(doprint)write(lout,*)' in ttraks,reduce,i',i,' ierr',ierr
        IF(IERR.EQ.0)THEN
          CALL UHTOC(I,3,C3,3)
          REDUCED_SET=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
C          if(doprint)write(lout,*)' in ttraks i=',i,' c3',c3,' reduced_set',
C     &      REDUCED_SET
        END IF
        CALL EZGET_l('DO_ANALYSIS', CALL_TRD_ANALYSIS,IERR)
        CALL EZRSET
        NEVOLD=0
      END IF
C
      DOPRINT=TRD_DO_PRINT()
      IF(.NOT.CALL_TRD)GO TO 999
      DO_ALL=.FALSE.
      DO_TRACK=.FALSE.
      DO_PPHO=.FALSE.
      IF(ILINK(1).EQ.0)THEN ! no ZTRK selected
        IF(ILINK(2).EQ.0)THEN ! no PPHO selected: do all tracks
          DO_ALL=.TRUE.
        ELSE ! phho selected
          DO_PPHO=.TRUE.
        END IF
      ELSE! ZTRK selected
        DO_TRACK=.TRUE.
      END IF
      DOPRINT=TRD_DO_PRINT()
      IF(DOPRINT)THEN
        WRITE(LOUT,*) ' +------------+'
        WRITE(LOUT,*) ' |enter TTRAKS| with ilink',ILINK(1),
     &    ILINK(2),' ngoodt',NGOODT
        WRITE(LOUT,*) ' +------------+'
        WRITE(LOUT,*)'DO_ALL,DO_TRACK,DO_PPHO',DO_ALL,DO_TRACK,DO_PPHO
      END IF
      ILINK(3)=0
      NTOTEV=NTOTEV+1
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG('Header not booked','TTRAKS',' ','W')
        GO TO 1999
      ENDIF
C
C Do the following only once per event
C
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN !
        NGOODT = 0
        TMIN_CDC=1
        IF( COSMIC1)THEN
          CALL DTRGTM(TRGTIM)
          TMIN_CDC=TRGTIM+.5
        END IF
C
        CALL MZLINT(IXCOM,'provl',PROVL,LINKPR(1),LINKPR(2))
        LINKPR(1)=ILINK(1)
        LINKPR(2)=ILINK(2)
        CALL TRD_HITS
        NEVOLD=IQ(LHEAD+9)
        ILINK(1)=LINKPR(1)
        ILINK(2)=LINKPR(2)
        NGI=0
        PROVL(1)=0
      END IF
      TYPTRA=0
      IF(DO_PPHO) THEN  ! Cacl or Ppho
        LINKTR(1)=0
        LINKTR(2)=ILINK(2)
        LINKTR(3)=ILINK(3)
        IF(DOPRINT)WRITE(LOUT,*)' appel a trisrc_clust_phot'
        CALL TRISRC_CLUST_PHOT(LINKTR(1),LINKTR(2),LINKTR(3),IER)
      ELSE !deal with ZTRK tracks
        TYPTRA = ILINK(1)
        CALL TRATRD(TYPTRA)
      END IF
      IF(DOPRINT)WRITE(LOUT,*)' In TTRAKS,NB. of tracks in TRD',NGOODT,
     &  ' gztrdt',GZTRDT()
      IF(NGOODT.LE.0)THEN
        CALL ERRMSG('No TRD tracks','TTRAKS',' ','W')
        GO TO 1999
      END IF
C  Create a temporary link area
      IF(FILL_TLYR)
     +CALL MZLINT(IXCOM,'/TRTOBN/',TRTOBN,TLPINF(NTRM,6,NMAXBN),TRTOBN)
      DOPRINT=DOPRINT.AND.TNEVDG.GT.0
      IF (DOPRINT) WRITE(LOUT,*)' NB. of tracks in TRD',NGOODT
      IF(NGOODT.GT.TTRAKG)THEN
        CALL ERRMSG('Too many TRD tracks','TTRAKS',
     &                        'Nb. of tracks reduced','W')
        NGOODT=TTRAKG
      END IF
      GOODTR=0
      IF(DOPRINT)WRITE(LOUT,*)
     &  ' in ttraks, before loop 170, ngi+1,ngoodt',NGI+1,NGOODT
      DO 170 ITRA=NGI+1,NGOODT
        CALL TRGVER(ITRA) ! histogr. geometry
        IF(DOPRINT)WRITE(LOUT,*)' after trgver,itra',ITRA,' gztrdt',
     &    GZTRDT()
        IDENT=0
        IF(DO_TRACK.OR.DO_PPHO)THEN
C  Fill the FADC and analyse only the "selected track" if ILINK ne. 0
          IF(DOPRINT)WRITE(LOUT,*)' dans ttraks,ident',
     &      JBIT(IDENTR(ITRA),4)
          IF(JBIT(IDENTR(ITRA),4).LE.0)GO TO 170
          IF(ILINK(2).NE.0)IDENT=1
          GOODTR=ITRA
        END IF
        IF(DOPRINT)WRITE(LOUT,*)' on fait l analyse trd pour la trace',
     &    ITRA
C  Fill the FADC bins
        IF(DOPRINT)WRITE(LOUT,*)' before TRCFAD trgver,itra',ITRA,
     &    ' gztrdt',GZTRDT()
        CALL TRCFAD(ITRA)
C        IF(.NOT.FILL_TLYR)THEN !  Drop the bank TLYR
C          DO 166 ILAY=  1,  3
C            LZTRDH=GZTRDH()
C            IF(LZTRDH.LE.0)GO TO 166
C            LL=LQ(LZTRDH-ILAY)
C            IF(LL.LE.0)GO TO 166
C            CALL MZDROP(IXMAIN,LL,' ')
C  166     CONTINUE
C        END IF
        IF(DOPRINT)WRITE(LOUT,*)' before TRDANA,itra',ITRA,' gztrdt',
     &    GZTRDT()
        IF(DOPRINT)WRITE(LOUT,*)' in tttraks, call trdana for track',
     &    ITRA
        CALL TRDANA(IDENT,ITRA)   ! Analyse the information
C
C **** Finds the link to TRDT bank
        LTRDT=GZTRDT()
        IF(LTRDT.LE.0) THEN
          CALL ERRMSG(' LTRDT=0 ','TTRAKS',' ','W')
          GO TO 160
        END IF
        ILINK(3) = LTRDT
        IF(ILINK(1).NE.0)LQ(LTRDT-4)=ILINK(1)! Fill links for selected track
        IF(ILINK(2).NE.0 .)LQ(LTRDT-5)=ILINK(2)
        IF(DOPRINT)WRITE(LOUT,*)' in ttraks itra',ITRA,' ilink',ILINK
        IF(DOPRINT)WRITE(LOUT,*)' in ttraks, identr',JBIT(IDENTR(ITRA),
     &    7)
C        WRITE(LOUT,*)' in ttraks itra',ITRA,' ilink',ILINK
C        WRITE(LOUT,*)' in ttraks, identr',JBIT(IDENTR(ITRA),
C     &    7)
C  Find central full track and corresponding link for non-selected tracks
        IF(ILINK(1).EQ.0 .AND. ILINK(2).EQ.0)THEN
          IF(JBIT(IDENTR(ITRA),7).EQ.0)GO TO 160!Only full tracks
          ITRACK=IDENTR(ITRA)/128
          IF(ITRACK.EQ.0)GO TO 160
          LZTRH=GZZTRH()
          LZTRK=LQ(LZTRH-IZZTRK)
          IF (LZTRK.NE.0) LZTRK=LZFIND(IXCOM,LZTRK,ITRACK,-5)
          LQ(LTRDT-4)=LZTRK
          LQ(LTRDT-5)=0
          LQ(LZTRK-9)=LTRDT
        END IF
  160   CONTINUE
        IF(DOPRINT)
     +    WRITE(LOUT,*)' after 160, LQ(LTRDT-4),LQ(LTRDT-5)',
     &    LQ(LTRDT-4),LQ(LTRDT-5)
        Q(LTRDT+30)=0
        IF(LTRDT.LE.0)GO TO 170
        IF( CALL_TRD_ANALYSIS)THEN
          CALL TDSTDRP ! drop bank TDST and its dependance
          CALL TRD_ANALYSIS_IN_RECO(LTRDT,ACCEPTANCE,EFFICIENCY)
          IF(DOPRINT)WRITE(LOUT,*)
     +      'acceptance. efficiency' ,ACCEPTANCE, EFFICIENCY
          Q(LTRDT+30)=1.
          IF(ACCEPTANCE)Q(LTRDT+31)=1.
          Q(LTRDT+32)=EFFICIENCY
        END IF
  170 CONTINUE
      NGI=NGOODT
      IF(GZTRDT().LE.0)THEN
        CALL ERRMSG('Cant find TRDT','TTRAKS',' ','W')
        GO TO 1999
      END IF
      IF(TRTOBN(1).NE.0)TRTOBN(1)=0
      IF(DO_HISTO .AND. .NOT. MCDATA)THEN
        CALL DIFWIRE ! Plot difference of coded wires in
                     !  different layers
        CALL TRH_OUT
      END IF
      CALL TRH_ON
      IF(DOPRINT)WRITE(LOUT,*)' befire TMULAY gztrdt',
     &    GZTRDT()
      CALL TMULAY                 ! Compute multiplicities in the cells
      TNEVDG=TNEVDG-1
      IF(DOPRINT .AND. NGOODT.GE.1 )THEN
        ITRA=0
        CALL PRTRDT(LOUT,LTRDT,1,'ALL',0) ! Print bank TRDT
      END IF
 1999 CONTINUE
      LL=0
c      IF(DOPRINT)THEN
c        LTHIT=GZTHIT()
c        IF(LTHIT.NE.0)THEN
c          WRITE(LOUT,*)' nb. of words in THIT',IQ(LTHIT-1)
c        ELSE
c          WRITE(LOUT,*)' Bank THIT is not filled'
c        END IF
c      END IF
      IF(DOPRINT) WRITE(LOUT,*) ' Exit TTRAKS '
C Reduced set of histograms for reconstruction
      IF(REDUCED_SET) CALL THIST_VER
  999 CONTINUE
      IF(DOPRINT)WRITE(LOUT,'(a30,/,a30)')' exitr TTRAKS',
     &    ' ----------------'
      RETURN
      END
