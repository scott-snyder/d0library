      FUNCTION QCD_MAKE_DST()
      ENTRY QCD_MAKE_STA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Construct the 'standard' QCD DST from this event
C-
C-      Added: Contribute to D0 MDST as well
C-   ENTRY SET_QCD_MAKE_DST( DO_D0) : Force us to contribute to D0 mdst instead
C-
C-   ENTRY UNP_QCD_MAKE_DST         : Unpack D0MDST portion
C-
C-   ENTRY QCD_MAKE_STA() : Construct the 'standard' QCD STA
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-DEC-1992   Richard V. Astur
C-   Updated   8-AUG-1993   Brent J. May - added LTRK, PLV0, VERT banks
C-   Updated  20-AUG-1993   Brent J. May - added call to remake PLV0 if needed
C-   Updated  23-NOV-1994   Ulrich Heintz - changed ZSHUNT for CAID to MZCOPY 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZANLS.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAEQ.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAID.LINK/LIST'
      INCLUDE 'D0$LINKS:IZJTCS.LINK/LIST'
      LOGICAL  QCDJETS
      EXTERNAL QCDJETS
      INTEGER GZANLS, LANLS, GZCATD, LCATD, LBANK, LCAEQ, GZCAEP, GZRECO
      INTEGER LCAID, GZCAID, GZMDST, GZCAEQ, GZJUTL, LJUTL, LCAID2
      INTEGER  GZPROC, LCAPH, GZCAPH, LJTCS
      EXTERNAL GZANLS, GZCATD, GZCAEP, GZRECO, GZCAID, GZMDST, GZCAEQ
      EXTERNAL GZJUTL, GZPROC, GZCAPH
      INTEGER I, NUM_HEAD_STRUCT_LINKS
      INTEGER GZTRGR
      INTEGER IVERS, IPASS
      LOGICAL D0MDST, DO_D0, SET_QCD_MAKE_DST, UNP_QCD_MAKE_DST
      SAVE    D0MDST
      DATA    D0MDST/ .FALSE./
      LOGICAL QCD_MAKE_DST, QCD_MAKE_STA
      LOGICAL FIRST, STA_STREAM
      REAL CON, TEMPLATE(6)
      INTEGER NJ,NR, POINT, IER
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C: We will make the QCD specialized DST/STA which is described in
C: PRJ$ROOT:[QCD_1.NTUPLES.DOC]QCD_DST_STA_FORMAT.DOC.  All final
C: results hang from the ANLS bank
C
C: For both QCD and D0 MDST, need HEAD, ANLS and RECO banks
C
      IF ( LHEAD .LE. 0 ) THEN
        CALL ERRMSG('No HEAD','QCD_MAKE_DST',
     &    ' No HEAD bank in this event', 'W')
        RETURN      ! Skip if no event structure
      ENDIF

      IF ( GZRECO() .LE. 0 ) THEN
        CALL ERRMSG('No RECO','QCD_MAKE_DST',
     &    ' No RECO bank in this event ', 'W' )
        RETURN
      ENDIF

C
C: D0MDST - Use existing ANLS
C: QCDMDST- Drop ANLS to start. Then book it.
C
      LANLS = GZANLS()
      IF ( D0MDST ) THEN
        IF ( LANLS .LE. 0 ) CALL ERRMSG('NO ANLS','QCD_MAKE_DST',
     &    'Need ANLS bank-abort','F')
C
C: Push if needed
C
        IF ( LANLS .GT. 0 ) THEN
          IF ( IQ( LANLS - 2) .LT. 5 ) CALL MZPUSH( IXCOM, LANLS, 1, 0,
     &      'I' )
        ENDIF
      ELSE
        IF ( LANLS .GT. 0 ) CALL MZDROP( IXCOM, LANLS, 'L' )
        CALL BKANLS( LANLS )
      ENDIF
C
C: Determine if the input file is an STA, i.e. has a TRGR bank
C
      STA_STREAM = ( GZTRGR() .GT. 0 )
C
C: QCDMDST ONLY:
C: For RECO < V11 we need to remake PLV0 bank to get the correct info
C: Must be before MDSTFL and PLV0_TO_MDST
C
      IF ( STA_STREAM .AND. (.NOT. D0MDST) ) THEN
        CALL RECO_VERSION( IVERS, IPASS )
        IF ( IVERS .LT. 11) CALL REDO_PLV0
      ENDIF
C
C: QCDMDST:
C: We will hang the following three banks  underneath ANLS: MDST, CATD and
C: JUTL.  Packed tracking, PLV0 and VERT bank hang off MDST.
C
      IF ( .NOT. D0MDST ) THEN
        CALL MDSTFL                   ! Book and fill microdst bank
C
C: Micro STA banks if STA input else Micro DST
C
        IF ( STA_STREAM ) THEN
          CALL LTRKFL                   ! Book and fill little tracking bank
          CALL PLV0_TO_MDST             ! Copy PLV0 bank to MDST link
          CALL VERT_TO_MDST             ! Copy VERT bank to MDST link
          CALL CAEQFL                     ! Make CAEQ bank shunt to ANLS
          LCAEQ = GZCAEQ()              ! Now under PROC
          IF ( LCAEQ .GT. 0 ) CALL ZSHUNT( IXCOM, LCAEQ, GZANLS(), -2, 0
     &      )
        ELSE
          LCATD = GZCATD()                ! Make CATD bank shunt to ANLS
          IF ( LCATD .GT. 0 ) THEN
            CALL ZSHUNT( IXCOM, LCATD, GZANLS(), -2, 0 )
          ENDIF
        ENDIF
C
C: Move the CAID bank under MDST
C
        LCAID = GZCAID()
        IF ( LCAID .GT. 0 ) THEN
          CALL ZSHUNT( IXCOM, LCAID, GZANLS(), -5, 0 )
        ENDIF
C
C: Book and fill the JUTL bank
C
        CALL JUTLFL(D0MDST )

C
C: D0MDST - Book and fill QCD JUTL, hang CAID under JUTL
C
      ELSE
C
C: Book and fill the JUTL bank
C
        IF ( QCDJETS() ) CALL JUTLFL(D0MDST )
C
C: Move the CAID bank under MDST
C
        LCAID = GZCAID()
        IF ( LCAID .GT. 0 ) THEN
          CALL ZSHUNT( IXCOM, LCAID, GZANLS(), -5, 0 )
        ENDIF
      ENDIF
C
C: QCDMDST - drop everything ourselves
C: We have what we want, now drop everything else.
C
      IF ( .NOT. D0MDST ) THEN
        NUM_HEAD_STRUCT_LINKS = IQ( LHEAD - 2 )
        DO I = 1, NUM_HEAD_STRUCT_LINKS
          IF ( I .NE. ABS( IZANLS ) ) THEN       ! Dont drop ANLS!!
            LBANK = LQ( LHEAD - I )
            IF ( LBANK .GT. 0 ) CALL MZDROP( IXCOM, LBANK, 'L' )
          ENDIF
        ENDDO
      ENDIF
C
C: Debug
C
c      IF ( FIRST ) THEN
c        CALL HBOOK1(973, ' ', 100, 0., 1000., 0. )
c        CALL HBOOK1(974, ' ', 100, 0., 1000., 0. )
c        CALL HBOOK1(975, ' ', 100, 0., 1000., 0. )
c        FIRST = .FALSE.
c      ENDIF
c      LANLS = GZANLS()
c      DO I = 1,3
c      LBANK = LQ( LANLS - I)
c      IF ( LBANK .GT. 0) CALL HFILL( 972+I, FLOAT(IQ(LBANK-1)), 0., 1.)
c      ENDDO
      QCD_MAKE_DST = .TRUE.
      QCD_MAKE_STA = .TRUE.
  999 RETURN
C
C: Entry point to set us into D0 MDST making mode instead of QCD MDST
C
      ENTRY SET_QCD_MAKE_DST( DO_D0 )
      D0MDST = DO_D0                ! If TRUE, make D0 mdst
      SET_QCD_MAKE_DST = .TRUE.
      RETURN
C
C: Unpack QCD portion of the D0 MDST
C
      ENTRY UNP_QCD_MAKE_DST
      UNP_QCD_MAKE_DST = .TRUE.
C
C: Need both ANLS and PROC bank
C
      LANLS = GZANLS()
      IF ( LANLS .LE. 0 ) THEN
        CALL ERRMSG('No ANLS','UNP_QCD_MAKE_DST',
     &    'Cant find ANLS bank-cannot unpack','F')
        RETURN
      ENDIF
      IF ( GZPROC() .LE. 0 ) THEN
        CALL ERRMSG('No PROC','UNP_QCD_MAKE_DST',
     &    'Cant find PROC bank-cannot unpack','F')
        RETURN
      ENDIF
C
C: Move CAID bank back to its usual place
C
      IF ( IQ(LANLS-2) .GT. 4 ) THEN
        LCAID = LQ( LANLS - 5 )                   ! Got one we want
        LCAID2= LQ( GZPROC() - IZCAID )           ! Where we want to put it
        IF ( LCAID2 .GT. 0 ) THEN
          CALL ERRMSG('CAID THERE','UNP_QCD_MAKE_DST',
     &      'CAID ALREADY PRESENT','W')
        ELSEIF ( LCAID .GT. 0 ) THEN
          CALL MZCOPY(IXCOM,LCAID,IXMAIN,GZPROC(),-IZCAID,' ')
        ENDIF
      ELSE
        CALL ERRMSG('OLD ANLS','UNP_QCD_MAKE_DST',
     &    'No CAID could be present, ANLS is too small','W')
      ENDIF
C
C: Now move JTCS as well. It is at the end of JUTL. Find it.
C: But will only be there on QCD jet events
C
      IF (QCDJETS() ) THEN
        LJUTL = LQ( GZANLS() - 1 )
        IF ( LJUTL .LE. 0) THEN
          CALL ERRMSG('NO JUTL','QCD_MAKE_DST','No JUTL in this event',
     &      'W')
          RETURN
        ENDIF
C
C: Forget first 3 header words
C
        POINT = LJUTL + 3
C
C: Readout L1,LT words
C
        POINT = POINT + 3*Q(POINT+1)
        POINT = POINT + 1
        POINT = POINT + 3*Q(POINT+1)
        POINT = POINT + 1
        POINT = POINT + 5*Q(POINT+1)
        POINT = POINT + 1
        POINT = POINT + 1             ! Cone size
C
C: Rest of bank is JTCS starting with word 2
C
        IF ( POINT + 1 .GT. IQ(LJUTL-1) + LJUTL ) THEN
          CALL ERRMSG('MISSING JTCS','UNP_QCD_MAKE_DST',
     &      'No JTCS in JUTL', 'W' )
        ELSE
          NR  = Q( POINT + 1 )         ! # of rings + 3
          CON = .1*(NR-3)              ! cone size
          NJ  = Q( POINT + 2 )          ! # of jets
          POINT = POINT + 2
C
C: Find CAPH to hang under
C
          TEMPLATE(1) = 1.0
          TEMPLATE(2) = 6.0
          TEMPLATE(3) = CON
          CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
          IF ( IER .NE. 0) THEN
            CALL ERRMSG('NO ALG MATCH','UNP_QCD_MAKE_DST',
     &        'CANT FIND CAPH TO HANG JTCS','W')
            CALL RESET_CAPH
            RETURN
          ENDIF
          LCAPH = GZCAPH()
          CALL RESET_CAPH

          LJTCS = LQ( LCAPH - IZJTCS )
          IF ( LJTCS .GT. 0 ) THEN
            CALL ERRMSG('JTCS THERE','UNP_QCD_MAKE_DST',
     &        'JTCS ALREADY PRESENT','W' )
            RETURN
          ENDIF
C
C: Book JTCS and fill
C
          CALL BKJTCS( LCAPH, NJ*NR+3, LJTCS )
          CALL UCOPY( Q(POINT+1), Q(LJTCS+4), NJ*NR)
          IQ( LJTCS + 2 ) = NR
          IQ( LJTCS + 3 ) = NJ
        ENDIF
      ENDIF
      RETURN
      END
