      SUBROUTINE UDST_TO_DST_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : re-create DST tree and fill from UDST and UCSH
C-                         banks
C-
C-   Controls:  TAGS_MAP_RCP
C-
C-   Created  13-DEC-1993   Sailesh Chopra
C-   Updated   5-JAN-1994   Ulrich Heintz  added PMUO->VERT link, call BKDTRK,
C-                          protect links in ZLINKC
C-   Updated  12-JAN-1994   Ulrich Heintz  add PMUO,PELC,PPHO,PNUT,PTAU counts
C-                          in PARH
C-   Updated  13-JAN-1994   Sailesh Chopra add ZLINKC_INI flag
C-   Updated  12-MAR-1994   Ulrich Heintz  issue 'target of link ->VERT' error
C-                                         only if at least one vertex in event
C-   Updated  17-OCT-1994   Ulrich Heintz  unpack VTXT,VFIT,TRDT banks
C-   Updated  13-NOV-1995   Ian Adam  PNU1 bank
C-   Updated  17-NOV-1995   Ulrich Heintz  add two jets algorithms (JETF,JETA)
C-   Updated  17-NOV-1995   Ulrich Heintz  treat filling if IQ(LBANK+0) 
C-   Updated  18-NOV-1995   Ulrich Heintz  clean up the UDST_FIX_xxxx section 
C-   Updated  18-NOV-1995   Ulrich Heintz  increase PELC/PPHO bank size to 47 
C-   Updated  11-DEC-1995   Ian Adam  Fill bookkeeping info into PNU1 
C-                                    since it isn't done in PNU1
C-   Updated  11-DEC-1996   Ulrich Heintz  fix pointer to VCORPX, VCORPY 
C-  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:UTAG_DATA.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INTEGER GZPROC,GZPARH,GZHSTR,GZGLOB,GZPLV0,GZPNUT,GZPELC,GZPPHO
      INTEGER GZTTRH,GZVTRH,GZFTRH,GZMTRH,GZZTRH,GZJETS,GZVERH,GZVERT
      INTEGER GZVCOR,GZDTRH,GZUCSH,GZZTRK,GZCAPH,GZVTXT,GZVFIT
      INTEGER LADDR,LPTAU,LVERT,LMUOT,LVCOR,LPMUO,LFDCT,LTRDT,LVFIT
      INTEGER LUCSH,LGLOB,LPLV0,LDTRK,LTTRH,LVTRH,LVTXT
      EQUIVALENCE(CSTLNK(60),LDTRK)
      EQUIVALENCE(CSTLNK(61),LFDCT)
      EQUIVALENCE(CSTLNK(62),LGLOB)
      EQUIVALENCE(CSTLNK(63),LPLV0)
      EQUIVALENCE(CSTLNK(64),LPTAU)
      EQUIVALENCE(CSTLNK(65),LMUOT)
      EQUIVALENCE(CSTLNK(66),LPMUO)
      EQUIVALENCE(CSTLNK(67),LTTRH)
      EQUIVALENCE(CSTLNK(68),LVTRH)
      EQUIVALENCE(CSTLNK(69),LVTXT)
      EQUIVALENCE(CSTLNK(70),LTRDT)
      EQUIVALENCE(CSTLNK(71),LVFIT)
      EQUIVALENCE(CSTLNK(72),LVERT)
      LOGICAL FLGCHK,FLGVAL,FOUND
C
      REAL MR_VECET(6)
      INTEGER NTAU_TRK,I,J,K,ND,ITEMP,L,N,ELINK,PLINK
      INTEGER IT,LINK_JET,II,GET_INDEX,IER,IPTR,IOBJECT_ADDR,LINK_PTR
      INTEGER NCELLS,IVCOR,IGLOB,IZFIT,IPOL
      INTEGER IVERT,IPMUO,IPELC,IPPHO,IHMTE,IHMTP,IPNUT,IPTAU,ICACL
      INTEGER ICASH,IJETS,IJNEP,ID_TRAK,ID_VERT,ID_VTXT,ID_GLOB
      INTEGER ID_PNU1
      INTEGER I_VERSION1,I_RECO,I_MAGPOL,NCONE,NCONE_JNEP
      INTEGER I_VCORPX,I_VCORPY,I_VCORPX1,I_VCORPY1
      PARAMETER( NCONE = 8,NCONE_JNEP = 3 )
      CHARACTER*1 CONE(NCONE_JNEP),PROD_NAME*40,JET_NAME*8
      DATA CONE/'7','5','3'/
      INTEGER NUM_PNU1,LPNU1
C
      REAL    TEMPLATE(7,NCONE+1),CAPH_DATA(10,NCONE+2)
      DATA    TEMPLATE/ 1.,6.,0.7,0.,0.,0.,0.,      ! CONE R=0.3
     &                  1.,6.,0.5,0.,0.,0.,0.,      ! CONE R=0.5
     &                  1.,6.,0.3,0.,0.,0.,0.,      ! CONE R=0.7
     &                  1.,6.,1.0,0.,0.,0.,0.,      ! CONE R=1.0
     &                  3.,6.,0.7,8.,1.,9.,0.35,    ! CONE R=0.7  NOMERGE
     &                  2.,6.,0.7,10.,0.3,0.,0.,    ! R=0.7 forward
     &                  1.,6.,-.7,0.,0.,0.,0.,      ! R=0.7 D0 angles
     &                  1.,6.,-1.,0.,0.,0.,0.,      ! NN
     &                  0.,0.,0.,0.,0.,0.,0./       ! CAPHEL
      DATA CAPH_DATA/ 0.7,8.,0.5,0.01,1.0,1.0,1.,0.,0.,0.,   ! CONE R=0.7
     &                0.5,8.,0.5,0.01,1.0,1.0,1.,0.,0.,0.,   ! CONE R=0.5
     &                0.3,8.,0.5,0.01,1.0,1.0,1.,0.,0.,0.,   ! CONE R=0.3
     &                1.0,8.,0.5,0.01,1.0,1.0,1.,0.,0.,0.,   ! CONE R=1.0
     &                0.7,8.,1. ,0.35,1.0,1.0,1.,0.,0.,0.,   ! CONE R=0.7 NMG
     &                0.7,8.,0.5,0.01,0.3,0.0,3.,0.,0.,0.,   ! R=0.7 forward
     &               -0.7,8.,0.5,0.01,1.0,1.0,1.,0.,0.,0.,   ! R=0.7 D0 angles
     &                -1.,2.,2. ,0.5 ,0.5,0.5,0.,8.,1.,255., ! NN 2x2
     &                0. ,0.,0. ,0.  ,0. ,0. ,0.,0.,0.,0.,   ! CAPHEL
     &                -1.,2.,2. ,0.05,-.8,1.0,0.,5.,1.,255./ ! NN (RECO>12.11)
C-
C-DTRK TO FDCT MAP
C-
      INTEGER IFDC(27)

      DATA IFDC/
C- DTRK 1,2,3,4,5,6,7,8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
     &  0,0,0,0,0,6,4,5,22, 0, 0,19, 0,25,31,23, 0,24, 0,20, 0, 0, 0, 0,
C- DTRK 25,26,27
     &   0, 0, 0/

      REAL Z0
      LOGICAL BOOK_BANKS,BOOK_DTRK
      LOGICAL FIRST,IF_JETS
      DATA FIRST/.TRUE./,IF_JETS/.FALSE./
C---------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        IF (.NOT. FLGCHK('ZLINKC_INI'))THEN
          CALL FLGBK('ZLINKC_INI',1)
        ENDIF
        IF (.NOT. FLGVAL('ZLINKC_INI'))THEN
          CALL MZLINK(IXCOM,'/ZLINKC/',CSTLNK,CRFLNK,CRFLNK(LNKMX))
          CALL FLGSET('ZLINKC_INI',.TRUE.)
        ENDIF
      ENDIF
C
      IF (FLGVAL('NEW_UTAG'))THEN
        DO I=1,IDMAX
          IF(XGRP1(I) .EQ. 'TRAK')ID_TRAK=I
          IF(XGRP1(I) .EQ. 'VERT')ID_VERT=I
          IF(XGRP1(I) .EQ. 'VTXT')ID_VTXT=I
          IF(XGRP1(I) .EQ. 'GLOB')THEN
            ID_GLOB=I
            DO J=1,NTAGS_GRP(I)
              IF(INDEX(TAGS_MAP(1,J,I),'VERSION1').GT.0)I_VERSION1=J
              IF(INDEX(TAGS_MAP(1,J,I),'RECO').GT.0)I_RECO=J
              IF(INDEX(TAGS_MAP(1,J,I),'MAGPOL').GT.0)I_MAGPOL=J
              IF(INDEX(TAGS_MAP(1,J,I),'VCORPX ').GT.0)I_VCORPX=J
              IF(INDEX(TAGS_MAP(1,J,I),'VCORPY ').GT.0)I_VCORPY=J
              IF(INDEX(TAGS_MAP(1,J,I),'VCORPX1').GT.0)I_VCORPX1=J
              IF(INDEX(TAGS_MAP(1,J,I),'VCORPY1').GT.0)I_VCORPY1=J
            ENDDO
          ENDIF
          IF(XGRP1(I) .EQ. 'PNU1')ID_PNU1=I
        ENDDO
      ENDIF
C
C... check RECO version
C
      IF(UDST_DATA(I_RECO,ID_GLOB).LT.11)THEN
        CALL ERRMSG('RECO_VERSION<11','UDST_TO_DST_FILL',
     &    'this code is not supported for RECO versions < 11','W')
      ENDIF
C
C... get bank version numbers from UDST
C
      IF(ID_GLOB.GT.0.AND.I_VERSION1.GT.0)THEN
        CALL UDST_UNPACK_VERSION(UDST_DATA(I_VERSION1,ID_GLOB),
     &    IVCOR,IGLOB,IZFIT,IVERT,IPMUO,IPELC,IPPHO,IHMTE,IHMTP,IPNUT,
     &    IPTAU,ICACL,ICASH,IJETS,IJNEP)
      ELSE
        CALL ERRMSG('NO_VERSIONS','UDST_TO_DST_FILL',
     &    'version numbers not found in UDST','W')
      ENDIF
C
C ****  Book/Fill top level DST banks under current path
C
      LADDR = GZPROC()
      IF (LADDR.LE.0) CALL BKPROC(LADDR)
      LPROC=LADDR
C
      LADDR = GZPARH()
      IF (LADDR.LE.0) CALL BKPARH(LADDR)
      LPARH=LADDR
C-
C-***** Book HSTR bank
C-
      LHSTR = GZHSTR()
      IF (LHSTR.LE.0) CALL BKHSTR(LHSTR)
C-
C-***** Book GLOB bank and main ring VCOR banks
C-
      LGLOB = GZGLOB()
      IF(LGLOB.LE.0)CALL BKGLOB(LGLOB)
      IQ(LGLOB+1)=IGLOB
      MR_VECET(1)=UDST_DATA(I_VCORPX,ID_GLOB)
      MR_VECET(2)=UDST_DATA(I_VCORPY,ID_GLOB)
      CALL VCORFL(0,'MRFX',6,MR_VECET,LVCOR)
      LQ(LGLOB-2)  = LVCOR ! Reference link to VCOR
      IF(IGLOB.GE.6)THEN
        MR_VECET(1)=UDST_DATA(I_VCORPX1,ID_GLOB)
        MR_VECET(2)=UDST_DATA(I_VCORPY1,ID_GLOB)
        CALL VCORFL(0,'MRFN',6,MR_VECET,LVCOR)
        LQ(LGLOB-3)  = LVCOR ! Reference link to VCOR
      ENDIF
C-
C-***** Book MTRH bank
C-
      LMTRH = GZMTRH(0)
      IF(LMTRH.LE.0) CALL BKMTRH(0,0,LMTRH)
      IPOL=INT(UDST_DATA(I_MAGPOL,ID_GLOB))
      CALL UDST_PUT_MAGPOL(IPOL)
C-
C-***** Book TTRH bank
C-
      LTTRH = GZTTRH(0)
      IF(LTTRH.LE.0) CALL BKTTRH(LTTRH)
C-
C-***** Book PLV0 bank
C-
      LPLV0 = GZPLV0()
      IF(LPLV0.LE.0) CALL BKPLV0(LPLV0)
C-
C-***** Book VERH bank
C-
      LVERH = GZVERH()
      IF(LVERH.LE.0) CALL BKVERH(LVERH)
C-
C-***** book VTRH bank
C-
      LVTRH = GZVTRH()
      IF(LVTRH .LE. 0) CALL BKVTRH(LVTRH)
C-
C-Book all the CAPH banks
C-
      DO I=NCONE+1,1,-1
        CALL BKCAPH(LCAPH)
        IF(I.LT.NCONE) CALL CAPHFL_INT(K_ALGORITHM,A_CONE_JET)
        IF(I.EQ.NCONE) CALL CAPHFL_INT(K_ALGORITHM,A_NN_JET)
        IF(I.GT.NCONE) CALL CAPHFL_INT(K_ALGORITHM,A_ELECTRON)
        DO J=1,10
          CALL CAPHFL_REAL(K_BASE+J,CAPH_DATA(J,I))
        ENDDO
        IF(I.EQ.NCONE.AND.UDST_DATA(I_RECO,ID_GLOB).GE.12.11)THEN
          DO J=1,10                   ! new nearest neighbor algorithm
            CALL CAPHFL_REAL(K_BASE+J,CAPH_DATA(J,NCONE+2))
          ENDDO
        ENDIF
      ENDDO
C-
C- Handle PNU1 bank, special case

      IF (ID_PNU1.GT.0) THEN
        K = ID_PNU1
        NUM_PNU1 = NOBJ(K)
        CALL BKPNU1(NUM_PNU1,LPNU1)
        IQ(LPNU1+1) = 1
        IQ(LPNU1+2) = 11
        IQ(LPNU1+3) = NUM_PNU1
        IF (LPNU1.GT.0) THEN
          DO J=1, NOBJ(K)
            I=1
            LADDR = LPNU1 + (J-1)*IQ(LPNU1+2)
            DO WHILE (I.LE.NTAGS_GRP(K))
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
              I=I+1
            ENDDO
          ENDDO
        ELSE
          CALL ERRMSG('BKPNU1 FAILURE','UDST_TO_DST_FILL',' ','W')
        ENDIF
      ENDIF
C-
C- book the VERT banks
C-
      IF(ID_VERT.GT.0)THEN
        K=ID_VERT
        DO J = 1,NOBJ(K)
          BOOK_BANKS = .TRUE.
          I=1
          DO WHILE (I .LE. NTAGS_GRP(K))
            LADDR=0
            IF (BANK_NAME(I,K) .EQ. 'NULL')GOTO 21
            IF(BOOK_BANKS)THEN
              BOOK_BANKS = .FALSE.
              CALL BKVERT(LVERT,0)
              LADDR = LVERT
              IQ(LVERT+1)=IVERT
              IQ(LVERT-5)=J
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'VERT') LADDR =  GZVERT(J)
C-
C-FILL IN THE BANKS
C-
            IF (LADDR .NE. 0)THEN
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
C              IF( IOR(I,K) .EQ. 'B')I=I+1
            ENDIF
   21       CONTINUE
            I=I+1
          ENDDO
        ENDDO
      ENDIF
C-
C- book the VTXT banks
C-
      IF(ID_VTXT.GT.0)THEN
        K=ID_VTXT
        DO J = 1,NOBJ(K)
          BOOK_BANKS = .TRUE.
          I=1
          DO WHILE (I .LE. NTAGS_GRP(K))
            LADDR=0
            IF (BANK_NAME(I,K) .EQ. 'NULL')GOTO 22
            IF(BOOK_BANKS)THEN
              BOOK_BANKS = .FALSE.
              CALL BKVTXT(LVTXT)
              LADDR = LVTXT
              IQ(LVTXT-5)=J
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'VTXT') LADDR =  GZVTXT(J)
C-
C-FILL IN THE BANKS
C-
            IF (LADDR .NE. 0)THEN
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
C              IF( IOR(I,K) .EQ. 'B')I=I+1
            ENDIF
   22       CONTINUE
            I=I+1
          ENDDO
        ENDDO
      ENDIF
C-
C- book all other tracking banks
C-
      IF(ID_TRAK.GT.0)THEN
        K=ID_TRAK
        DO J=1, NOBJ(K)
          BOOK_BANKS=.TRUE.
          BOOK_DTRK = .TRUE.
          I=1
          Z0=0
          DO WHILE (I .LE. NTAGS_GRP(K))
            LADDR = 0
            IF(BANK_NAME(I,K) .EQ. 'NULL')GOTO 20
            IF(BOOK_BANKS)THEN
              BOOK_BANKS = .FALSE.
              CALL BKZTRK(LZTRK)
              CALL BKZFIT(LZTRK,LZFIT)
              IQ(LZFIT+1)=IZFIT
              IQ(LZFIT-5)=J
              LZTRH=GZZTRH()
              IQ(LZTRH+2)=IQ(LZTRH+2)+1 ! count ZTRKs
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'DTRK' .AND. Z0 .EQ. 0.)THEN
              II = 1
              DO WHILE (II .LE. NTAGS_GRP(K) .AND.
     &          INDEX(TAGS_MAP(2,II,K), 'LDTRK+11') .EQ. 0)
                II = II+1
              ENDDO
              IF(II .GT. NTAGS_GRP(K) )THEN
                CALL ERRMSG('word not found','UDST_TO_DST_FILL',
     &            'z of track','F')
              ELSE
                Z0=UDST_DATA((J-1)*NTAGS_GRP(K)+II,K)
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'DTRK')THEN
              IF (Z0 .EQ. 0)THEN ! vertex only track
                LADDR=0
              ELSEIF (ABS(Z0) .LT. 105.3460 )THEN
                IF(BOOK_DTRK)THEN
                  BOOK_DTRK = .FALSE.
                  CALL BKDTRK(LDTRK)
                  LQ(LZTRK-7) = LDTRK
                  LQ(LDTRK-2) = LZTRK
                ENDIF
                LADDR=LDTRK
              ELSE
                IF (BOOK_DTRK)THEN
                  BOOK_DTRK = .FALSE.
                  CALL BKFDCT(LFDCT)
                  IF(Z0.GT.0)IQ(LFDCT+1)=IQ(LFDCT+1)+2**0
                  LQ(LZTRK-8) = LFDCT
                  LQ(LFDCT-2) = LZTRK
                ENDIF
                LADDR=LFDCT
                ITEMP = IELE(I,K)
                IELE(I,K) = IFDC(ITEMP)
              ENDIF
            ENDIF
            IF (BANK_NAME(I,K) .EQ. 'ZFIT')LADDR=LZFIT
            IF (BANK_NAME(I,K) .EQ. 'ZTRK')LADDR=LZTRK
            IF (BANK_NAME(I,K) .EQ. 'ZTRH')LADDR=LZTRH
C-
C-FILL THE BANK
C-
            IF (LADDR .NE. 0)THEN
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
C              IF( IOR(I,K) .EQ. 'B')I=I+1
            ENDIF
            IF (BANK_NAME(I,K) .EQ. 'LINK') THEN
              IPTR = UDST_DATA((J-1)*NTAGS_GRP(K)+I,K)
              IF( LINK_BANK(I,K) .EQ. 'VERT' .AND. IPTR .NE. 0)THEN
                LVERT=GZVERT(IPTR)
                IF (LVERT .LE. 0 .AND. NOBJ(ID_VERT).GT.0 )
     &            CALL ERRMSG('target of link not found',
     &            'UDST_TO_DST_FILL','ZTRK->VERT','W')
                LQ(LZTRK-2)=LVERT
              ELSEIF ( LINK_BANK(I,K) .EQ. 'VTXT' .AND. IPTR .NE. 0)THEN
                LVTXT=GZVTXT(IPTR)
                IF (LVTXT .LE. 0 .AND. NOBJ(ID_VTXT).GT.0 )
     &            CALL ERRMSG('target of link not found',
     &            'UDST_TO_DST_FILL','ZTRK->VTXT','W')
                LQ(LZTRK-6) = LVTXT
                LQ(LVTXT-2) = LZTRK
              ENDIF
            ENDIF
            IF(ABS(Z0) .GE. 100.0 .AND. BANK_NAME(I,K) .EQ. 'DTRK')
     &        IELE(I,K)=ITEMP
   20       CONTINUE
            I=I+1
          ENDDO
        ENDDO
      ENDIF
C-
C-JET BANKS
C-
      IF_JETS = .FALSE.
      DO 40 K=1,IDMAX
        IF(INDEX(XGRP1(K),'JET') .LE. 0 .AND.
     &     INDEX(XGRP1(K),'JNM') .LE. 0)GOTO 40
        DO J = 1,NOBJ(K)
          BOOK_BANKS=.TRUE.
          I=1
          IF_JETS = .TRUE.
          DO WHILE (I .LE. NTAGS_GRP(K))
            IER = 0
            LADDR = 0
            IF(BANK_NAME(I,K) .NE. 'NULL')THEN
              IF(BOOK_BANKS)THEN
                BOOK_BANKS = .FALSE.
                IF(XGRP1(K) .EQ. 'JET7')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
                ELSEIF(XGRP1(K) .EQ. 'JET5')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
                ELSEIF(XGRP1(K) .EQ. 'JET3')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
                ELSEIF(XGRP1(K) .EQ. 'JET1')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,4),IER)
                ELSEIF(XGRP1(K) .EQ. 'JNM7')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,5),IER)
                ELSEIF(XGRP1(K) .EQ. 'JETF')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,6),IER)
                ELSEIF(XGRP1(K) .EQ. 'JETA')THEN
                  CALL SET_CAPH('CONE_JET',TEMPLATE(1,7),IER)
                ELSEIF(XGRP1(K) .EQ. 'JETN')THEN
                  CALL SET_CAPH('NN_JET',TEMPLATE(1,8),IER)
                ELSE
                  CALL ERRMSG('UNKNOWN_JET_GROUP','UDST_TO_DST_FILL',
     &              XGRP1(K),'W')
                  GOTO 40
                ENDIF
                IF (IER .NE. 0)THEN
                  CALL ERRMSG('Error setting CAPH','UDST_TO_DST_FILL',
     &              'JETS','F')
                ENDIF
                CALL BKJETS(LJETS)
                IQ(LJETS+1)=IJETS
                IQ(LJETS-5)=J
                LCAPH=GZCAPH()
                IQ(LCAPH+3)=IQ(LCAPH+3)+1
                CALL RESET_CAPH
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'JETS')LADDR=LJETS
            IF(BANK_NAME(I,K) .EQ. 'CAPH')LADDR=LCAPH
C-
C-FILL THE BANK
C-
            IF (LADDR .NE. 0)THEN
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
C              IF( IOR(I,K) .EQ. 'B')I=I+1
            ENDIF
            I=I+1
          ENDDO
          CALL UDST_FIX_JETS(LJETS)
        ENDDO
   40 CONTINUE
C-
C-*******FILLING OTHER BANKS
C-
      DO K=1,IDMAX
        IF (XGRP1(K) .EQ. 'TRAK' ) GOTO 101
        IF (INDEX(XGRP1(K),'JET') .NE. 0 ) GOTO 101
        IF (INDEX(XGRP1(K),'JNM') .NE. 0 ) GOTO 101
        IF (XGRP1(K) .EQ. 'VERT' ) GOTO 101
        IF (XGRP1(K) .EQ. 'VTXT' ) GOTO 101
        IF (XGRP1(K) .EQ. 'PNU1' ) GOTO 101
        DO J=1,NOBJ(K)
          BOOK_BANKS = .TRUE.
          IOBJECT_ADDR=0
          I=1
          DO WHILE (I .LE. NTAGS_GRP(K))
            LADDR = 0
            IF(BANK_NAME(I,K) .EQ. 'NULL')GOTO 100
C-
C- book the VFIT banks
C-
            IF (XGRP1(K) .EQ. 'VFIT')THEN
              IF(BOOK_BANKS)THEN
                BOOK_BANKS = .FALSE.
                CALL BKVFIT(LVFIT)
                LADDR = LVFIT
                IQ(LVFIT-5)=J
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'VFIT') LADDR =  GZVFIT(J)
C-
C- ELEMENT IS FROM PELC
C-
            IF (XGRP1(K) .EQ. 'PELC')THEN
              IF (BOOK_BANKS) THEN
                BOOK_BANKS = .FALSE.
C- book PELC bank
                L = 0
                N = 47
                CALL BKPELC(L,N,LPELC)
                IQ(LPARH+3)=IQ(LPARH+3)+1   ! count PELC banks
                IQ(LPELC+1)=IPELC         ! version number
                IQ(LPELC+2)=12            ! BANK ID
                IQ(LPELC-5)=J
                IOBJECT_ADDR = LPELC
C- book CACL bank
                CALL SET_CAPH('ELECTRON',TEMPLATE(1,NCONE+1),IER)
                IF (IER .EQ. 0)THEN
                  CALL BKCACL(LCACL)
                  IQ(LCACL+1)=ICACL
                ELSE
                  CALL ERRMSG('Error setting CAPH','UDST_TO_DST_FILL',
     &              'CACL','F')
                ENDIF
                LQ(LPELC-2)=LCACL
                CALL RESET_CAPH
C- book HMTE bank
                CALL BKHMTE(LPELC,LHMTE)
                IQ(LHMTE+1)=IHMTE
C- book TRDT bank
                CALL BKTRDT(LTRDT)
                LQ(LTRDT-5)=LCACL
                IQ(LTRDT-5)=J ! label bank with PELC number for reference with
              ENDIF           ! UDST_VALUE
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'PELC')LADDR=LPELC
C-
C-PPHO BANK
C-
            IF (XGRP1(K) .EQ. 'PPHO')THEN
              IF (BOOK_BANKS) THEN
                BOOK_BANKS=.FALSE.
C- book PPHO bank
                L = 0
                N = 47
                CALL BKPPHO(L,N,LPPHO)
                IQ(LPARH+4)=IQ(LPARH+4)+1   ! count PPHO banks
                IQ(LPPHO+1)=IPPHO           ! version number
                IQ(LPPHO+2)=10              ! bank ID
                IQ(LPPHO-5)=J
                IOBJECT_ADDR = LPPHO
C- book CACL bank
                CALL SET_CAPH('ELECTRON',TEMPLATE(1,NCONE+1),IER)
                IF (IER .EQ. 0)THEN
                  CALL BKCACL(LCACL)
                  IQ(LCACL+1)=ICACL
                ELSE
                  CALL ERRMSG('Error setting CAPH','UDST_TO_DST_FILL',
     &              'CACL','F')
                ENDIF
                LQ(LPPHO-2)=LCACL
                CALL RESET_CAPH
C- book HMTP bank
                CALL BKHMTP(LPPHO,LHMTP)
                IQ(LHMTP+1)=IHMTP
C- book TRDT bank
                CALL BKTRDT(LTRDT)
                LQ(LTRDT-5)=LCACL
                IQ(LTRDT-5)=-J ! label bank with -PPHO number for reference with
              ENDIF           ! UDST_VALUE
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'PPHO')LADDR=LPPHO
C-
            IF (BANK_NAME(I,K) .EQ. 'CASH')THEN
              IF(IELE(I,K) .EQ. 2)THEN
                NCELLS=UDST_DATA((J-1)*NTAGS_GRP(K)+I,K)  ! MN(K).GRP(J).WRD(I)
                CALL BKCASH(LCACL,NCELLS,LCASH)
                IQ(LCASH+1)=ICASH
              ENDIF
              LADDR=LCASH
            ENDIF
C-
C- GET CASH LINK
C-
            IF (BANK_NAME(I,K) .EQ. 'LINK' .AND.
     &        (LINK_BANK(I,K) .EQ. 'UCSH' .OR. LINK_BANK(I,K) .EQ.
     &        'CASH') )THEN
              IPTR = UDST_DATA((J-1)*NTAGS_GRP(K)+I,K)    ! MN(K).GRP(J).WRD(I)
              IF (IPTR .NE. 0)THEN
                LUCSH = GZUCSH()
                IF (LUCSH .LE. 0)THEN
                  CALL ERRMSG('bank not found','UDST_TO_DST_FILL',
     &              'UCSH','W')
                ELSE
                  IT = 0
                  DO II = IPTR,IPTR+NCELLS-1
                    IT = IT+1
                    IQ(LCASH+2*IT+1)=IQ(LUCSH+1+2*II)
                    Q(LCASH+2*IT+2)=Q(LUCSH+2+2*II)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'TRDT')THEN
              Q(LTRDT+30)=1. ! flag that TRD_ANALYSIS has been called
              LADDR = LTRDT
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'CACL')LADDR = LCACL
            IF(BANK_NAME(I,K) .EQ. 'HMTE')LADDR = LHMTE
            IF(BANK_NAME(I,K) .EQ. 'HMTP')LADDR = LHMTP
C
C ****  Build MUON banks
C
            IF (XGRP1(K) .EQ. 'PMUO')THEN
              IF (BOOK_BANKS)THEN
                BOOK_BANKS = .FALSE.
                L = 0
                N = 0
                CALL BKPMUO(L,N,LPMUO)
                IQ(LPMUO+1)=IPMUO
                IQ(LPARH+2)=IQ(LPARH+2)+1   ! count PMUO banks
                CALL BKMUOT(0,0,LMUOT)
                LQ(LPMUO-IQ(LPMUO-2)-1)=LMUOT
                IQ(LPMUO-5)=J
                IQ(LMUOT-5)=J
                IOBJECT_ADDR = LPMUO
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'PMUO')LADDR=LPMUO
C-
C-****BUILD PTAU BANK
C-
            IF(XGRP1(K) .EQ. 'PTAU')THEN
              IF (BOOK_BANKS)THEN
                BOOK_BANKS = .FALSE.
                N=0
                DO II = 1,NTAGS_GRP(K)
                  IF(TAGS_MAP(1,II,K) .EQ. 'NTKTAU')THEN
                    N=UDST_DATA((J-1)*NTAGS_GRP(K)+II,K)
                  ENDIF
                ENDDO
                CALL BKPTAU(N,LPTAU)  ! this also updates count in PARH
                IQ(LPTAU+1)=IPTAU           ! version number
                IOBJECT_ADDR = LPTAU
                NTAU_TRK=1
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'PTAU')LADDR = LPTAU
C-
C-PNUT BANKS
C-
            IF (XGRP1(K) .EQ. 'PNUT')THEN
              IF(BOOK_BANKS)THEN
                BOOK_BANKS = .FALSE.
                CALL BKPNUT(J)
                LPNUT = GZPNUT(J)
                IQ(LPNUT+1)=IPNUT           ! version number
                IQ(LPARH+5)=IQ(LPARH+5)+1   ! count PNUT banks
                IOBJECT_ADDR = LPNUT
                ND = IQ(LPNUT-1)
                CALL VZERO(Q(LPNUT+3),ND-2)
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'PNUT')LADDR = LPNUT
C-
C-JNEP BANKS
C-
            IF (BANK_NAME(I,K) .EQ. 'JNEP')THEN
              IF(BOOK_BANKS)THEN
                IT = GET_INDEX(NTAGS_GRP(K),bank_name(1,K),'LINK')
                LINK_JET = UDST_DATA((J-1)*NTAGS_GRP(K)+IT,K)
                FOUND=.FALSE.
                DO II =1,NCONE_JNEP
                  IF(INDEX(XGRP1(K),CONE(II)) .NE. 0)THEN
                    CALL SET_CAPH('CONE_JET',TEMPLATE(1,II),IER)
                    FOUND=.TRUE.
                  ENDIF
                ENDDO
                IF(.NOT.FOUND)THEN
                  CALL ERRMSG('UNKNOWN_JNEP_CONE','UDST_TO_DST_FILL',
     &                XGRP1(K),'W')
                  GOTO 100            ! skip this one
                ENDIF
                LJETS = GZJETS()
                DO WHILE (LJETS .NE. 0 .AND. IQ(LJETS-5).NE.LINK_JET)
                  LJETS = LQ(LJETS)
                ENDDO
                CALL RESET_CAPH
                BOOK_BANKS=.FALSE.
                CALL BKJNEP(LJETS,LJNEP)
                IQ(LJNEP+1)=IJNEP           ! version number
              ENDIF
              LADDR = LJNEP
            ENDIF
C-
C-GLOBAL BANKS
C-
            IF(XGRP1(K) .EQ. 'GLOB')THEN
              II=1
              DO WHILE (INDEX(TAGS_MAP(1,II,K), 'RECO') .EQ. 0 .AND. II
     &          .LT. NTAGS_GRP(K))
                II=II+1
              ENDDO
              IF(II .LE. NTAGS_GRP(K))THEN
                IQ(LHSTR+3)=INT(UDST_DATA(II,K))
                IQ(LHSTR+4)=UDST_DATA(II,K)*100-IQ(LHSTR+3)*100
                IF(IQ(LHSTR+3) .GT. 0)THEN
                  PROD_NAME='FULL_D0RECO'
                  CALL UCTOH(PROD_NAME,IQ(LHSTR+7),4,40)
                ENDIF
              ELSE
                CALL ERRMSG('RECO VERSION NOT FOUND','UDST_TO_DST_FILL',
     &            ' ','W')
              ENDIF
            ENDIF
            IF(BANK_NAME(I,K) .EQ. 'GLOB')LADDR=LGLOB
            IF(BANK_NAME(I,K) .EQ. 'PLV0')LADDR=LPLV0
            IF(BANK_NAME(I,K) .EQ. 'MTRH')LADDR=GZMTRH()
            IF(BANK_NAME(I,K) .EQ. 'FTRH')LADDR=GZFTRH()
            IF(BANK_NAME(I,K) .EQ. 'DTRH')LADDR=GZDTRH()
            IF(BANK_NAME(I,K) .EQ. 'TTRH')LADDR=LTTRH
            IF(BANK_NAME(I,K) .EQ. 'VTRH')LADDR=GZVTRH()
            IF(BANK_NAME(I,K) .EQ. 'MUOT')LADDR=LMUOT
            IF(BANK_NAME(I,K) .EQ. 'VCOR')LADDR=GZVCOR()
            IF(BANK_NAME(I,K) .EQ. 'VERH')LADDR=GZVERH()
C-
C-FILL THE BANK
C-
            IF (LADDR .NE. 0)THEN
              CALL UDST_TO_DST_FILL_BANK(LADDR,IOR(I,K),IELE(I,K),
     &          UDST_DATA((J-1)*NTAGS_GRP(K)+I,K))
C              IF( IOR(I,K) .EQ. 'B')I=I+1
            ELSEIF (BANK_NAME(I,K) .EQ. 'LINK')THEN
              LINK_PTR = UDST_DATA((J-1)*NTAGS_GRP(K)+I,K)
              IF (LINK_PTR .EQ. 0)GOTO 100
              IF(LINK_BANK(I,K) .EQ. 'TRAK' .OR. LINK_BANK(I,K) .EQ.
     &          'ZTRK')THEN
                LZTRK = GZZTRK(0)
                DO WHILE (IQ(LZTRK-5) .NE. LINK_PTR .AND. LZTRK .NE. 0)
                  LZTRK = LQ(LZTRK)
                ENDDO
                IF (LZTRK .NE. 0)THEN
                  IF(IQ(IOBJECT_ADDR-4) .EQ. 'PELC' .OR.
     &              IQ(IOBJECT_ADDR-4) .EQ. 'PPHO')THEN
                    LQ(IOBJECT_ADDR-3)=LZTRK
                    LQ(LZTRK-4) = IOBJECT_ADDR
                    IF(IQ(IOBJECT_ADDR-4) .EQ. 'PELC')THEN
                      LCACL=LQ(IOBJECT_ADDR-2)
                      IF(LCACL.GT.0)LQ(LCACL-6)=LZTRK
                    ENDIF
                  ELSEIF(IQ(IOBJECT_ADDR-4) .EQ. 'PMUO')THEN
                    LQ(IOBJECT_ADDR-IQ(LPMUO-2)-4)=LZTRK
                    LQ(LZTRK-3) = IOBJECT_ADDR
                  ELSEIF(IQ(IOBJECT_ADDR-4) .EQ. 'PTAU')THEN
                    LQ(IOBJECT_ADDR-2-NTAU_TRK)=LZTRK
                    NTAU_TRK=NTAU_TRK+1
                    LQ(LZTRK-5) = IOBJECT_ADDR
                  ENDIF
                ENDIF
              ELSEIF(INDEX(LINK_BANK(I,K),'JET') .NE. 0 .AND.
     &            XGRP1(K) .EQ. 'PTAU')THEN
                CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
                LJETS = GZJETS()
                DO WHILE(LJETS .NE. 0 .AND. IQ(LJETS-5) .NE. LINK_PTR)
                  LJETS=LQ(LJETS)
                ENDDO
                IF (LJETS .NE. 0)LQ(IOBJECT_ADDR-2)=LJETS
                CALL RESET_CAPH
              ELSEIF(INDEX(LINK_BANK(I,K),'VERT').NE.0.AND.XGRP1(K).EQ.
     &          'PMUO')THEN
                LVERT=GZVERT(LINK_PTR)
                IF (LVERT.LE.0 .AND. NOBJ(ID_VERT).GT.0)
     &            CALL ERRMSG('target of link not found',
     &            'UDST_TO_DST_FILL','PMUO->VERT','W')
                LQ(LPMUO-IQ(LPMUO-2)-3)=LVERT
              ENDIF
            ELSE
              CALL ERRMSG('bank not found','UDST_TO_DST_FILL',
     &          BANK_NAME(I,K),'W')
            ENDIF
  100       CONTINUE
            I=I+1
          ENDDO
          IF(XGRP1(K).EQ.'PELC')THEN
            CALL UDST_FIX_PELC(LPELC)
          ELSEIF(XGRP1(K).EQ.'PPHO')THEN
            CALL UDST_FIX_PELC(LPPHO)
          ELSEIF(XGRP1(K).EQ.'PMUO')THEN
            CALL UDST_FIX_PMUO(LPMUO)
          ELSEIF(XGRP1(K).EQ.'PTAU')THEN
            CALL UDST_FIX_PTAU(LPTAU)
          ELSEIF(XGRP1(K) .EQ. 'PNUT')THEN
            CALL UDST_FIX_PNUT(LPNUT)
          ELSEIF(INDEX(XGRP1(K),'JNP').NE.0)THEN
            CALL UDST_FIX_JETS(LJNEP)
          ENDIF
        ENDDO
  101   CONTINUE
      ENDDO
C-
C- create reference links
C-
C- links to/from TRDT
      LTTRH=GZTTRH()
      LTRDT=LQ(LTTRH-1)
      DO WHILE(LTRDT.GT.0)
        Q(LTRDT+31)=ABS(Q(LTRDT+31)) ! valid values in DST: 1,0; in UDST: -1,0
        LCACL=LQ(LTRDT-5)
        IF(LCACL.GT.0)THEN
          LQ(LCACL-5) = LTRDT
          LZTRK=LQ(LCACL-6)
          IF(LZTRK.GT.0)THEN
            LQ(LZTRK-9)=LTRDT
            LQ(LTRDT-4)=LZTRK
          ENDIF
        ENDIF
        LTRDT=LQ(LTRDT)
      ENDDO
C
C- links from JETS
      IF( .NOT. IF_JETS) GOTO 999
      DO I= 1,NCONE
        IF(I.LT.NCONE)CALL SET_CAPH('CONE_JET',TEMPLATE(1,I),IER)
        IF(I.EQ.NCONE)CALL SET_CAPH('NN_JET',TEMPLATE(1,I),IER)
        IF (IER .NE. 0)THEN
          CALL ERRMSG('SET_CAPH','UDST_TO_DST_FILL',
     &      'ERROR SETTING CAPH','F')
          GOTO 500
        ENDIF
        IF(I .EQ. 1)THEN            ! 0.7 cone jet
          JET_NAME='JET7'
        ELSEIF(I .EQ. 2)THEN
          JET_NAME='JET5'
        ELSEIF(I .EQ. 3)THEN
          JET_NAME='JET3'
        ELSEIF(I .EQ. 4)THEN
          JET_NAME='JET1'
        ELSEIF(I .EQ. 5)THEN
          JET_NAME='JNM7'
        ELSEIF(I .EQ. 6)THEN
          JET_NAME='JETF'
        ELSEIF(I .EQ. 7)THEN
          JET_NAME='JETA'
        ELSEIF(I .EQ. 8)THEN
          JET_NAME='JETN'
        ELSE
          CALL ERRMSG('UNKNOWN_JETS_CONE','UDST_TO_DST_FILL',' ','W')
          GOTO 500
        ENDIF
C-
C-Find the corresponding grp index
C-
        DO J=1,MAX_GRP
          IF(XGRP1(J) .EQ. JET_NAME)THEN
            DO K=1,NOBJ(J)
C-
C-Find the correct jet to point to
C-
              LJETS=GZJETS()
              DO WHILE(IQ(LJETS-5) .NE. K .AND. LJETS .GT. 0)
                LJETS = LQ(LJETS)
              ENDDO
              ELINK =3
              PLINK =3
C-
C-Find the link
C-
              DO II = 1,NTAGS_GRP(J)
                LINK_PTR = 0
                IF(BANK_NAME(II,J) .EQ. 'LINK' .AND.
     &            LINK_BANK(II,J) .EQ. 'PELC')THEN
                  LINK_PTR = UDST_DATA((K-1)*NTAGS_GRP(J)+II,J)
                ENDIF
                IF(LINK_PTR .NE. 0)THEN
                  IF(LINK_PTR .GE. 1)THEN             ! link to Electron
                    LPELC=GZPELC()
                    DO WHILE(LPELC .GT. 0)
                      IF(IQ(LPELC-5).EQ.LINK_PTR.AND.ELINK.LE.4)THEN
                        LQ(LJETS-ELINK)=LPELC
                        ELINK = ELINK+1
                        PLINK = ELINK
                      ENDIF
                      LPELC = LQ(LPELC)
                    ENDDO
C-
                  ELSEIF(LINK_PTR .LT. 0)THEN         ! link to Photon
                    LPPHO=GZPPHO()
                    DO WHILE(LPPHO .GT. 0)
                      IF(IQ(LPPHO-5).EQ.ABS(LINK_PTR).AND.PLINK.LE.4)
     &                  THEN
C ---> WHY "PLINK.LE.4" ?????????????
                        LQ(LJETS-PLINK)=LPPHO
                        PLINK = PLINK+1
                        ELINK = PLINK
                      ENDIF
                      LPPHO = LQ(LPPHO)
                    ENDDO
                  ENDIF
C-
                ENDIF           !link_ptr .ne. 0
              ENDDO             ! ii
            ENDDO               ! k
          ENDIF                 ! xgrp1(j) .eq. jet_name
        ENDDO                   ! j=1,20
  500   CONTINUE
        CALL RESET_CAPH
      ENDDO                     !i=1,NCONE
C
  999 RETURN
      END
