      SUBROUTINE MAKE_UDST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write info from DST banks to uDST's
C-
C-   Updated  29-SEP-1992   Ulrich Heintz
C-   Updated   6-DEC-1992   Ulrich Heintz
C-   Updated   8-JAN-1993   Meenakshi Narain - add .5 and .3 cone jets
C-   Updated   6-JUL-1993   Ulrich Heintz - updated vertex, muon, header groups
C-   Updated  14-AUG-1993   Ulrich Heintz - fixed storage of bit patterns,
C-                           extended HEAD, JETx, PELC, PPHO, PMUO groups
C-   Updated  22-NOV-1993   Ian Adam - add fixed part of PTAU and more
C-                           global info
C-   Updated  1-DEC-1993    Ian Adam - add track group, fixes, SORT_XXXX
C-                           subroutines
C-   Updated   6-DEC-1993   Ian Adam - fixes to PLV0, NPTAU, links to UCSH
C-                           for PELC/PPHO, link to track for PELC
C-   Updated  16-DEC-1993   Ian Adam  - fixes to GLOB filling
C-   Updated  19-DEC-1993   Ulrich Heintz - move GLOB filling to subroutine
C-   Updated  23-DEC-1993   Ulrich Heintz - fix sequence of PNUTs
C-   Updated  23-DEC-1993   Ulrich Heintz - fix pointer to UCSH for PELCs
C-   Updated  23-DEC-1993   Ulrich Heintz - move VERT filling to subroutine
C-   Updated  14-JAN-1994   Ian Adam - Fix VERT_NUMBER and TRACK_NUMBER filling,
C-                           take first ZTRK from ZTRH
C-   Updated  21-JAN-1994   Ian Adam - Add extra link to PELC/PPHO from JETS
C-   Updated  12-MAR-1994   Ulrich Heintz  changed GSLINK to GRLINK
C-   Updated  31-MAR-1994   Ulrich Heintz  uncorrect JETS, PELC, PPHO first
C-   Updated  17-OCT-1994   Ulrich Heintz  add VTXT and VFIT objects
C-   Updated  20-OCT-1995   Ian Adam  Add UCWX,CDTK and more jets for v5 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      INCLUDE 'D0$LINKS:IZZTRK.LINK'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:UDST_LINKS.INC'
      INTEGER IER,I,J,J1,NVARMAX,ILNK,JLNK,NCONE
      PARAMETER( NCONE = 7 )
      INTEGER NPELC,NVERT,NVFIT,NPMUO,NPPHO,NJET(NCONE),NJNEP(NCONE)
      INTEGER NPTAU,NTRAK,NVTXT,NPNUT,RUNNO,EVONUM,VERTEX_INDEX,ICONE
      INTEGER LPELC,GZPELC,LPPHO,GZPPHO,LJETS,GZJETS,LVERH,GZVERH,LPMUO
      INTEGER GZPMUO,LPTAU,GZPTAU,LVTRH,GZVTRH,GZPNUT,LZTRH,GZZTRH
      INTEGER LCAPH,GZCAPH,GZVFIT,GZVERT,LPNU1,GZPNU1
      INTEGER ID_PELC,ID_PPHO,ID_PTAU,ID_JNP7,ID_JNP5,ID_JNP3,ID_TRAK
      INTEGER ID_MUON,ID_PNUT,ID_VERT,ID_VTXT,ID_JETS(NCONE),ID_VFIT
      INTEGER ID_PNU1
      INTEGER KELEC,KPHOT,KJET,KPNUT,KMUON,KJNEP,KPTAU,KVERT,KTRAK,KVTXT
      INTEGER KVFIT,KPNU1
      INTEGER NPNU1
      PARAMETER( NVARMAX=10000 )
      REAL    XDATA(NVARMAX),XDATA1(NVARMAX)
      CHARACTER*8 MSG*7
      REAL    TEMPLATE(7,NCONE)
      DATA    TEMPLATE/ 1.0,6.0,0.7,0.0,0.0,0.0,0.0,  ! CONE 0.7
     &                  1.0,6.0,0.5,0.0,0.0,0.0,0.0,  ! CONE 0.5
     &                  1.0,6.0,0.3,0.0,0.0,0.0,0.0,  ! CONE 0.3
     &                  1.0,6.0,1.0,0.0,0.0,0.0,0.0,  ! CONE 1.0
     &                  2.0,6.0,0.7,10.,0.3,0.0,0.0,  ! 0.7 fwd-improved
     &                  1.0,6.0,-.7,0.0,0.0,0.0,0.0,  ! 0.7 D0 angles
     &                  1.0,6.0,-1.0,0.0,0.0,0.0,0.0/ ! NN

      INTEGER CSH_COUNT,CSH_OBJECT,N_CSH_MAX
      PARAMETER(N_CSH_MAX=10000)
      INTEGER NCASH,CASHCELLS(N_CSH_MAX),CSH_CELL(N_CSH_MAX)
      REAL    CSH_ENERGY(N_CSH_MAX),CASHDATA(N_CSH_MAX)

      INTEGER CWX_COUNT,CWX_OBJECT,N_CWX_MAX
      PARAMETER(N_CWX_MAX=10000)
      INTEGER NCAWX,CAWXCELLS(N_CWX_MAX),CWX_CELL(N_CWX_MAX)
      REAL    CWX_ENERGY(N_CWX_MAX),CAWXDATA(N_CWX_MAX)

      LOGICAL FIRST
      DATA    FIRST/.TRUE./
      EXTERNAL  EVONUM, RUNNO
C----------------------------------------------------------------------
C... reserve a link or two...
      CALL GRLINK('MYLINK',ILNK)
      CALL GRLINK('MYLINK',JLNK)
C... First undo all corrections and sort all objects according to uncorrected Et
      LCAPH=GZCAPH()
      IF (LCAPH.GT.0) THEN
        DO WHILE (LCAPH.GT.0)
          LJETS=LQ(LCAPH-IZJETS)
          IF(LJETS.GT.0)THEN
            IF (IQ(LJETS-1).GE.35) THEN   ! if correction records are present
              DO WHILE (LJETS.GT.0)
                CALL UNCORRECT_JETS_BANK( LJETS, .TRUE., IER )
                IF(IER.NE.0.AND.IER.NE.-2)CALL ERRMSG(
     &            'ERROR from UNCORRECT_JETS_BANK','MAKE_UDST',' ','W')
                LJETS=LQ(LJETS)
              ENDDO
            ELSEIF(IQ(LJETS+26).NE.0)THEN ! if correction was done but no record
              IF(IER.NE.0)CALL ERRMSG('old JETS bank','MAKE_UDST',
     &          'cannot undo jet corrections','W')
            ENDIF
            LJETS=LQ(LCAPH-IZJETS)
            CALL ZSORT(IXCOM,LJETS,6)
            LJETS=LQ(LCAPH-IZJETS)
            CALL ZTOPSY(IXCOM,LJETS)
            LJETS=LQ(LCAPH-IZJETS)
          ENDIF
          LCAPH=LQ(LCAPH)
        ENDDO
      ENDIF
      LPELC=GZPELC()
      DO WHILE (LPELC.GT.0)
        CALL UNCORRECTEM(LPELC)
        LPELC=LQ(LPELC)
      ENDDO
      LPELC=GZPELC()
      IF (LPELC.GT.0) THEN
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC=GZPELC()
        CALL ZTOPSY(IXCOM,LPELC)
        LPELC=GZPELC()
      ENDIF
      LPPHO=GZPPHO()
      DO WHILE (LPPHO.GT.0)
        CALL UNCORRECTEM(LPPHO)
        LPPHO=LQ(LPPHO)
      ENDDO
      LPPHO=GZPPHO()
      IF (LPPHO.GT.0) THEN
        CALL ZSORT(IXCOM,LPPHO,7)
        LPPHO=GZPPHO()
        CALL ZTOPSY(IXCOM,LPPHO)
        LPPHO=GZPPHO()
      ENDIF
      LPTAU=GZPTAU()
      IF (LPTAU.GT.0) THEN
        CALL ZSORT(IXCOM,LPTAU,7)
        LPTAU=GZPTAU()
        CALL ZTOPSY(IXCOM,LPTAU)
        LPTAU=GZPTAU()
      ENDIF
      LPMUO=GZPMUO(0)
      IF (LPMUO.GT.0) THEN
        CALL ZSORT(IXCOM,LPMUO,14)   ! sort PMUO banks according to Et
        LPMUO=GZPMUO(0)
        CALL ZTOPSY(IXCOM,LPMUO)
        LPMUO=GZPMUO(0)
      ENDIF
C
C... ZERO CSH INDEX
      CSH_COUNT=0
      CSH_OBJECT=0
      CWX_COUNT=0
      CWX_OBJECT=0
C----------------------------------------------------------------------
C... event vertex (VERT)

      IF (FIRST) THEN
        CALL UDST_VERT_TAGS(KVERT,ID_VERT)
      ENDIF

      CALL VZERO(VERT_NUMBER,N_VERT_MAX)

      NVERT=0
      VERTEX_INDEX=0
      LVERH=GZVERH()
      IF(LVERH.GT.0)THEN
        NVERT=IQ(LVERH+2)   ! Number of primary vertices
        J=1
        LRLINK(ILNK)=GZVERT(1)
        DO WHILE(LRLINK(ILNK).GT.0)
          VERTEX_INDEX=VERTEX_INDEX+1
          CALL UDST_GET_VERT(LRLINK(ILNK),XDATA(J))
          J=J+KVERT
          IF (VERTEX_INDEX.LE.N_VERT_MAX) VERT_NUMBER(VERTEX_INDEX)
     &      =IQ(LRLINK(ILNK)-5)
          LRLINK(ILNK)=GZVERT(VERTEX_INDEX+1)
        ENDDO
      ELSE
        CALL ERRMSG('no VERH bank','MAKE_UDST','LVERH = 0','W')
      ENDIF

      CALL UDST_FILL_GROUP(ID_VERT,XDATA,NVARMAX,VERTEX_INDEX)
C----------------------------------------------------------------------
C... event vertex (VFIT)

      IF (FIRST) CALL UDST_VFIT_TAGS(KVFIT,ID_VFIT)

      NVFIT=0
      J=1
      LRLINK(ILNK)=GZVFIT(1)
      DO WHILE(LRLINK(ILNK).GT.0)
        NVFIT=NVFIT+1
        CALL UDST_GET_VFIT(LRLINK(ILNK),XDATA(J))
        J=J+KVFIT
        LRLINK(ILNK)=GZVFIT(NVFIT+1)
      ENDDO

      CALL UDST_FILL_GROUP(ID_VFIT,XDATA,NVARMAX,NVFIT)
C----------------------------------------------------------------------
C... tracks (VTXT)

      IF (FIRST) THEN
        CALL UDST_VTXT_TAGS(KVTXT,ID_VTXT)
      ENDIF

      CALL VZERO(VTXT_NUMBER,N_VTXT_MAX)

      NVTXT=0
      J=1

      LVTRH=GZVTRH()
      IF (LVTRH.GT.0) THEN
        LRLINK(ILNK)=LQ(LVTRH-IZVTXT)
      ELSE
        LRLINK(ILNK)=0
      ENDIF
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KVTXT)
        NVTXT=NVTXT+1
        CALL UDST_GET_VTXT(LRLINK(ILNK),XDATA(J))
        J=J+KVTXT
        IF (NVTXT.LE.N_VTXT_MAX) VTXT_NUMBER(NVTXT)=IQ(LRLINK(ILNK)-5)
        LRLINK(ILNK)=LQ(LRLINK(ILNK))
      ENDDO
      IF(J.GT.NVARMAX-KVTXT)CALL ERRMSG('NVTXT>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_VTXT,XDATA,NVARMAX,NVTXT)
C----------------------------------------------------------------------
C... tracks (ZTRK)

      IF (FIRST) THEN
        CALL UDST_TRAK_TAGS(KTRAK,ID_TRAK)
      ENDIF

      CALL VZERO(TRACK_NUMBER,N_TRAK_MAX)

      NTRAK=0
      J=1

      LZTRH=GZZTRH()
      IF (LZTRH.GT.0) THEN
        LRLINK(ILNK)=LQ(LZTRH-IZZTRK)
      ELSE
        LRLINK(ILNK)=0
      ENDIF
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KTRAK)
        NTRAK=NTRAK+1
        CALL UDST_GET_TRAK(LRLINK(ILNK),XDATA(J))
        J=J+KTRAK
        IF (NTRAK.LE.N_TRAK_MAX) TRACK_NUMBER(NTRAK)=IQ(LRLINK(ILNK)-5)
        LRLINK(ILNK)=LQ(LRLINK(ILNK))
      ENDDO
      IF(J.GT.NVARMAX-KTRAK)CALL ERRMSG('NTRAK>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_TRAK,XDATA,NVARMAX,NTRAK)
C----------------------------------------------------------------------
C... electrons (PELC)

      IF(FIRST)THEN
        CALL UDST_PELC_TAGS(KELEC,ID_PELC)
      ENDIF

      NPELC=0
      J=1

      LRLINK(ILNK)=GZPELC()
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KELEC)
        NPELC=NPELC+1
        CALL UDST_GET_EMCLUS(LRLINK(ILNK),XDATA(J),CSH_COUNT+1,
     &    CWX_COUNT+1)
        J=J+KELEC

        CALL UDST_GET_CASH(LRLINK(ILNK),NCASH,CASHCELLS,CASHDATA)
        CSH_OBJECT=CSH_OBJECT+1
        IF(CSH_COUNT+NCASH.GT.N_CSH_MAX)THEN
          WRITE (MSG,10) CSH_COUNT+NCASH
   10     FORMAT(I7)
          CALL ERRMSG('CSH_COUNT>MAX','MAKE_UDST',MSG//' CASH cells',
     &      'W')
        ELSE
          DO I=1,NCASH
            CSH_COUNT = CSH_COUNT + 1
            CSH_CELL(CSH_COUNT)   = CASHCELLS(I)
            CSH_ENERGY(CSH_COUNT) = CASHDATA(I)
          ENDDO
        ENDIF

        NCAWX=0
        CALL UDST_GET_CAWX(LRLINK(ILNK),NCAWX,CAWXCELLS,CAWXDATA)
        CWX_OBJECT=CWX_OBJECT+1
        IF(CWX_COUNT+NCAWX.GT.N_CWX_MAX)THEN
          WRITE (MSG,10) CWX_COUNT+NCAWX
          CALL ERRMSG('CWX_COUNT>MAX','MAKE_UDST',MSG//' CAWX cells',
     &      'W')
        ELSE
          DO I=1,NCAWX
            CWX_COUNT = CWX_COUNT + 1
            CWX_CELL(CWX_COUNT)   = CAWXCELLS(I)
            CWX_ENERGY(CWX_COUNT) = CAWXDATA(I)
          ENDDO
        ENDIF

        LRLINK(ILNK)=LQ(LRLINK(ILNK))       ! pointer to next electron
      ENDDO
      IF(J.GT.NVARMAX-KELEC)CALL ERRMSG('NELEC>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_PELC,XDATA,NVARMAX,NPELC)
C----------------------------------------------------------------------
C... photons (PPHO)

      IF(FIRST)THEN
        CALL UDST_PPHO_TAGS(KPHOT,ID_PPHO)
      ENDIF

      NPPHO=0
      J=1

      LRLINK(ILNK)=GZPPHO()
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KPHOT)
        NPPHO=NPPHO+1
        CALL UDST_GET_EMCLUS(LRLINK(ILNK),XDATA(J),CSH_COUNT+1,
     &    CWX_COUNT+1)
        J=J+KPHOT

        CALL UDST_GET_CASH(LRLINK(ILNK),NCASH,CASHCELLS,CASHDATA)
        CSH_OBJECT=CSH_OBJECT+1
        IF(CSH_COUNT+NCASH.GT.N_CSH_MAX)THEN
          WRITE (MSG,10) CSH_COUNT+NCASH
          CALL ERRMSG('CSH_COUNT>MAX','MAKE_UDST',
     &      'cannot handle '//MSG//' CASH cells','W')
        ELSE
          DO I=1,NCASH
            CSH_COUNT = CSH_COUNT + 1
            CSH_CELL(CSH_COUNT)   = CASHCELLS(I)
            CSH_ENERGY(CSH_COUNT) = CASHDATA(I)
          ENDDO
        ENDIF

        NCAWX=0
        CALL UDST_GET_CAWX(LRLINK(ILNK),NCAWX,CAWXCELLS,CAWXDATA)
        CWX_OBJECT=CWX_OBJECT+1
        IF(CWX_COUNT+NCAWX.GT.N_CWX_MAX)THEN
          WRITE (MSG,10) CWX_COUNT+NCAWX
          CALL ERRMSG('CWX_COUNT>MAX','MAKE_UDST',
     &      'cannot handle '//MSG//' CAWX cells','W')
        ELSE
          DO I=1,NCAWX
            CWX_COUNT = CWX_COUNT + 1
            CWX_CELL(CWX_COUNT)   = CAWXCELLS(I)
            CWX_ENERGY(CWX_COUNT) = CAWXDATA(I)
          ENDDO
        ENDIF

        LRLINK(ILNK)=LQ(LRLINK(ILNK))          ! pointer to next photon
      ENDDO
      IF(J.GT.NVARMAX-KPHOT)CALL ERRMSG('NPHOT>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_PPHO,XDATA,NVARMAX,NPPHO)
C----------------------------------------------------------------------
C... Taus (PTAU)

      IF (FIRST) THEN
        CALL UDST_PTAU_TAGS(KPTAU,ID_PTAU)
      ENDIF

      NPTAU=0
      J=1
      LRLINK(ILNK)=GZPTAU()
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KPTAU)
        NPTAU=NPTAU+1
        CALL UDST_GET_PTAU(LRLINK(ILNK),XDATA(J))
        J=J+KPTAU
        LRLINK(ILNK)=LQ(LRLINK(ILNK))
      ENDDO
      IF(J.GT.NVARMAX-KPTAU)CALL ERRMSG('NPTAU>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')
      CALL UDST_FILL_GROUP(ID_PTAU,XDATA,NVARMAX,NPTAU)
C----------------------------------------------------------------------
C... jets (JETS,JNEP)
C
      DO ICONE = 1, NCONE
        IF(FIRST)THEN
          CALL UDST_JETS_TAGS(ICONE,KJET,ID_JETS(ICONE))
          IF(ICONE.EQ.1)CALL UDST_JNEP_TAGS(ICONE,KJNEP,ID_JNP7)
          IF(ICONE.EQ.2)CALL UDST_JNEP_TAGS(ICONE,KJNEP,ID_JNP5)
          IF(ICONE.EQ.3)CALL UDST_JNEP_TAGS(ICONE,KJNEP,ID_JNP3)
        ENDIF
        NJET(ICONE)=0
        NJNEP(ICONE)=0
        IF(ICONE.LT.NCONE)THEN
          CALL SET_CAPH('CONE_JET',TEMPLATE(1,ICONE),IER)
        ELSE
          CALL SET_CAPH('NN_JET',TEMPLATE(1,ICONE),IER)
        ENDIF
        IF(IER.NE.0)THEN
          WRITE (MSG,100) ICONE
  100     FORMAT('ICONE=',I1)
          CALL ERRMSG('SET_CAPH','MAKE_UDST',MSG,'W')
        ELSE
          J=1
          J1=1
          LRLINK(ILNK)=GZJETS()
          DO WHILE (LRLINK(ILNK).NE.0.AND.J.LE.NVARMAX-KJET)
            NJET(ICONE)=NJET(ICONE)+1
            CALL UDST_GET_JETS(LRLINK(ILNK),XDATA(J))
            J=J+KJET
            IF(ICONE.EQ.1.OR.ICONE.EQ.2.OR.ICONE.EQ.3)THEN
              LRLINK(JLNK)=LQ(LRLINK(ILNK)-2)
              IF(LRLINK(JLNK).GT.0)THEN
                CALL UDST_GET_JNEP(LRLINK(JLNK),XDATA1(J1))
                XDATA1(J1+KJNEP-1)=NJET(ICONE) ! link to parent jet
                NJNEP(ICONE)=NJNEP(ICONE)+1
                J1=J1+KJNEP
              ENDIF
            ENDIF
            LRLINK(ILNK)=LQ(LRLINK(ILNK))
          ENDDO
          IF(J.GT.NVARMAX-KJET)CALL ERRMSG('NJET>MAX','MAKE_UDST',
     &      'number of words exceeds NVARMAX','W')
        ENDIF
        IF (ICONE.EQ.1)THEN
          CALL UDST_FILL_GROUP(ID_JNP7,XDATA1,NVARMAX,NJNEP(ICONE))
        ELSEIF (ICONE.EQ.2) THEN
          CALL UDST_FILL_GROUP(ID_JNP5,XDATA1,NVARMAX,NJNEP(ICONE))
        ELSEIF (ICONE.EQ.3) THEN
          CALL UDST_FILL_GROUP(ID_JNP3,XDATA1,NVARMAX,NJNEP(ICONE))
        ENDIF
        CALL UDST_FILL_GROUP(ID_JETS(ICONE),XDATA,NVARMAX,NJET(ICONE))
        CALL RESET_CAPH
      ENDDO
C----------------------------------------------------------------------
C... missing Et (PNUT)
      IF(FIRST)THEN
        CALL UDST_PNUT_TAGS(KPNUT,ID_PNUT)
      ENDIF

      NPNUT=0
      J=1
      LRLINK(ILNK)=GZPNUT(NPNUT+1)

      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KPNUT)
        NPNUT=NPNUT+1
        CALL UDST_GET_PNUT(LRLINK(ILNK),XDATA(J))
        J=J+KPNUT
        LRLINK(ILNK)=GZPNUT(NPNUT+1)
      ENDDO
      IF(J.GT.NVARMAX-KPNUT)CALL ERRMSG('NPNUT>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_PNUT,XDATA,NVARMAX,NPNUT)
C----------------------------------------------------------------------
C... missing Et (PNU1)
      IF(FIRST)THEN
        CALL UDST_PNU1_TAGS(KPNU1,ID_PNU1)
      ENDIF

      LPNU1 = GZPNU1()
      IF (LPNU1.GT.0) THEN
        NPNU1 = IQ(LPNU1+3)
        J=1
        DO I = 1,NPNU1
          CALL UDST_GET_PNU1(LPNU1,I,XDATA(J))
          J=J+KPNU1
        ENDDO
        IF(J.GT.NVARMAX-KPNU1)CALL ERRMSG('NPNU1>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')
        CALL UDST_FILL_GROUP(ID_PNU1,XDATA,NVARMAX,NPNU1)
      ELSE
        CALL ERRMSG('NO PNU1','MAKE_UDST',' ','W')
      ENDIF
C----------------------------------------------------------------------
C... muons (new muon code supplied by Cecilia)
      IF(FIRST)THEN
        CALL UDST_MUON_TAGS(KMUON,ID_MUON)
      ENDIF

      NPMUO=0
      J=1

      LRLINK(ILNK)=GZPMUO(0)
      DO WHILE (LRLINK(ILNK).GT.0.AND.J.LE.NVARMAX-KMUON)
        CALL UDST_GET_MUON(LRLINK(ILNK), XDATA(J), KMUON)
        NPMUO=NPMUO+1
        J=J+KMUON
        LRLINK(ILNK)=LQ(LRLINK(ILNK))          ! pointer to next muon
      ENDDO
      IF(J.GT.NVARMAX-KMUON)CALL ERRMSG('NPMUO>MAX','MAKE_UDST',
     &  'number of words exceeds NVARMAX','W')

      CALL UDST_FILL_GROUP(ID_MUON,XDATA,NVARMAX,NPMUO)
C----------------------------------------------------------------------
C... GLOB bank, etc.
      IF(FIRST)THEN
        CALL UDST_GLOB_TAGS
      ENDIF
      CALL UDST_GET_GLOB(NPELC,NPPHO,NPMUO,NPTAU,NJET,NVERT)
C----------------------------------------------------------------------
      CALL UCSHFL(CSH_COUNT,CSH_CELL,CSH_ENERGY)
      CALL UCWXFL(CWX_COUNT,CWX_CELL,CWX_ENERGY)
      CALL FILL_CDTK
C----------------------------------------------------------------------
  999 FIRST=.FALSE.
      CALL RRLINK('MYLINK',ILNK)
      CALL RRLINK('MYLINK',JLNK)
      RETURN
      END
