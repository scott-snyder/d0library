      SUBROUTINE FILL_MCINFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill MC information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER I,J,KMAX,NTAGS
      INTEGER NMUISAW,NMUISAB,NANAL,IOFFSET, NBBISAW,NBBISAB
      INTEGER NVAR1,NVAR2
      REAL    XDATA(1000)
      REAL    MUISPT(NWANT_MUISA),MUISPTB(NWANT_MUISA)
      REAL    MUISPTPAR(NWANT_MUISA)
      REAL    MUISETA(NWANT_MUISA),MUISPHI(NWANT_MUISA)
      REAL    MUISETAB(NWANT_MUISA),MUISPHIB(NWANT_MUISA)
      REAL    BBISPT(NWANT_BBISA),BBISPTB(NWANT_BBISA)
      REAL    BBISETA(NWANT_BBISA),BBISPHI(NWANT_BBISA)
      REAL    BBISETAB(NWANT_BBISA),BBISPHIB(NWANT_BBISA)
      REAL  L1MU(NVAR_L2MUON,NWANT_L1MUON),L1JT(NVAR_L2JET,NWANT_L1JET)
      REAL  L2MU(NVAR_L2MUON,NWANT_L2MUON),L2JT(NVAR_L2JET,NWANT_L2JET)
C----------------------------------------------------------------------
      IF (MCDATA) THEN
        NANAL=1
        CALL ISMUMU(NANAL,NMUISAW,MUISPT,MUISETA,MUISPHI,MUISPTPAR,
     &      NMUISAB,MUISPTB,MUISETAB,MUISPHIB)
        IF(nmuisaw.GT.nwant_muisa) nmuisaw=nwant_muisa
        IF((nmuisaw+nmuisab).gt.nwant_muisa)
     &      nmuisab=nwant_muisa-nmuisaw
        ixmuisa(1)=nmuisaw+nmuisab
        DO J=1,NMUISAW
          xmuisa(j+1)=muispt(j)
          xmuisa(j+1*nwant_muisa+1)=muiseta(j)
          xmuisa(j+2*nwant_muisa+1)=muisphi(j)
          ixmuisa(j+3*nwant_muisa+1)=nanal
          xmuisa(j+4*nwant_muisa+1)=muisptpar(j)
        ENDDO
        DO J=1,NMUISAB
          ioffset=nmuisaw+j
          xmuisa(ioffset+1) =muisptb(j)
          xmuisa(ioffset+1*nwant_muisa+1) =muisetab(j)
          xmuisa(ioffset+2*nwant_muisa+1) =muisphib(j)
          ixmuisa(ioffset+3*nwant_muisa+1)=5
        ENDDO
      ENDIF
C
C *** bbar isajet information (if MC events)
C
      IF (MCDATA) THEN
        NANAL=1
        CALL ISBBAR(NANAL,NBBISAW,BBISPT,BBISETA,BBISPHI,
     &      NBBISAB,BBISPTB,BBISETAB,BBISPHIB)
        IF(nbbisaw.GT.nwant_bbisa) nbbisaw=nwant_bbisa
        IF((nbbisaw+nbbisab).gt.nwant_bbisa)
     &      nbbisab=nwant_bbisa-nbbisaw
        ixbbisa(1)=nbbisaw+nbbisab
        DO J=1,NBBISAW
          xbbisa(j+1)=bbispt(j)
          xbbisa(j+1*nwant_bbisa+1)=bbiseta(j)
          xbbisa(j+2*nwant_bbisa+1)=bbisphi(j)
          ixbbisa(j+3*nwant_bbisa+1)=6
        ENDDO
        DO J=1,NBBISAB
          ioffset=nbbisaw+j
          xbbisa(ioffset+1) =bbisptb(j)
          xbbisa(ioffset+1*nwant_bbisa+1) =bbisetab(j)
          xbbisa(ioffset+2*nwant_bbisa+1) =bbisphib(j)
          ixbbisa(ioffset+3*nwant_bbisa+1)=9
        ENDDO
      ENDIF
C
C *** Fill the L1 and L2 muon and jet information arrays
C
      CALL TOP_DILEP_L2INFO()
C
C *** muon L1 information
C
      CALL TOP_DILEP_GET_L1MUON(L1MU)
      ixl1muon(1)=3
      DO J=1,3
        xl1muon(j+1) =l1mu(1,j)
        xl1muon(j+1*nwant_l1muon+1) =l1mu(2,j)
        xl1muon(j+2*nwant_l1muon+1) =l1mu(3,j)
        xl1muon(j+3*nwant_l1muon+1) =l1mu(4,j)
        xl1muon(j+4*nwant_l1muon+1) =l1mu(5,j)
      ENDDO
C
C *** muon L2 information
C
      CALL TOP_DILEP_GET_L2MUON(L2MU)
      ixl2muon(1)=3
      DO J=1,3
        xl2muon(j+1) =l2mu(1,j)
        xl2muon(j+1*nwant_l2muon+1) =l2mu(2,j)
        xl2muon(j+2*nwant_l2muon+1) =l2mu(3,j)
        xl2muon(j+3*nwant_l2muon+1) =l2mu(4,j)
        xl2muon(j+4*nwant_l2muon+1) =l2mu(5,j)
      ENDDO
C
C *** jet L1 information
C
      CALL TOP_DILEP_GET_L1JET(L1JT)
      ixl1jet(1)=3
      DO J=1,3
        xl1jet(j+1) =l1jt(1,j)
        xl1jet(j+1*nwant_l1jet+1) =l1jt(2,j)
        xl1jet(j+2*nwant_l1jet+1) =l1jt(3,j)
        xl1jet(j+3*nwant_l1jet+1) =l1jt(4,j)
        xl1jet(j+4*nwant_l1jet+1) =l1jt(5,j)
      ENDDO
C
C *** jet L2 information
C
      CALL TOP_DILEP_GET_L2JET(L2JT)
      ixl2jet(1)=3
      DO J=1,3
        xl2jet(j+1) =l2jt(1,j)
        xl2jet(j+1*nwant_l2jet+1) =l2jt(2,j)
        xl2jet(j+2*nwant_l2jet+1) =l2jt(3,j)
        xl2jet(j+3*nwant_l2jet+1) =l2jt(4,j)
        xl2jet(j+4*nwant_l2jet+1) =l2jt(5,j)
      ENDDO
  999 RETURN
C...........................................................................
      ENTRY MUISA_INFO(NVAR,XDATA)
      nvar = nwant_muisa*nvar_muisa+1
      CALL ucopy(xmuisa,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY BBISA_INFO(NVAR,XDATA)
      nvar = nwant_bbisa*nvar_bbisa+1
      CALL ucopy(xbbisa,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY L2MUON_INFO(NVAR,XDATA)
      nvar1 = nwant_l2muon*nvar_l2muon+1
      nvar2 = nwant_l1muon*nvar_l2muon+1
C  note: nvar_l1muon = nvar_l2muon by requirement
      CALL ucopy(xl2muon,xdata,nvar1)
      do i=1,nvar2
        xdata(i+nvar1)=xl1muon(i)
      enddo
      nvar=nvar1+nvar2
      RETURN
C...........................................................................
      ENTRY L2JET_INFO(NVAR,XDATA)
      nvar1 = nwant_l2jet*nvar_l2jet+1
      nvar2 = nwant_l1jet*nvar_l2jet+1
C  note: nvar_l1jet = nvar_l2jet by requirement
      CALL ucopy(xl2jet,xdata,nvar1)
      do i=1,nvar2
        xdata(i+nvar1)=xl1jet(i)
      enddo
      nvar=nvar1+nvar2
      RETURN
C...........................................................................
      ENTRY MUISA_TAGS(NTAGS,TMUISA,KMAX)
      KMAX = nwant_MUISA
      ntags = nvar_MUISA+1
      TMUISA(1)='NMUISA'
      TMUISA(2)='MUISAPT'
      TMUISA(3)='MUISAETA'
      TMUISA(4)='MUISAPHI'
      TMUISA(5)='MUISAPAR:I'
      TMUISA(6)='MUISPTPAR'
      RETURN
C...........................................................................
      ENTRY BBISA_TAGS(NTAGS,TBBISA,KMAX)
      KMAX = nwant_BBISA
      ntags = nvar_BBISA+1
      TBBISA(1)='NBBISA'
      TBBISA(2)='BBISAPT'
      TBBISA(3)='BBISAETA'
      TBBISA(4)='BBISAPHI'
      TBBISA(5)='BBISAPAR:I'
      RETURN
C...........................................................................
      ENTRY L2MUON_TAGS(NTAGS,TL2MU,KMAX)
      KMAX = nwant_l2muon+nwant_l1muon
      ntags = nvar_l2muon+1
      TL2MU(1)='NL2MU'
      TL2MU(2)='L2MUPT'
      TL2MU(3)='L2MUETA'
      TL2MU(4)='L2MUPHI'
      TL2MU(5)='L2MUREC'
      TL2MU(6)='L2MUDR'
      RETURN
C...........................................................................
      ENTRY L2JET_TAGS(NTAGS,TL2JT,KMAX)
      KMAX = nwant_l2jet+nwant_l1jet
      ntags = nvar_l2jet+1
      TL2JT(1)='NL2JT'
      TL2JT(2)='L2JTPT'
      TL2JT(3)='L2JTETA'
      TL2JT(4)='L2JTPHI'
      TL2JT(5)='L2JTREC'
      TL2JT(6)='L2JTDR'
      RETURN
      END
