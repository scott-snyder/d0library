CDECK  ID>, HPROJ1. 
*CMZ :  4.13/01 06/09/91  13.55.01  by  Rene Brun
*-- Author :
      SUBROUTINE HPROJ11(ID1,IDN,ISEL,UWFUNC,IFROM,ITOM,IVARX)
*.==========>
*.           Fill histogram ID1 using variable number IVARX
*.           from the N-tuple IDN.
*.
*..=========> ( R.Brun )
C-    Updated  06-OCT-1992  Alexandre Zinchenko - project combination
C-                          of Ntuple variables (new name HPROJ11) -
C-      All questions about non-D0 standards send to Mr.Rene Brun !
C----------------------------------------------------------------------
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHDUM(10),
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X(512)
      EXTERNAL UWFUNC
*.___________________________________________
*
      CALL HGNPAR(IDN,'HPROJ1')
      IF(LCIDN.LE.0)GO TO 99
      NDIM=IQ(LCIDN+2)
      IF(IVARX.LE.0.OR.IVARX.GT.NDIM)THEN
         CALL HBUG('Error in N-tuple parameters','HPROJ1',IDN)
         RETURN
      ENDIF
      ITO=MIN(ITOM,IQ(LCIDN+3))
*
      DO 10 IDNEVT=IFROM,ITO
         CALL HGNF(IDN,IDNEVT,X,IERROR)
         IF(IERROR.NE.0)GO TO 99
         VIDN1=X(IVARX)
         IF(ISEL.EQ.0)THEN
            CALL HF1(ID1,X(IVARX),1.)
         ELSE
            W=UWFUNC(X,ISEL)
            IF(W.NE.1.E+6)THEN
               CALL HF1(ID1,W,1.)
            ENDIF
         ENDIF
  10  CONTINUE
*
  99  END
CDECK  ID>, HPROJ2. 
*CMZ :  4.13/01 06/09/91  13.59.04  by  Rene Brun
*-- Author :
      SUBROUTINE HPROJ22(ID1,IDN,ISEL,UWFUNC,IFROM,ITOM,IVARX,IVARY)
*.==========>
*.           Fill 2-DIM histogram ID1 using variables number IVARX
*.           and IVARY from the N-tuple IDN.
*.
*..=========> ( R.Brun )
C-    Updated  06-OCT-1992  Alexandre Zinchenko - project combination
C-                          of Ntuple variables (new name HPROJ22)
C-      All questions about non-D0 standards send to Mr.Rene Brun !
C----------------------------------------------------------------------
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHDUM(10),
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,X(512)
      EXTERNAL UWFUNC
*.___________________________________________
*
      CALL HGNPAR(IDN,'HPROJ2')
      IF(LCIDN.LE.0)GO TO 99
      NDIM=IQ(LCIDN+2)
      IF(IVARX.LE.0.OR.IVARX.GT.NDIM)THEN
         CALL HBUG('Wrong parameters','HPROJ2',IDN)
         RETURN
      ENDIF
      IF(IVARY.LE.0.OR.IVARY.GT.NDIM)THEN
         CALL HBUG('Wrong parameters','HPROJ2',IDN)
         RETURN
      ENDIF
      ITO=MIN(ITOM,IQ(LCIDN+3))
*
      DO 10 IDNEVT=IFROM,ITO
         CALL HGNF(IDN,IDNEVT,X,IERROR)
         IF(IERROR.NE.0)GO TO 99
         VIDN1=X(IVARX)
         VIDN2=X(IVARY)
         IF(ISEL.EQ.0)THEN
            CALL HFILL(ID1,X(IVARX),X(IVARY),1.)
         ELSE
            W=UWFUNC(X,ISEL)
            IF(W.NE.1.E+6)THEN
               CALL HFILL(ID1,W,X(IVARY),1.)
            ENDIF
         ENDIF
  10  CONTINUE
*
  99  END
CDECK  ID>, HFITH1. 
*CMZ :  4.10/05 17/11/89  12.05.03  by  Rene Brun
*-- Author :
      SUBROUTINE HFITH1(EXDA,ICELL)
*.==========>
*.           Gets histogram bin/cell content into EXDA
*..=========> ( I.Ivanchenko )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine from the CERNLIB, updated to do
C-            fit of 2-dim. histograms with restricted region in X
C-
C-   Updated   8-JUN-1993   Alexandre Zinchenko
C-
C----------------------------------------------------------------------
      INTEGER     NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,        LMAIN
      REAL                                       FENC   ,      HCV
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      INTEGER   IQ        ,LQ
      REAL            Q
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      INTEGER       HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHDUM,
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHDUM(10),
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*
      INTEGER   KNCX  ,KXMIN  ,KXMAX  ,KBWIDX  ,KMIN  ,KMAX  ,KNORM  ,
     +          KTIT1   ,KNCY  ,KYMIN  ,KYMAX  ,KBWIDY   ,KSCAL2   ,
     +          KTIT2   ,KNBIT  ,KNOENT  ,KSTAT1  ,KNSDIR  ,KNRH  ,
     +          KCON1  ,KCON2  ,KBITS  ,KNTOT
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
      INTEGER           I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,
     +                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,
     +I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,
     +I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230
      COMMON / HCBITS  / I1,   I2,   I3,   I4,   I5,   I6,   I7,   I8,
     +                  I9,   I10,  I11,  I12,  I13,  I14,  I15,  I16,
     +I17,  I18,  I19,  I20,  I21,  I22,  I23,  I24,  I25,  I26,  I27,
     +I28,  I29,  I30,  I31,  I32,  I33,  I34,  I35,  I123, I230
*
      COMMON/HCFIT2/ENDFLG,NA,INDFLG(5),IFLFUN,NAMFUN,IDIMPN,XFUMIL(10),
     +        NUMEP ,XMIN  ,EPSW  ,ALLCHA,BINWID,WGTMAX,YMIN,BINWIY,
     +       NCHANX,LINEAR,IFLSF ,IFLBUF,IDER,IWEIGH,ITFUM,ISUPIM
*
      COMMON/HCFITR/IFTRNG,IFTLOW,IFTUP
C
      DIMENSION EXDA(4)
*.___________________________________________
*
      IF(IFTRNG.NE.0)THEN
         ICHANX = MOD(ICELL,NCHANX) 
         IF (ICHANX.EQ.0) ICHANX = NCHANX 
         RCHANX = ICHANX
         ICHANX = ICHANX + IFTLOW - 1 
      ELSE
         ICHANX=MOD(ICELL,NCHANX)
         IF(ICHANX.EQ.0)ICHANX=NCHANX
         RCHANX=ICHANX
      ENDIF
*
      IF(IDIMPN.EQ.3)THEN
         EXDA(1)=HCX(ICHANX,1)
         IF(I6.EQ.0)THEN
            EXDA(3)=XMIN+(RCHANX-0.5)*BINWID
         ELSE
            LBINS=LQ(LCID-2)
            EXDA(3)=0.5*(Q(LBINS+ICHANX)+Q(LBINS+ICHANX+1))
         ENDIF
      ELSE
cAZ         ICHANY=(ICELL-ICHANX)/NCHANX+1
         ICHANY=(ICELL-int(rCHANX))/NCHANX+1
         RCHANY=ICHANY
         EXDA(1)=HCXY(ICHANX,ICHANY)
         EXDA(3)=XMIN+(RCHANX-0.5)*BINWID
         EXDA(4)=YMIN+(RCHANY-0.5)*BINWIY
      ENDIF
*
*     Check out of errorbars presence
*
      IF(IWEIGH.NE.0)THEN
         IF(IWEIGH.EQ.1)THEN
            IF(EXDA(1).NE.0.)THEN
               EXDA(2)=1.
            ELSE
               EXDA(2)=0.
            ENDIF
         ELSEIF(IWEIGH.EQ.2)THEN
            EXDA(2)=1.
         ELSE
            EXDA(2)=WGTMAX
         ENDIF
         GO TO 99
      ENDIF
*
      IF(LQ(LCONT).NE.0.AND.IDIMPN.EQ.3)THEN
         EXDA(2)=HCX(ICHANX,2)
      ELSE
         EXDA(2)=SQRT(ABS(EXDA(1)))
      ENDIF
      IF(EXDA(2).EQ.0.)THEN
         EXDA(2)=ABS(EXDA(1))
         IF(EXDA(2).EQ.0.)GO TO 99
      ENDIF
*
   99 RETURN
      END
