      SUBROUTINE MSTRAK(GOGO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine keeps track space points
C-   along sigle muon track and store them in banks under bank-tree 
C-   HEAD-GEAN-PROC-MTRH-MUON-MTRJ.
C-
C-   Inputs  :   
C-      GOGO    A4    flow control flag.
C                       'INIT', 'UPDT' or 'FILL'
C-   Outputs :   (banks)
C-   Controls: 
C-
C-   Created  26-MAR-1990   KUNORI
C-   Updated  26-AUG-1992   Susumu Igarashi  Change position ID for 
C-                                           Magnet exits. MTRJ version 2.
C-   Updated  26-NOV-1992   Susumu Igarashi  Add SAMUS trajectory and hits
C-                                           cell address. MTRJ version 3.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  -- D0 common blocks...
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C  -- Geant common blocks...
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C  -- variable in argument...
      CHARACTER*4 GOGO
C  -- local variables...
      INTEGER IFIRST,I,J
      INTEGER LPROC,LMUON,LMTRJ,NDATA
      INTEGER MXNPDT,MXNSAM
      PARAMETER (MXNPDT=310)
      PARAMETER (MXNSAM=6)
      INTEGER INMVOL,INMCEN,INMCAL,INMMUO
      INTEGER INPDT(MXNPDT),INMCXX,INMETD,INPDTC(MXNPDT)
      INTEGER INSAM(MXNSAM),INSMXX,INSAMC(MXNSAM) 
      INTEGER OUTMCXX,OUTMETD,OUTSMXX
      INTEGER INMODU,INPLAN,INCELL
      INTEGER HMCEN,HMCAL,HMMUO
      CHARACTER*4 ANAME
      INTEGER NUMB,NUMBC
      INTEGER NWDS
      PARAMETER (NWDS=10)
      INTEGER MXNTRJ,NNTRJ
      PARAMETER (MXNTRJ=100)
      REAL XYZTRJ(NWDS,MXNTRJ)
C     -- external functions...
      INTEGER GZPROC
      EXTERNAL GZPROC
C     -- data statements...
      DATA IFIRST/0/
C----------------------------------------------------------------------

      IF(GOGO.EQ.'UPDT') THEN
C        -- process only muons and skip all others...
         IF(IPART.NE.5.AND.IPART.NE.6) THEN
            GO TO 999
         ENDIF
C        -- entering a new volume...
C                                        (( inwvol in /GCTRAK/ ))
         IF(INWVOL.EQ.1) THEN
C                                        (( nlevel in /GCVOLU/ ))
         IF(NLEVEL.GE.2) THEN
C
C          -- inside calorimeter mother volume, 'MCAL'...
C             also may be inside 'MCEN'
C
           IF(NAMES(2).EQ.HMCAL) THEN
              IF(NLEVEL.GE.3.AND.NAMES(3).EQ.HMCEN) THEN
C
C             -- inside central tracker mother volume, 'MCEN'...
C
                 IF(INMCEN.EQ.0) THEN
                    INMCEN=1
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=100000.
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
C
              ELSE     !  ((( if(inside-MCEN) )))
C
C             -- outside 'MCEN' and inside 'MCAL'...
C
                 IF(INMCAL.EQ.0) THEN
                    INMCAL=1
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=200000.
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
C
              ENDIF    !  ((( if(inside-MCAL and outside-MCEN) )))

C          
           ENDIF       !  ((( if(names(2).eq.hmcal) )))
C
C          -- inside muon mother volume, 'MMUO'...
C
           IF(NAMES(2).EQ.HMMUO) THEN
C             -- entering muon mother volume...
              IF(INMMUO.EQ.0) THEN
                 INMMUO=1
                 IF(NNTRJ.LT.MXNTRJ) THEN
                    NNTRJ=NNTRJ+1
                    XYZTRJ(1,NNTRJ)=300000.
                    CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                    XYZTRJ(9,NNTRJ)=SLENG
                    XYZTRJ(10,NNTRJ)=0.0
                 ENDIF
              ENDIF
C
C             -- check if we are lower level or not...
              IF(NLEVEL.LE.2) GO TO 110
C
C             -- entering a muon module...
C             -- WAMUS --
              CALL MSGNUM(NAMES(NLEVEL),ANAME,NUMB)
C             write(6,*) 'name,numb=',aname,numb
              IF(ANAME(1:1).EQ.'A'.OR.ANAME(1:1).EQ.'B'.
     +                              OR.ANAME(1:1).EQ.'C') THEN
                 IF(NUMB.GE.0.AND.NUMB.LE.MXNPDT) THEN
                 IF(INPDT(NUMB).EQ.0) THEN
                    INPDT(NUMB)=1
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=FLOAT(300000+NUMB)
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
                 ENDIF

                 IF(NLEVEL.EQ.5) THEN
                 CALL MSGNUM(NAMES(NLEVEL),ANAME,NUMB)
                 IF(ANAME(1:2).EQ.'AC'.OR.ANAME(1:2).EQ.'BC'.
     +                              OR.ANAME(1:2).EQ.'CC') THEN
                 IF(NUMB.GE.0.AND.NUMB.LE.MXNPDT) THEN
                   INPDTC(NUMB)=INPDTC(NUMB)+1
                   IF(NNTRJ.LT.MXNTRJ) THEN
                     IF(INPDTC(NUMB).LT.10)THEN
                       IF(INPDTC(NUMB).EQ.1) THEN
                         NNTRJ=NNTRJ+1
                         XYZTRJ(1,NNTRJ)=FLOAT(301000+NUMB)
                         DO 10 I=2,10
   10                    XYZTRJ(I,NNTRJ)=0.0
                       ENDIF
                       XYZTRJ(1+INPDTC(NUMB),NNTRJ)=
     +                   NUMBER(NLEVEL-1)*100+NUMBER(NLEVEL)
C                         (plane number)*100 + (cell number)
                     ENDIF
                   ENDIF
C                 ENDIF
                 ENDIF
                 ENDIF
                 ENDIF
              ENDIF
C             -- entering central toroids...
              IF(ANAME(1:2).EQ.'MC') THEN
                 IF(INMCXX.EQ.0) THEN
                    INMCXX=1
                    OUTMCXX=0
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=410000.
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
              ENDIF
C             -- entering end toroids...
              IF(ANAME(1:2).EQ.'ME') THEN
                 IF(INMETD.EQ.0) THEN
                    INMETD=1
                    OUTMETD=0
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=420000. 
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
              ENDIF

C             -- entering a muon module...
C             -- SAMUS --
              IF(NLEVEL.LT.2) GOTO 110
              CALL UHTOC(NAMES(3),4,ANAME,4)
C             write(6,*) 'name,numb=',aname,numb
              IF(ANAME(1:3).EQ.'SST')THEN
                 READ(ANAME(4:4),'(I1)')NUMB
                 IF(NUMB.GT.0 .AND. NUMB.LE.MXNSAM) THEN
                 IF(INSAM(NUMB).EQ.0) THEN
                    INSAM(NUMB)=1
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=FLOAT(500000+NUMB)
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
                 ENDIF

                 IF(NLEVEL.EQ.4) THEN
                 CALL UHTOC(NAMES(NLEVEL),4,ANAME,4)
                 IF(ANAME(1:2).EQ.'SV')THEN
                   NUMBC=NUMBER(NLEVEL)
                 IF(NUMBC.GT.0 .AND. NUMBC.LE.7200) THEN
                   INSAMC(NUMB)=INSAMC(NUMB)+1
                   IF(NNTRJ.LT.MXNTRJ) THEN
                     IF(INSAMC(NUMB).LT.10)THEN
                       IF(INSAMC(NUMB).EQ.1) THEN
                         NNTRJ=NNTRJ+1
                         XYZTRJ(1,NNTRJ)=FLOAT(501000+NUMB)
                         DO 20 I=2,10
   20                    XYZTRJ(I,NNTRJ)=0.0
                       ENDIF
                       XYZTRJ(1+INSAMC(NUMB),NNTRJ)=FLOAT(NUMBC)
C                      number = (Nstation-1)*1200+(Nsection-1)*200+Ntube
                     ENDIF
                   ENDIF
C                 ENDIF
                 ENDIF
                 ENDIF
                 ENDIF
              ENDIF
C             -- entering SAMUS toroids...
              IF(ANAME(1:2).EQ.'SM') THEN
                 IF(INSMXX.EQ.0) THEN
                    INSMXX=1
                    OUTSMXX=0
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       NNTRJ=NNTRJ+1
                       XYZTRJ(1,NNTRJ)=600000.
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=0.0
                    ENDIF
                 ENDIF
              ENDIF

110           CONTINUE
            ENDIF       ! ((( if(names(2).eq.hmmuo) )))            
C
         ENDIF          ! ((( if(nlevel.ge.2) )))
         ENDIF          ! ((( if(inwvol.eq.1) )))
C
C        -- leaving a volume...
C                                        (( inwvol in /GCTRAK/ ))
         IF(INWVOL.EQ.2) THEN
C                                        (( nlevel in /GCVOLU/ ))
         IF(NLEVEL.GE.2) THEN
C
C          -- inside muon mother volume, 'MMUO'...
C
           IF(NAMES(2).EQ.HMMUO) THEN
C             -- check if we are lower level or not...
              IF(NLEVEL.GT.2) THEN
C             -- leaving central toroids...
              CALL MSGNUM(NAMES(NLEVEL),ANAME,NUMB)
              IF(ANAME(1:2).EQ.'MC') THEN
                 IF(OUTMCXX.EQ.0) THEN
                    OUTMCXX=1
                    NNTRJ=NNTRJ+1
                 ENDIF
                 IF(NNTRJ.LT.MXNTRJ) THEN
                    XYZTRJ(1,NNTRJ)=410001.
                    CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                    XYZTRJ(9,NNTRJ)=SLENG
                    XYZTRJ(10,NNTRJ)=FLOAT(INWVOL)
                 ENDIF
              ENDIF
C             -- leaving end toroids...
              IF(ANAME(1:2).EQ.'ME') THEN
                 IF(OUTMETD.EQ.0) THEN
                    OUTMETD=1
                    NNTRJ=NNTRJ+1
                 ENDIF
                 IF(NNTRJ.LT.MXNTRJ) THEN
                    XYZTRJ(1,NNTRJ)=420001. 
                    CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                    XYZTRJ(9,NNTRJ)=SLENG
                    XYZTRJ(10,NNTRJ)=FLOAT(INWVOL)
                 ENDIF
              ENDIF
C             -- leaving SAMUS toroids...
              CALL MSGNUM(NAMES(NLEVEL),ANAME,NUMB)
              IF(ANAME(1:2).EQ.'SM') THEN
                 IF(OUTSMXX.EQ.0) THEN
                    OUTSMXX=1
                    NNTRJ=NNTRJ+1
                 ENDIF
                 IF(NNTRJ.LT.MXNTRJ) THEN
                    XYZTRJ(1,NNTRJ)=600001.
                    CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                    XYZTRJ(9,NNTRJ)=SLENG
                    XYZTRJ(10,NNTRJ)=FLOAT(INWVOL)
                 ENDIF
              ENDIF
              ENDIF
            ENDIF       ! ((( if(names(2).eq.hmmuo) )))            
C
         ENDIF          ! ((( if(nlevel.ge.2) )))
         ENDIF          ! ((( if(inwvol.eq.2) )))
         GO TO 999
      ENDIF

      IF(GOGO.EQ.'INIT') THEN
C        -- initialize some variables at the first call of this job...
         IF(IFIRST.EQ.0) THEN
            IFIRST=1
            CALL UCTOH('MCEN',HMCEN,4,4)
            CALL UCTOH('MCAL',HMCAL,4,4)
            CALL UCTOH('MMUO',HMMUO,4,4)
         ENDIF
C
C        -- initialize at the beginning of each track...
C
         INMVOL=0
         INMCEN=0
         INMCAL=0
         INMMUO=0
         CALL VZERO(INPDT,MXNPDT)
         CALL VZERO(INPDTC,MXNPDT)
         INMCXX=0
         INMETD=0
         OUTMCXX=0
         OUTMETD=0
         CALL VZERO(INSAM,MXNSAM)
         CALL VZERO(INSAMC,MXNSAM)
         INSMXX=0
         OUTSMXX=0
C
         NNTRJ=1
C
         GO TO 999
      ENDIF

      IF(GOGO.EQ.'FILL') THEN
C        -- do not fill, if track does not reach to the muon system...
         IF(INMMUO.EQ.0) THEN
            GO TO 999
         ENDIF
C        -- book Muon bank...
         CALL BKMUON(0,LMUON)
C        -- book MTRJ bank...
         NDATA=5+NNTRJ*NWDS
         CALL BKMTRJ(LMUON,NDATA,LMTRJ)
         IQ(LMTRJ+1)=3     ! version number
         IQ(LMTRJ+2)=NWDS 
         IQ(LMTRJ+3)=NNTRJ
         IQ(LMTRJ+4)=0
         IQ(LMTRJ+5)=0
C        -- add vertex point...
         CALL UCOPY(VERT,XYZTRJ(2,1),3)
         XYZTRJ(1,1)=1.0
         XYZTRJ(8,1)=SQRT(PVERT(1)**2+PVERT(2)**2+PVERT(3)**2)
         XYZTRJ(5,1)=PVERT(1)/XYZTRJ(8,1)
         XYZTRJ(6,1)=PVERT(2)/XYZTRJ(8,1)
         XYZTRJ(7,1)=PVERT(3)/XYZTRJ(8,1)
         XYZTRJ(9,1)=0.0
         XYZTRJ(10,1)=0.0
C
         CALL UCOPY(XYZTRJ(1,1),Q(LMTRJ+6),NNTRJ*NWDS)
C
C        WRITE(LOUT,60) NNTRJ
C        IF(NNTRJ.NE.0) THEN
C           DO 390 I=1,NNTRJ
C              WRITE(LOUT,61) I,(XYZTRJ(J,I),J=1,10)
C 390       CONTINUE
C        ENDIF
C 60     FORMAT(///' DEBUG IN MSTRAK...    NNTRJ=',I5)
C 61     FORMAT(5X,I5,3X,F8.0,3X,3F8.2,3X,4F9.3,2X,2F10.1)
      ENDIF

  999 RETURN
      END
