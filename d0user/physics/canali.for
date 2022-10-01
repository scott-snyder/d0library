      FUNCTION CANALI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     CALORIMETER analysis subroutine.
C-     
C-  ENTRY CALSTA: give status
C-  ENTRY CALDIA: available for user dialog       
C-
C-   Created   6-FEB-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CANALI,CALSTA,CALDIA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER N1,N2,N3,N4,N5,LEGO1,LEGO2,LEGO3,LEGO4,LEGO5
      INTEGER LCAEH,GZCAEH,I,K,POINTR,NR,NCH,LAYER
      INTEGER LCATE,GZCATE,LPNUT,GZPNUT
      INTEGER LDCATE,NCHEM,NCHTOT,NCELLE,NCELLS,NJETS,NCACL
      INTEGER LJETS,LCACL,GZJETS,GZCACL,GZCAEP,NANAL
      REAL    E,ET,EMAX,ETMAX,ENUT(4),ETOT
      INTEGER NJTMAX
      PARAMETER (NJTMAX=10)
      REAL PJETS(4,10),P(4),JMASS
      REAL    TH,ETA,PHI,SIG(3)
      CHARACTER*60 MESG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
C   
C          SET UP HISTOGRAMS    
C   
        CALL HCDIR('//PAWC',' ')    ! go to top directory
        CALL HMDIR('CANALYSIS','S')
        CALL HBOOK1(1,' TOTAL ENERGY$',50,0.,2000.,0.)
        CALL HBOOK1(2,' Maximum Et in a cell$',50,0.,50.,0.0)
        CALL HBOOK1(3,' Maximum E in a cell$',50,0.,100.,0.0)
        CALL HBOOK1(4,' Maximum  Et in a tower$',50,0.,100.,0.0)
        CALL HBOOK1(5,' Maximum EM Et in a tower$',50,0.,50.,0.0)
        CALL HBOOK1(6,' Jets Et $',50,0.,200.,0.0)
        CALL HBOOK1(7,' Clusters Et $',50,0.,100.,0.0)
        CALL HBOOK1(8,' Missing Et 1$',50,0.,100.,0.0)
        CALL HBOOK1(9,' Missing Et 2$',50,0.,100.,0.0)
        CALL HBOOK1(10,' Jet Pair Masses$',50,0.,500.,0.0)
        FIRST=.FALSE.
      ENDIF
C
      NANAL=NANAL+1
      CANALI=.TRUE.
C
C         Hits and towers and corrections
C
      IF(GZCAEP().NE.0) THEN   
        CALL CAHITS
        CALL HCDIR('//PAWC/CANALYSIS',' ')  ! go to CANALYSIS directory
        LCAEH=GZCAEH()
        NR=IQ(LCAEH+2)
        NCH=IQ(LCAEH+3)
        POINTR=LCAEH
C
C         now book and fill LEGO banks for lego plots
C
C    
C         loop over channels
C
C  find number of channels in massless gaps and ICD
        DO  I=1,NCH 
          LAYER=IQ(POINTR+14)
          K=LAYER-7
          IF(K.GT.0.AND.K.LT.4) NCELLS=NCELLS+1
          POINTR=POINTR+NR
        ENDDO
C           reserve links for lego plots
        CALL GSLINK('CANALI',N1) 
C             book lego plots
        CALL BKLEGO(' phi vs. eta ET in CC/EC',NCELLS,LEGO1)
        LSLINK(N1)=LEGO1
C
        LCAEH=GZCAEH()
        NR=IQ(LCAEH+2)
        NCH=IQ(LCAEH+3)
        POINTR=LCAEH
        NCELLS=0
        EMAX=0
        ETMAX=0
        ETOT=0
C
        DO  I=1,NCH
          E=Q(POINTR+7)
          ET=Q(POINTR+8)
          ETOT=ETOT+E
          IF(EMAX.LT.E) EMAX=E
          IF(ETMAX.LT.ET) ETMAX=ET
          LAYER=IQ(POINTR+14)
          K=LAYER-7
          IF(K.GT.0.AND.K.LT.4) THEN
            CALL ETOETA(Q(POINTR+4),PHI,TH,ETA)
            LEGO1=LSLINK(N1)+NCELLS*3
            Q(LEGO1+11)=PHI
            Q(LEGO1+12)=ETA
            Q(LEGO1+13)=ET
            NCELLS=NCELLS+1
          ENDIF
          POINTR=POINTR+NR
        ENDDO
        CALL HFILL(1,ETOT,0.,1.)
        CALL HFILL(2,ETMAX,0.,1.)
        CALL HFILL(3,EMAX,0.,1.)
C
C       Analyze towers
        LCATE=GZCATE()
        NR=IQ(LCATE+2)
        NCHTOT=MOD(IQ(LCATE+3),10000)
        NCHEM=IQ(LCATE+3)/10000
C
C         towers
        NCELLS=0
        NCELLE=0
        ETMAX=0
        LDCATE=GZCATE()
        DO 11 I=1,NCHEM
          ET=Q(LDCATE+8)
          IF(ET.GT.ETMAX) ETMAX=ET
   11   CONTINUE
        CALL HFILL(5,ETMAX,0.,1.)
        DO 12 I=NCHEM+1,NCHTOT
          ET=Q(LDCATE+8)
          IF(ET.GT.ETMAX) ETMAX=ET
          LDCATE=LDCATE+NR
   12   CONTINUE
        CALL HFILL(4,ETMAX,0.,1.)
        CALL RSLINK('CANALI',N1) 
      ELSE
        CALL ERRMSG('No CAEP bank','CANALI','Cannot get hits or towers',
     &    'W')
      ENDIF
C
C           missing Et
C  
        DO I=1,2
          LPNUT=GZPNUT(I)
          IF(LPNUT.GT.0) THEN
            ET=Q(LPNUT+7)
            CALL HFILL(7+I,ET,0.,1.)
          ENDIF
        ENDDO
C       
C      jets
C
C       find # of jets
        NJETS=0
        LJETS=GZJETS()
   21   IF(LJETS.NE.0) THEN
          NJETS=NJETS+1
          ET=Q(LJETS+6)
          CALL HFILL(6,ET,0.,1.)
          IF(NJETS.LE.NJTMAX) CALL UCOPY(Q(LJETS+2),PJETS(1,NJETS),4)
          LJETS=LQ(LJETS)
          GOTO 21
        ENDIF
C
C             calculate jet pair masses
        IF(NJETS.GT.1) THEN
          IF(NJETS.GT.NJTMAX) NJETS=NJTMAX
          DO I=1,NJETS-1
            DO K=I+1,NJETS
              CALL VADD(PJETS(1,I),PJETS(1,K),P,4)
              JMASS=P(4)**2-P(1)**2-P(2)**2-P(3)**2
              IF(JMASS.GT.0.) JMASS=SQRT(JMASS)
              CALL HFILL(10,JMASS,0.,1.0)
            ENDDO
          ENDDO
        ENDIF
C
C       
C      clusters
C
C       find # of clusters
      NCACL=0
      LCACL=GZCACL()
   23 IF(LCACL.NE.0) THEN
        NCACL=NCACL+1
        LCACL=LQ(LCACL)
        ET=Q(LCACL+8)
        CALL HFILL(7,ET,0.,1.)
        GOTO 23
      ENDIF
  999 RETURN
C
C
      ENTRY CALSTA()
C
      CALSTA=.TRUE.
      WRITE(MESG,1000) NANAL
 1000 FORMAT(I5,' events analyzed by CANALI')
      CALL INTMSG(MESG)
      RETURN
C
C
      ENTRY CALDIA()
C
C       add here any USER DIALOG
      RETURN
      END
