      SUBROUTINE UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
C----------------------------------------------------------------------
C-   Purpose and Methods : unpack information from TPRL
C-   Inputs  : LTPRL         integer       link to TPRL bank to be unpacked
C-   Outputs : IER           integer       error code (0=OK)
C-             VERSION       real          if <2.0 then some quantities
C-                                         are missing
C-             REAL_WORD     real(100)     see below
C-             INTEGER_WORD  integer(100)  see below
C-   Created  18-DEC-1992   Alain PLUQUET
C-   Updated  23-JUN-1993   Alain PLUQUET  changes bandwidth for cluster
C-                                         energies
C-   Updated   5-JUL-1993   Alain PLUQUET  adds local density of TRD hits
C-                                         moves documentation to TPRL.ZEB
C-   Updated  15-OCT-1993   Alain PLUQUET changes unpacking for 512 wires
C-   Updated  26-NOV-1993   A. ZYLBERSTEJN
C-   Updated  15-JUN-1995   A. Zylberstejn: add Z inters of track in anode plane
C-   Updated  21-JUN-1995   A. ZYLBERSTEJN    Transform version 2 format to
C-                                            version>3 format for clusters
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INTEGER IER,I,JS,KS,NA,NC,NCLA,NCLC,JBYT,A,B,LAYER,SECTOR
      INTEGER LOUT,LTPRL,LTRDT,LSTART,LNEXT,TRUNIT,J,L,K
      REAL ANODE_HV(2),WINDOW_HV(2),POTENTIAL_HV(2),VERSION
      INTEGER ANODE_HV_MODULE(4)
      INTEGER WINDOW_HV_MODULE(4)
      INTEGER POTENTIAL_HV_MODULE(4)
      LOGICAL DOPRINT,TRD_DO_PRINT,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        DOPRINT=.FALSE.
C        doprint=.true.
      END IF
      DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
      DO I=1,NWORD
        REAL_WORD(I)=0.
        INTEGER_WORD(I)=0
      ENDDO
      IF(LTPRL.LE.0)GO TO 999
      VERSION=Q(LTPRL+1)
      IF (VERSION.GE.2.0) THEN
C-------------------------------------------------------------------
C EPICOR*100. <--> bits 1 to 17          EPICOR in [0000.00,1310.71]
C B*1000 <---> bits 18 to 32             B in [00.000,32.767]
C-------------------------------------------------------------------
        LTRDT=LQ(LTPRL+1)
        IF (LQ(LTRDT-1).EQ.LTPRL) LAYER=1
        IF (LQ(LTRDT-2).EQ.LTPRL) LAYER=2
        IF (LQ(LTRDT-3).EQ.LTPRL) LAYER=3
        IF(DOPRINT)
     &    WRITE(LOUT,'(a30,/,a30)')' enter UNPACK_TPRL',
     &    ' ----------------'
        REAL_WORD(1)=FLOAT(JBYT(IQ(LTPRL+5),1,17))/100.
        REAL_WORD(2)=FLOAT(JBYT(IQ(LTPRL+5),18,15))/1000.
C-------------------------------------------------------------------
C (3.+Distance of closest anode)*10000 <--> bits 1 to 16 phi
C                                                 in [-3.0000,3.5535]
C |sin(theta)|*10000 track     <--> bits 17 to 32
C                                        sin(theta)in [0.0000,6.5535]
C-------------------------------------------------------------------
        REAL_WORD(41)=FLOAT(JBYT(IQ(LTPRL+3),1,16))/10000.-3.
        REAL_WORD(42)=FLOAT(JBYT(IQ(LTPRL+3),17,16))/10000.
C-------------------------------------------------------------------
C        if(doprint)write(lout,*)' In unpack_tprl,real_word',
C        (real_word(i),i=1,4)
C                    correction in [0.000,65.535]
C gas correction*1000          <--> bits 1  to 16
C high voltage correction*1000 <--> bits 17 to 32
C-------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,1000.,43,2,2,LTPRL+15,LNEXT)
C-------------------------------------------------------------------
C HV ANODE     for 1st hit wire <--> bits 1 to 11     HV in [0000.,2047.]
C HV WINDOW    for 1st hit wire <--> bits 12 to 22    HV in [0000.,2047.]
C HV POTENTIAL for 1st hit wire <--> bits 23 to 32    HV in [0000.,1023.]
C etc for 2nd,3rd,4th hit wire
C-------------------------------------------------------------------
        REAL_WORD(3)=FLOAT(JBYT(IQ(LTPRL+6),1,11))
        REAL_WORD(4)=-FLOAT(JBYT(IQ(LTPRL+6),12,11))
        REAL_WORD(5)=FLOAT(JBYT(IQ(LTPRL+6),23,10))
        REAL_WORD(6)=FLOAT(JBYT(IQ(LTPRL+7),1,11))
        REAL_WORD(7)=-FLOAT(JBYT(IQ(LTPRL+7),12,11))
        REAL_WORD(8)=FLOAT(JBYT(IQ(LTPRL+7),23,10))
        REAL_WORD(9)=FLOAT(JBYT(IQ(LTPRL+8),1,11))
        REAL_WORD(10)=-FLOAT(JBYT(IQ(LTPRL+8),12,11))
        REAL_WORD(11)=FLOAT(JBYT(IQ(LTPRL+8),23,10))
        REAL_WORD(12)=FLOAT(JBYT(IQ(LTPRL+9),1,11))
        REAL_WORD(13)=-FLOAT(JBYT(IQ(LTPRL+9),12,11))
        REAL_WORD(14)=FLOAT(JBYT(IQ(LTPRL+9),23,10))
C------------------------------------------------------------------
C PEDESTAL*1000.                        pedestal in [00.000,65.535]
C 1st hit wire <--> bits 1  to 16
C 2nd hit wire <--> bits 17 to 32
C 3rd hit wire <--> bits 1  to 16
C 4th hit wire <--> bits 17 to 32
C------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,1000.,15,4,2,LTPRL+10,LNEXT)
C------------------------------------------------------------------
C ENERGY*100 on adjacent       energy in [00.00,655.35]
C adjacent wire inf <--> bits 1  to 16
C adjacent wire sup <--> bits 17 to 32
C------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,100.,19,2,2,LTPRL+14,LNEXT)
C------------------------------------------------------------------
C coded word for hit anode cells   <--->  bits 1  to 8   word in [0,255]
C coded word for hit cathode cells <--->  bits 9  to 16  word in [0,255]
C------------------------------------------------------------------
        CALL UNPACK_INTEGER (INTEGER_WORD,1,2,4,LTPRL+16,LNEXT)
C------------------------------------------------------------------
C SECTOR correction correction in [0.00,2.55]
C 1st hit wire*100<--> bits 1 to 8
C 2nd hit wire*100<--> bits 9 to 16
C 3rd hit wire*100<--> bits 17 to 24
C 4th hit wire*100<--> bits 25 to 32
C------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,100.,21,4,4,LTPRL+18,LNEXT)
C------------------------------------------------------------------
C WIRE correction correction in [0.00,2.55]
C 1st hit wire*100 <--> bits 1 to 8
C 2nd hit wire*100 <--> bits 9 to 16
C 3rd hit wire*100 <--> bits 17 to 24
C 4th hit wire*100 <--> bits 25 to 32
C-------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,100.,25,4,4,LTPRL+19,LNEXT)
C-------------------------------------------------------------------
C electronic gain                         correction in [0.00,2.55]
C 1st hit wire*100 <--> bits 1 to 8
C 2nd hit wire*100 <--> bits 9 to 16
C 3rd hit wire*100 <--> bits 17 to 24
C 4th hit wire*100 <--> bits 25 to 32
C------------------------------------------------------------------------
        CALL UNPACK_REAL (REAL_WORD,100.,29,4,4,LTPRL+20,LNEXT)
C------------------------------------------------------------------------
C multiplicity (1=1track) for 1st hit anode <--> bits 1 to 8
C multiplicity (1=1track) for 2nd hit anode <--> bits 9 to 16
C multiplicity (1=1track) for 3rd hit anode <--> bits 17 to 24
C multiplicity (1=1track) for 4th hit anode <--> bits 25 to 32
C-------------------------------------------------------------------------
        CALL UNPACK_INTEGER (INTEGER_WORD,8,4,4,LTPRL+17,LNEXT)
C-------------------------------------------------------------------------
C Number of hit anodes NA  <--> bits 1 to 8
C Number of hit cathodes NC <--> bits 9 to 16
C Number of reconstructed anode clusters   NCLA  <--> bits 17 to 24
C Number of reconstructed cathode clusters NCLC  <--> bits 25 to 32
C-------------------------------------------------------------------------
        LSTART=LTPRL+25
        CALL UNPACK_INTEGER (INTEGER_WORD,4,4,4,LSTART,LNEXT)
        NA=INTEGER_WORD(4)
        NC=INTEGER_WORD(5)
        NCLA=INTEGER_WORD(6)
        NCLC=INTEGER_WORD(7)
        IF(DOPRINT)WRITE(LOUT,4037)NA,NC,NCLA,NCLC
 4037   FORMAT( ' in UNPACK_TPRL: na,nc',2I3,'ncla,nclc',2I3)
C Fix for emporary problem in packing the corrections
        IF(NA.GT.1 .AND. REAL_WORD(6).LE.0.)THEN
          DO I=6,8
            REAL_WORD(I)=REAL_WORD(I+3)
            REAL_WORD(I+6)=REAL_WORD(I+3)
          END DO
        END IF
C-------------------------------------------------------------------------
C Hit anode-1 number unpacked 4 by 4               number in   [1,256]
C Energy*100. hit anode unpacked 2 by 2            energy in   [00.00,655.35]
C Hit cathode number-1 unpacked 4 by 4             number in   [1,256]
C Energy*100. hit cathode unpacked 2 by 2          energy in   [00.00,655.35]
C Energy*10000. for 1st anode cluster            energy in   [00.00,655.35]
C Position for 1st anode cluster                 position in [0,255]
C Energy*10000. for 1st cathode cluster          energy in   [00.00,655.35]
C Position for 1st cathode cluster               position in [0,255]
C-------------------------------------------------------------------------
        IF (VERSION.LT.3.) THEN
          LSTART=LTPRL+26
          CALL UNPACK_INTEGER (INTEGER_WORD,51,NA,4,LSTART,LNEXT)
        ELSE
          IF (MOD(IFIX(VERSION*1000.),1000).EQ.256) THEN
            CALL UNPACK_INTEGER (INTEGER_WORD,51,NA,4,LNEXT,LNEXT)
          ELSEIF (MOD(IFIX(VERSION*1000.),1000).EQ.512) THEN
            CALL UNPACK_INTEGER (INTEGER_WORD,51,NA,2,LNEXT,LNEXT)
          ELSE
            CALL ERRMSG
     &          (' UNPACK_TPRL','UNPACK_TPRL',
     &          'number of wires is not valid','W')
          ENDIF
        ENDIF
        REAL_WORD(49)=-999
        REAL_WORD(50)=-999.
        CALL UNPACK_REAL (REAL_WORD,100.,51,NA,2,LNEXT,LNEXT)
        IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,'anode,energy',
     &      (INTEGER_WORD(50+I),REAL_WORD(50+I),I=1,NA)
        CALL UNPACK_INTEGER (INTEGER_WORD,51+NA,NC,4,LNEXT,LNEXT)
        CALL UNPACK_REAL (REAL_WORD,100.,51+NA,NC,2,LNEXT,LNEXT)
        IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,'cath.,energy',
     &      (INTEGER_WORD(50+NA+I),REAL_WORD(50+NA+I),I=1,NC)
        J=NWORD-4
        REAL_WORD(J+1)=FLOAT(JBYT(IQ(LTPRL+23),1,10))/100.
        REAL_WORD(J+2)=FLOAT(JBYT(IQ(LTPRL+23),11,10))/10.
        REAL_WORD(J+3)= (1-2*JBYT(IQ(LTPRL+23),31,1))*
     &    FLOAT(JBYT(IQ(LTPRL+23),21,10)) /10.
        REAL_WORD(J+4)=FLOAT(IQ(LTPRL+24))/100.
C        print*,' in unpack_tprl, pointeur',J+1,
C     &    ' phi,r,z',REAL_WORD(J+1),REAL_WORD(J+2),REAL_WORD(J+3)
        IF(DOPRINT)WRITE(LOUT,*)' in unpack_tprl, pointeur',J+1,
     &    ' phi,r,z',REAL_WORD(J+1),REAL_WORD(J+2),REAL_WORD(J+3)
        IF(VERSION.LT.3.)THEN
          CALL UNPACK_REAL(REAL_WORD,100.,51+NA+NC,NCLA,2,LNEXT,LNEXT)
          CALL UNPACK_INTEGER (INTEGER_WORD,51+NA+NC,NCLA,4,LNEXT,LNEXT)
          CALL UNPACK_REAL (REAL_WORD,100.,51+NA+NC+NCLA,NCLC,2,LNEXT,
     &      LNEXT)
          CALL UNPACK_INTEGER (INTEGER_WORD,51+NA+NC+NCLA,NCLC,4,LNEXT,
     &      LNEXT)
        ELSE
C          IF(DOPRINT)WRITE(LOUT,*)' ncla',NCLA
          JS=50+NA+NC
          IF(JS+5*NCLA.GT.NWORD)THEN
            CALL ERRMSG('too many words for TRD info',
     &            'UNPACK_TPRL',' ','W')
            NCLA=0
            NCLC=0
          END IF
          IF(NCLA.NE.0)THEN
C  Clusters  anodes
C  Eclust cath,Left clust,Center clust,Right clust,Peak value
            DO I=1,NCLA
              JS=JS+1
              REAL_WORD(JS)=IQ(LNEXT)*.01
              LNEXT=LNEXT+1
C              JS=JS+1
              CALL UNPACK_INTEGER(INTEGER_WORD,JS,4,4,LNEXT,LNEXT)
              IF(DOPRINT)WRITE(LOUT,*)' in unpack_tprl cluster anode',
     &          'js',JS,' enegery clus',REAL_WORD(JS),' left',
     &          INTEGER_WORD(JS)
              JS=JS+4
            END DO
C     cathodes
            IF(JS+5*NCLC.GT.NWORD)THEN
              CALL ERRMSG('too many words for TRD info',
     &            'UNPACK_TPRL',' ','W')
              NCLC=0
            END IF
            IF(NCLC.NE.0)THEN
              DO I=1,NCLC
                JS=JS+1
                REAL_WORD(JS)=IQ(LNEXT)*.01
                LNEXT=LNEXT+1
C                JS=JS+1
                CALL UNPACK_INTEGER(INTEGER_WORD,JS,4,4,LNEXT, LNEXT)
C              IF(DOPRINT)WRITE(LOUT,*)' cluster cath js',JS,
C     &          ' enegery clus',REAL_WORD(JS),' left',integer_word(js)
                JS=JS+4
              END DO
            END IF
          END IF
C  unpack number of clusters per wire
          IF(VERSION.GE.3.)THEN
            JS=JS+1
            CALL    UNPACK_INTEGER (INTEGER_WORD,JS,MIN0(NA,8),4,
     &        LNEXT,LNEXT)
            IF(DOPRINT)WRITE(LOUT,*)
     &        ' in unpack nb. of clusters per chan. anode JS',
     &        JS,(INTEGER_WORD(I),I=JS,JS+NA-1)
            JS=JS+NA
            CALL    UNPACK_INTEGER (INTEGER_WORD,JS,MIN0(NC,8),4,
     &        LNEXT,LNEXT)
            IF(DOPRINT)WRITE(LOUT,*)
     &        ' nb. of clusters per chan. cath. JS',
     &        JS,(INTEGER_WORD(I),I=JS,JS+NC-1)
          END IF
        END IF
        DO I=51,50+NA
          INTEGER_WORD(I)=INTEGER_WORD(I)+1
        ENDDO
        DO I=51+NA,50+NA+NC
          INTEGER_WORD(I)=INTEGER_WORD(I)+1
        ENDDO
C-----------------------------------------------------------------------
C Unpacked words/coded words
C-----------------------------------------------------------------------
        INTEGER_WORD(12)=MOD(IQ(LTPRL+21),1000)! total nb. of hit anodes
        INTEGER_WORD(13)=IQ(LTPRL+21)/1000 ! total nb. of hit cathodes
        IF(DOPRINT)WRITE(LOUT,*)' ncoded',INTEGER_WORD(12),
     &      INTEGER_WORD(13)
        INTEGER_WORD(3)=IQ(LTPRL+22)       ! local density of TRD HITS
        REAL_WORD(45)=Q(LTPRL+2)
        REAL_WORD(46)=Q(LTPRL+12)
        REAL_WORD(47)=Q(LTPRL+13)
      ELSE
C----------------------------------------------------------
C information from old versions of TPRL
C----------------------------------------------------------

        REAL_WORD(19)=Q(LTPRL+10)
        REAL_WORD(20)=Q(LTPRL+11)
        REAL_WORD(45)=Q(LTPRL+2)
        REAL_WORD(46)=Q(LTPRL+12)
        REAL_WORD(47)=Q(LTPRL+13)
        NA=IQ(LTPRL+14)
        NC=IQ(LTPRL+15)
        NCLA=IQ(LTPRL+16)
        NCLC=IQ(LTPRL+17)
        INTEGER_WORD(4)=NA
        INTEGER_WORD(5)=NC
        INTEGER_WORD(6)=NCLA
        INTEGER_WORD(7)=NCLC
        LTRDT=LQ(LTPRL+1)
        IF (LQ(LTRDT-1).EQ.LTPRL) LAYER=1
        IF (LQ(LTRDT-2).EQ.LTPRL) LAYER=2
        IF (LQ(LTRDT-3).EQ.LTPRL) LAYER=3
        DO I=1,NA
          REAL_WORD(50+I)=Q(LTPRL+17+NA+I)
          INTEGER_WORD(50+I)=IFIX(Q(LTPRL+17+I))
          IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' anode',
     &        INTEGER_WORD(50+I),' energy',REAL_WORD(50+I)
        ENDDO
        IF(NC.Ge.1 )THEN
          j=0
          DO I=1,NC
            INTEGER_WORD(50+NA+I)=0
            REAL_WORD(50+NA+I)=0.
            if(Q(LTPRL+17+2*NA+NC+I).lt.0.2)go to 456
            j=j+1
            IF(Q(LTPRL+17+2*NA+I).LE.0.)THEN
              if(doprint)write(lout,*)
     +         ' in unpack_tprl,na,nc',NA,I,'LTPRL+17+2*NA+I',
     &          LTPRL+17+2*NA+I,' q',Q(LTPRL+17+2*NA+I),' version',
     +          VERSION
              GO TO 456
            END IF
            REAL_WORD(50+NA+j)=Q(LTPRL+17+2*NA+NC+I)
            INTEGER_WORD(50+NA+j)=Q(LTPRL+17+2*NA+I)
            IF(DOPRINT)WRITE(LOUT,*)' layer',LAYER,' cath.',
     &        INTEGER_WORD(50+j+NA),' energy',REAL_WORD(50+j+NA)
  456       CONTINUE
          ENDDO
        INTEGER_WORD(5)=6
        END IF
        A=18+2*(NA+NC)-1
        B=A+2*NCLA
C        IF(NCLA.NE.0)THEN
C          JS=51+NA+NC
C     anodes
        DO I=1,NCLA
          REAL_WORD(50+NA+NC+I)=Q(LTPRL+A+2*(I-1)+1)
          INTEGER_WORD(50+NA+NC+I)=IFIX(Q(LTPRL+A+2*I))
        ENDDO
        DO I=1,NCLC
          REAL_WORD(50+NA+NC+NCLA+I)=Q(LTPRL+B+2*(I-1)+1)
          INTEGER_WORD(50+NA+NC+NCLA+I)=Q(LTPRL+B+2*I)
        ENDDO
C        end if
        SECTOR=INTEGER_WORD(12)/16    ! SECTOR NUMBER IN [0,15]
        IF (LAYER.EQ.1) ANODE_HV_MODULE(1)=SECTOR+1
        IF (LAYER.EQ.2) ANODE_HV_MODULE(1)=SECTOR/2+17
        IF (LAYER.EQ.3) ANODE_HV_MODULE(1)=SECTOR/2+25
        POTENTIAL_HV_MODULE(1)=3*SECTOR/8+32+LAYER
        WINDOW_HV_MODULE(1)=POTENTIAL_HV_MODULE(1)+8
        ANODE_HV(1)=MOD(Q(LTPRL+6),10000.)
        ANODE_HV(2)=(Q(LTPRL+6)-MOD(Q(LTPRL+6),10000.))/10000.
        POTENTIAL_HV(1)=MOD(Q(LTPRL+7),10000.)
        POTENTIAL_HV(2)=(Q(LTPRL+7)-MOD(Q(LTPRL+7),10000.))/10000.
        WINDOW_HV(1)=-MOD(Q(LTPRL+8),10000.)
        WINDOW_HV(2)=-(Q(LTPRL+8)-MOD(Q(LTPRL+8),10000.))/10000.
        REAL_WORD(3)=ANODE_HV(1)
        REAL_WORD(4)=WINDOW_HV(1)
        REAL_WORD(5)=POTENTIAL_HV(1)
        DO I=2,NA
          SECTOR=INTEGER_WORD(11+I)/16    ! SECTOR NUMBER IN [0,15]
          IF (LAYER.EQ.1) ANODE_HV_MODULE(I)=SECTOR+1
          IF (LAYER.EQ.2) ANODE_HV_MODULE(I)=SECTOR/2+17
          IF (LAYER.EQ.3) ANODE_HV_MODULE(I)=SECTOR/2+25
          POTENTIAL_HV_MODULE(I)=3*SECTOR/8+32+LAYER
          WINDOW_HV_MODULE(I)=POTENTIAL_HV_MODULE(I)+8
          IF (ANODE_HV_MODULE(I).EQ.ANODE_HV_MODULE(1)) THEN
            REAL_WORD(3*I)=ANODE_HV(1)
          ELSE
            REAL_WORD(3*I)=ANODE_HV(2)
          ENDIF
          IF (WINDOW_HV_MODULE(I).EQ.WINDOW_HV_MODULE(1)) THEN
            REAL_WORD(3*I+1)=WINDOW_HV(1)
          ELSE
            REAL_WORD(3*I+1)=WINDOW_HV(2)
          ENDIF
          IF (POTENTIAL_HV_MODULE(I).EQ.POTENTIAL_HV_MODULE(1)) THEN
            REAL_WORD(3*I+2)=POTENTIAL_HV(1)
          ELSE
            REAL_WORD(3*I+2)=POTENTIAL_HV(2)
          ENDIF
        ENDDO
      ENDIF
      IF(DOPRINT)WRITE(LOUT,*)' in unpack_tprl,version',VERSION,
     &    ' ncla,nclc',NCLA,NCLC
      IF(VERSION.LT.3.)THEN ! convert old version foramt of clusters in new
                            ! version
        IWS(1)=NCLA
        IWS(2)=NCLC
        IF(NCLA.NE.0)THEN
          J=50+NA+NC
          DO I=1,NCLA
            WS(I+100)=REAL_WORD(J+I)
            IWS(I+200)=INTEGER_WORD(J+I)
C            PRINT*,' old clust anode ',I,' pointeur',J+I,
C     &        ' energy,position',
C     &        REAL_WORD(J+I),INTEGER_WORD(J+I)
          END DO
        END IF
        IF(NCLC.NE.0)THEN
          J=50+NA+NC+NCLA
          DO I=1,NCLC
            WS(I+300)=REAL_WORD(J+I)
            IWS(I+400)=INTEGER_WORD(J+I)
C            PRINT*,' old clust cath ',I,' pointeur',J+I,
C     &        ' energy,position',
C     &        REAL_WORD(J+I),INTEGER_WORD(J+I)
          END DO
        END IF
        L=50+NA+NC
        DO K=1,2 ! anodes,cathodes
          IF(IWS(K).NE.0)THEN ! request clusters
            DO I=1,IWS(K)
              REAL_WORD(L+1+(I-1)*5)=0.
              INTEGER_WORD(L+1+(I-1)*5)=0
              IF(I.LE.4)THEN
                REAL_WORD(L+1+(I-1)*5)=WS(I+200*K-100)
                INTEGER_WORD(L+1+(I-1)*5)=IWS(I+200*K)
              END IF
              INTEGER_WORD(L+2+(I-1)*5)=0
              INTEGER_WORD(L+3+(I-1)*5)=0
              INTEGER_WORD(L+4+(I-1)*5)=0
              INTEGER_WORD(L+5+(I-1)*5)=0
C            PRINT*,' new clust',K,' NB ',i,' POINTEUR',
C     &        L+1+(I-1)*5,' ENERGY,POSITION',
C     &        REAL_WORD(L+1+(I-1)*5),INTEGER_WORD(L+1+(I-1)*5)
            END DO
          END IF
          L=L+5*NCLA
        END DO
      END IF
      IF(DOPRINT)
     &    WRITE(LOUT,'(A30,/,A30)')' EXIT UNPACK_TPRL',
     &    ' ----------------'
  999 RETURN
      END
