      SUBROUTINE KT_READ_CAEH(NEG_CELLS,NEG_ETA,NEG_PHI,
     &  NEGATIVE,POS_CELLS,POS_ETA,POS_ETA2,POS_PHI,POS_PHI2,POSITIVE,
     &  EM,ICD,FH,CELL_COUNTER,CELL_EM,CELL_ICD,CELL_FH, ier)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO READ CELLS FROM THE CAEH BANK, AND PRECLUSTER
C-                          ALL CELLS WITHIN A RADIUS OF 0.2
C-
C-   Returned value  :
C-   Inputs  : NONE
C-
C-   Outputs : NEG_CELLS      NUMBER OF NEGATIVE ET CELLS
C-             NEG_ETA        ONE DIMENSIONAL ARRAY CONTAINING ETA OF NEG CELLS
C-             NEG_PHI        ONE DIMENSIONAL ARRAY CONTAINING PHI OF NEG CELLS
C-             NEGATIVE       2-D ARRAY CONTAINING ET OF NEG CELLS
C-             POS_CELLS      NUMBER OF POSITIVE CELLS
C-             POS_ETA        1-D ARRAY CONTAINING ETA OF POS CELLS
C-             POS_ETA2       2-D ARRAY CONTAINING ETA OF POS CELLS
C-             POS_PHI        1-D ARRAY CONTAINING PHI OF POS CELLS
C-             POS_PHI2       2-D ARRAY CONTAINING PHI OF POS CELLS
C-             POSITIVE       2-D ARRAY CONTAINING ET OF POS CELLS
C-             EM             2-D ARRAY CONTAINING ET OF EM CELLS
C-             ICD            2-D ARRAY CONTAINING ET OF ICD CELLS
C-             FH             2-D ARRAY CONTAINING ET OF FH CELLS
C-             CELL_COUNTER   2-D ARRAY CONTAINING NUMBER OF CELLS IN PRECLUSTER
C-             CELL_EM        2-D ARRAY WITH NUMBER OF EM CELLS IN PRECLUSTER
C-             CELL_ICD       2-D ARRAY WITH NUMBER OF ICD CELLS IN PRECLUSTER
C-             CELL_FH        2-D ARRAY WITH NUMBER OF FH CELLS IN PRECLUSTER
c-             ier            Error code, 0 == all is well
c-
C-   Controls: NONE
C-
C-   Created  11-JUL-1995    Brad Abbott
c-   Updated  24-oct-1995    Return Error Code.
c-                           Use constants for arrays,  Use uzero to
c-                           zero things out
c-                           Gordon Watts
c-   Updated  13-nov-1995    Remove double "+" sign... how did vms miss this!?
c-                           Gordon Watts
c-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      INCLUDE 'd0$inc:pi.def'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$inc:KTJET.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
C
C-------------------------------------------------------------------
C
C..VARIABLES
      INTEGER NEG_CELLS,POS_CELLS
      INTEGER STATUS,IETANEW,IPHINEW
      INTEGER IER,I,J,K,CELLS,NEWK,PREC_ETA,PREC_PHI
      INTEGER IETA,IPHI,ILYR,ICHAN
      INTEGER WANTED,FOUND
      PARAMETER(WANTED=3)

      integer ieta_max, ieta_min
      integer iphi_max, iphi_min
      parameter (ieta_max = 46)
      parameter (ieta_min = -46)
      parameter (iphi_min = 0)
      parameter (iphi_max = 65)

      REAL NEGATIVE(ieta_min:ieta_max,iphi_min:iphi_max)
      real POSITIVE(ieta_min:ieta_max,iphi_min:iphi_max)
      REAL ET
      real POS_NEG(ieta_min:ieta_max,iphi_min:iphi_max)
      real DUMMY
      REAL POS_NEG_ETA2(ieta_min:ieta_max,iphi_min:iphi_max)
      real POS_NEG_PHI2(ieta_min:ieta_max,iphi_min:iphi_max)
      REAL SEX,SEY,CWEIGHT,PHI,THETA,ETA,DRMIN
      REAL EXC,EYC,EZC,EC,e(4),DETA,DPHI,DR
      REAL ETNEW,PHINEW,ETANEW,delta_phi,ICDNEW
      REAL FHNEW,CELLNEW_EM,CELLNEW_ICD,CELLNEW_FH
      REAL POS_ETA2(ieta_min:ieta_max,iphi_min:iphi_max)
      real POS_PHI2(ieta_min:ieta_max,iphi_min:iphi_max)
C
      REAL NEG_ETA(2000),NEG_PHI(2000),rphi1,rphi2
      REAL POS_ETA(15000),POS_PHI(15000),SORT_ET(17000)
      REAL POS_NEG_ETA(17000),POS_NEG_PHI(17000)

      REAL CELL_COUNTER(ieta_min:ieta_max,iphi_min:iphi_max)
      real EMNEW,CELLNEW
      REAL ICD(ieta_min:ieta_max,iphi_min:iphi_max)
      real EM(ieta_min:ieta_max,iphi_min:iphi_max)
      real FH(ieta_min:ieta_max,iphi_min:iphi_max)
      REAL CELL_EM(ieta_min:ieta_max,iphi_min:iphi_max)
      real CELL_ICD(ieta_min:ieta_max,iphi_min:iphi_max)
      REAL CELL_FH(ieta_min:ieta_max,iphi_min:iphi_max)
      real INFO(3,WANTED),Z,PETA_TO_DETA

      INTEGER LISAQ,ID
      REAL ISAQ_ETA,ISAQ_PHI,ISAQ_TH,ISAQ_E(4)

      INTEGER  LISP1,LISV1,IDV
      REAL ISP1_ETA,ISP1_PHI,ISP1_TH,ISP1_E(4),ISP1_PP(4)
      REAL XV1,YV1,ZV1

      INTEGER LCATD,GZCATD,II,OFF,ISEC,N,ICATD,CATD_IETA,CATD_IPHI
      REAL CATD_ETA,CATD_PHI,DELETA,CATD_E(4)
C
      LOGICAL START,OK

C----------------------------------------------------------------------
c
C STATEMENT FUNCTION
C
C---Relative Delta Phi. Equivalent to RPHI2-RPHI1 but take into account
C---wrap around to give smallest absolute value.
C
      DELTA_PHI(RPHI1,RPHI2) =
     &  MIN(MOD(ABS((RPHI1)-(RPHI2)),2*SNGL(PI)),
     &  2*SNGL(PI)-MOD(ABS((RPHI1)-(RPHI2)),2*SNGL(PI)))*
     &  SIGN(1.,(RPHI2)-(RPHI1))*((-1)**INT(((RPHI2)-(RPHI1))/SNGL(PI)))

C----------------------------------------------------------------------
C
C..   ZERO OUT ARRAYS

      call uzero (positive, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (negative, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (POS_NEG, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (EM, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (ICD, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (FH, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (POS_NEG_ETA2, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (POS_NEG_PHI2, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (pos_eta2, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (pos_phi2, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (CELL_COUNTER, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (CELL_EM, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (CELL_ICD, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))
      call uzero (CELL_FH, 1, (ieta_max - ieta_min + 1) *
     $     (iphi_max - iphi_min + 1))

      Z=0
      IF(USE_DET_ETA) THEN
        CALL VERTEX_INFO(WANTED,FOUND,INFO,OK)
        IF(OK) THEN
          Z=INFO(1,1)
        ELSE
          CALL ERRMSG ('CANNOT FIND VERTEX','KT_READ_CAEH',
     &                 '  ','W')
        ENDIF
      ENDIF

      LARGE_NEGATIVE_CELLS=0
      ET_REMOVED=0
      IF(input_type.EQ.6) THEN
C..  Get each cell and determine its physics eta and phi and project into
c..   physics eta towers
        START=.TRUE.
        IER=0
        POS_CELLS=0
        NEG_CELLS=0
        DO WHILE(IER.EQ.0)
          CALL GTCAEH(START,EXC,EYC,EZC,EC,ET,SEX,SEY,CWEIGHT,IETA,IPHI,
     &        ILYR,STATUS,ICHAN,IER)
          IF(IER.EQ.0) THEN
            START=.FALSE.
            IF(ET.LT.NEGATIVE_ET_CUT) THEN
              LARGE_NEGATIVE_CELLS=LARGE_NEGATIVE_CELLS+1
              ET_REMOVED=ET_REMOVED+ET
              GOTO 31
            ENDIF
            E(1)=EXC
            E(2)=EYC
            E(3)=EZC
            E(4)=EC
            CALL ETOETA(E,PHI,THETA,ETA)
            IF(USE_DET_ETA) ETA=PETA_TO_DETA(ETA,Z)
            IETA=INT(10*ABS(ETA)+0.5)
            IF(ETA.LT.0) IETA=-IETA
            IF(ABS(IETA).GT.45) IETA=46
            IPHI=INT(10*PHI+0.5)
            IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=65
            POS_NEG_ETA2(IETA,IPHI)=ETA
            POS_NEG_PHI2(IETA,IPHI)=PHI
            POS_NEG(IETA,IPHI)=POS_NEG(IETA,IPHI)+ET
            CELL_COUNTER(IETA,IPHI)=CELL_COUNTER(IETA,IPHI)+1
            IF((ILYR.GE.MNLYEM).AND.(ILYR.LE.MXLYEM))  THEN
              EM(IETA,IPHI)=EM(IETA,IPHI)+ET
              CELL_EM(IETA,IPHI)=CELL_EM(IETA,IPHI)+1
            ENDIF
            IF((ILYR.GE.MNLYMG).AND.(ILYR.LE.MXLYMG)) THEN
              ICD(IETA,IPHI)=ICD(IETA,IPHI)+ET
              CELL_ICD(IETA,IPHI)=CELL_ICD(IETA,IPHI)+1
            ENDIF
            IF((ILYR.GE.MNLYFH).AND.(ILYR.LE.MXLYFH)) THEN
              FH(IETA,IPHI)=FH(IETA,IPHI)+ET
              CELL_FH(IETA,IPHI)=CELL_FH(IETA,IPHI)+1
            ENDIF
          ENDIF
   31   ENDDO

c
c  If we didn't read any cells because we bombed on the first attempt,
c  return now!  Zero out the error guy if all is well!
c

      if (start .and. ier .ne. 0) then
        return
      endif
      ier = 0

      ENDIF

      IF(input_type.EQ.1) THEN ! isaq banks
        lisaq=0
  201   CALL gtisaq(lisaq,lisaq,id,isaq_e,isaq_phi,isaq_th,isaq_eta)
        IF(lisaq.GT.0) THEN
          IF(USE_DET_ETA) isaq_ETA=PETA_TO_DETA(isaq_ETA,Z)
          IETA=INT(10*ABS(isaq_ETA)+0.5)
          IF(isaq_ETA.LT.0) IETA=-IETA
          IF(ABS(IETA).GT.45) IETA=46
          IPHI=INT(10*isaq_PHI+0.5)
          IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=65
          POS_NEG_ETA2(IETA,IPHI)=isaq_ETA
          POS_NEG_PHI2(IETA,IPHI)=isaq_PHI
          POS_NEG(IETA,IPHI)=POS_NEG(IETA,IPHI)
     &          +sqrt(isaq_e(1)**2+isaq_e(2)**2)
          GOTO 201
        ENDIF
      ENDIF

      IF(input_type.EQ.2) THEN  ! run on particles
        LISV1 = 0
  101   CALL gtisv1(lisv1,lisv1,idv,ISP1_pp,xV1,yV1,zV1)
        IF(lisv1.GT.0)THEN
          lisp1=lisv1-izisp1
  301     CALL gtisp1(lisp1,lisp1,id,isp1_e,isp1_phi,isp1_th,isp1_eta)
          IF(lisp1.GT.0) THEN
            IF(USE_DET_ETA) isp1_eta=PETA_TO_DETA(isp1_ETA,Z)
            IETA=INT(10*ABS(isp1_ETA)+0.5)
            IF(isp1_ETA.LT.0) IETA=-IETA
            IF(ABS(IETA).GT.45) IETA=46
            IPHI=INT(10*isp1_PHI+0.5)
            IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=65
            POS_NEG_ETA2(IETA,IPHI)=isp1_ETA
            POS_NEG_PHI2(IETA,IPHI)=isp1_PHI
            POS_NEG(IETA,IPHI)=POS_NEG(IETA,IPHI)
     &          +sqrt(isp1_e(1)**2+isp1_e(2)**2)
            GOTO 301
          ENDIF
          GOTO 101
        ENDIF
      ENDIF

      IF(input_type.EQ.4) THEN ! catd

        LCATD = GZCATD()
        IF ( LCATD .GT. 0 ) THEN
          DO II = 1,2
            OFF = 8
            ISEC = 1
            N   = IQ(LCATD + 8 )
            IF ( II .EQ. 2 ) THEN
              OFF = N + 9
              ISEC = 2
              N = IQ(LCATD + N + 9 )
            ENDIF
            DO I = 1, N
              ICATD = LCATD + OFF + I
              CALL UPCATD(ICATD,ISEC,catd_IETA,catd_IPHI,catd_ETA,
     &            catd_PHI,DELETA,catd_E)
              IF(USE_DET_ETA) catd_eta=PETA_TO_DETA(catd_ETA,Z)
              IETA=INT(10*ABS(catd_ETA)+0.5)
              IF(catd_ETA.LT.0) IETA=-IETA
              IF(ABS(IETA).GT.45) IETA=46
              IPHI=INT(10*catd_PHI+0.5)
              IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=65
              POS_NEG_ETA2(IETA,IPHI)=catd_ETA
              POS_NEG_PHI2(IETA,IPHI)=catd_PHI
              POS_NEG(IETA,IPHI)=POS_NEG(IETA,IPHI)
     &            +sqrt(catd_e(1)**2+catd_e(2)**2)
            ENDDO
          ENDDO
        ENDIF
      ENDIF

C.. AFTER CELLS PROJECTED INTO TOWERS, DETERMINE NUMBER OF TOWERS

      IF(ETA_ORDERED) THEN
        cells=0
        DO j=0,63
          DO i=-45,45
            IF(pos_neg(i,j).ne.0) then
              cells=cells+1
              IF(cells.LT.17000) pos_neg_eta(cells)=pos_neg_eta2(i,j)
              IF(cells.LT.17000) pos_neg_phi(cells)=pos_neg_phi2(i,j)
            ENDIF
          ENDDO
        ENDDO
        IF(cells.GT.17000) THEN
          cells=17000
          CALL errmsg('too many cells','kt_read_caeh',' ','W')
        ENDIF
      ENDIF

      IF(ET_ORDERED) THEN
        CELLS=0
        DO J=0,63
          DO I=-45,45
            IF(POS_NEG(I,J).NE.0) THEN
              CELLS=CELLS+1
              IF(cells.LT.17000) sort_et(cells)=pos_neg(i,j)
              IF(cells.LT.17000) pos_neg_eta(cells)=pos_neg_eta2(i,j)
              IF(cells.LT.17000) pos_neg_phi(cells)=pos_neg_phi2(i,j)
            ENDIF
          ENDDO
        ENDDO
        IF(cells.GT.17000) THEN
          cells=17000
          CALL errmsg('too many cells','kt_read_caeh',' ','W')
        ENDIF

        DO I=1,CELLS
          DO J=1+I,CELLS
            IF(SORT_ET(I).LT.SORT_ET(J)) THEN
              DUMMY=SORT_ET(I)
              SORT_ET(I)=SORT_ET(J)
              SORT_ET(J)=DUMMY
              DUMMY=POS_NEG_ETA(I)
              POS_NEG_ETA(I)=POS_NEG_ETA(J)
              POS_NEG_ETA(J)=DUMMY
              DUMMY=POS_NEG_PHI(I)
              POS_NEG_PHI(I)=POS_NEG_PHI(J)
              POS_NEG_PHI(J)=DUMMY
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      if(DO_PRECLUSTER) THEN

c.. precluster towers so that no two towers are closer than prec_width apart

      DO i=1,cells
        ieta=INT(10*ABS(pos_neg_ETA(i))+0.5)
        IF(pos_neg_ETA(i).LT.0) IETA=-IETA
        IF(ABS(IETA).GT.45) IETA=999
        iphi=INT(10*pos_neg_phi(i)+0.5)
        IF((iphi.LT.0).OR.(iphi.GT.63)) iphi=999
        IF((abs(ieta).LE.45).AND.(iphi.GE.0).AND.(iphi.LE.63)) THEN
   20     IF((pos_neg(ieta,iphi).ne.0).AND.(POS_NEG(IETA,IPHI).LT.
     &             MIN_PREC_ET)) THEN
            DRMIN=PREC_WIDTH
            DO j=ieta-3,ieta+3
              DO k=iphi-4,iphi+4
                IF((K.GE.0).AND.(K.LE.63)) NEWK=K
                IF(K.LT.0) NEWK=64+K
                IF(K.GT.63) NEWK=K-64
                IF((abs(j).LE.45).AND.(NEWk.GE.0).AND.(NEWk.LE.63)) THEN
                  IF(pos_neg(j,NEWk).ne.0) then
                    deta=pos_neg_eta2(ieta,iphi)-pos_neg_eta2(j,NEWk)
                    dphi=abs(pos_neg_phi2(ieta,iphi)-
     %                  pos_neg_phi2(j,NEWk))
                    IF(dphi.GT.3.1415926) dphi=6.283185-dphi
                    dr=sqrt(deta**2+dphi**2)
                    IF((dr.LE.DRMIN).AND.(dr.GT.0.0001)) THEN
                      DRMIN=DR
                      PREC_ETA=J
                      PREC_PHI=NEWK
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
            IF(DRMIN.LT.PREC_WIDTH) THEN
              etnew=POS_NEG(IETA,IPHI)+POS_NEG(PREC_ETA,PREC_PHI)
              EMNEW=EM(IETA,IPHI)+EM(PREC_ETA,PREC_PHI)
              ICDNEW=ICD(IETA,IPHI)+ICD(PREC_ETA,PREC_PHI)
              FHNEW=FH(IETA,IPHI)+FH(PREC_ETA,PREC_PHI)
              CELLNEW=CELL_COUNTER(IETA,IPHI)+
     &                    CELL_COUNTER(PREC_ETA,PREC_PHI)
              CELLNEW_EM=CELL_EM(IETA,IPHI)+
     &                    CELL_EM(PREC_ETA,PREC_PHI)
              CELLNEW_ICD=CELL_ICD(IETA,IPHI)+
     &                    CELL_ICD(PREC_ETA,PREC_PHI)
              CELLNEW_FH=CELL_FH(IETA,IPHI)+
     &                    CELL_FH(PREC_ETA,PREC_PHI)
            ETANEW=(POS_NEG_ETA2(IETA,IPHI)*abs(POS_NEG(IETA,IPHI))
     &        +POS_NEG_ETA2(PREC_ETA,PREC_PHI)*
     &        abs(POS_NEG(PREC_ETA,PREC_PHI)))/
     &       (abs(POS_NEG(IETA,IPHI))+abs(POS_NEG(PREC_ETA,PREC_PHI)))
           PHINEW=POS_NEG_PHI2(IETA,IPHI)+
     &       delta_phi(pos_neg_phi2(ieta,iphi),
     &       pos_neg_phi2(prec_eta,prec_phi))*
     &          abs(pos_neg(prec_eta,prec_phi))/
     &     (abs(POS_NEG(IETA,IPHI))+abs(POS_NEG(PREC_ETA,PREC_PHI)))
              IF(phinew.GT.6.283) phinew=phinew-6.283
              IF(phinew.LT.0) phinew=phinew+6.283
              pos_neg(ieta,iphi)=0
              pos_neg(prec_eta,prec_phi)=0
              EM(IETA,IPHI)=0
              EM(PREC_ETA,PREC_PHI)=0
              ICD(IETA,IPHI)=0
              ICD(PREC_ETA,PREC_PHI)=0
              FH(IETA,IPHI)=0
              FH(PREC_ETA,PREC_PHI)=0
              CELL_COUNTER(PREC_ETA,PREC_PHI)=0
              CELL_COUNTER(IETA,IPHI)=0
              CELL_EM(PREC_ETA,PREC_PHI)=0
              CELL_EM(IETA,IPHI)=0
              CELL_ICD(PREC_ETA,PREC_PHI)=0
              CELL_ICD(IETA,IPHI)=0
              CELL_FH(PREC_ETA,PREC_PHI)=0
              CELL_FH(IETA,IPHI)=0
              ietanew=int(10*ABS(etanew)+0.5)
              IF(ETANEW.LT.0) IETANEW=-IETANEW
              IF(ABS(IETANEW).GT.45) IETANEW=46
              iphinew=int(10*phinew+0.5)
              IF((iphinew.LT.0).OR.(iphinew.GT.63)) iphinew=65
              pos_neg(ietanew,iphinew)=pos_neg(ietanew,iphinew)
     &                    +etnew
              EM(IETANEW,IPHINEW)= EM(IETANEW,IPHINEW)+EMNEW
              ICD(IETANEW,IPHINEW)=ICD(IETANEW,IPHINEW)+ICDNEW
              FH(IETANEW,IPHINEW)=FH(IETANEW,IPHINEW)+FHNEW
              CELL_COUNTER(IETANEW,IPHINEW)=
     &                    CELL_COUNTER(IETANEW,IPHINEW)+CELLNEW
              CELL_EM(IETANEW,IPHINEW)=
     &                    CELL_EM(IETANEW,IPHINEW)+CELLNEW_EM
              CELL_ICD(IETANEW,IPHINEW)=
     &                    CELL_ICD(IETANEW,IPHINEW)+CELLNEW_ICD
              CELL_FH(IETANEW,IPHINEW)=
     &                    CELL_FH(IETANEW,IPHINEW)+CELLNEW_FH
              pos_neg_eta2(iETANEW,IPHINEW)=etanew
              pos_neg_phi2(iETANEW,IPHINEW)=phinew
              ieta=ietanew
              iphi=iphinew
              GOTO 20
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      ENDIF

      DO I=-45,45
        DO J=0,63
          IF(POS_NEG(I,J).GT.0) THEN
            POS_CELLS=POS_CELLS+1
            IF(pos_cells.LT.15000)POS_ETA(POS_CELLS)=POS_NEG_ETA2(I,J)
            IF(pos_cells.LT.15000)POS_PHI(POS_CELLS)=POS_NEG_PHI2(I,J)
            POS_ETA2(I,J)=POS_NEG_ETA2(I,J)
            POS_PHI2(I,J)=POS_NEG_PHI2(I,J)
            positive(i,j)=pos_neg(i,j)
          ENDIF
          IF(POS_NEG(I,J).LT.0) THEN
            NEG_CELLS=NEG_CELLS+1
            IF(neg_cells.LT.2000) NEG_ETA(NEG_CELLS)=POS_NEG_ETA2(I,J)
            IF(neg_cells.LT.2000) NEG_PHI(NEG_CELLS)=POS_NEG_PHI2(I,J)
            negative(i,j)=pos_neg(i,j)
          ENDIF
        ENDDO
      ENDDO
      
      RETURN
      END
