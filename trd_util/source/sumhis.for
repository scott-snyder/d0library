      SUBROUTINE SUMHIS(ID,IPR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pour un histo_hbook (ID) ,
C-  calcul par canal le pourcentage(PERCEN)  du contenu des canaux superieurs
C-  au low-edge   rapporte au nombre d'entrees (les debords sont inclus) .
C-
C-   Inputs  : ID: ID(hbook) de l'histo source .
C-             Si ipr=2 , a la sortie ID contiendra PERCEN
C-        abs(IPR): 0 -   seul PERCEN est IMPRIME (horizontalement).
C-                  1 -  HPRINT(ID) .
C-                  2 -  HPRINT(ID) avec PERCEN sous les canaux
C-                  3 -  HPRINT(ID) (source) et HPRINT(ID) (percen) ,
C-                       ID est remis a 0 apres la 1ere impression
C-
C-        IPR le 0: PERCEN est en outre imprime (horiz)
C-   Outputs : Dans le commun /CSUMHI/
C-             PERCEN, array de dimension le 120, contiendra les pourcentages.
C-             L' output se fait sur l'unite LOUT definie ici en data .
C-   Controls:
C-
C-   Created  27-SEP-1989   J.F. DETOEUF ESQ.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PAWC.INC'
      INTEGER ID,IPR,JPR  ,I,J,N3 ,LOUT
      INTEGER NX,NY,NWT,IAD ,IEXP   ,LTIT,NOENT,TRUNIT
      REAL    XMI,XMA,YMI,YMA, SUM,SUMH(200), CW,TOT,X
      REAL AMAX0,HI
      CHARACTER TIT0*80,TIT*80,LINS(3)*(132),S4*(4)
      LOGICAL HEXIST
      REAL PERCEN(120)
C      COMMON/CSUBHI/PERCEN
C
      DATA LOUT / 6 /
C----------------------------------------------------------------------
      LOUT=TRUNIT()
      IF(.NOT. HEXIST(ID))THEN
        WRITE(LOUT,*)' HISTOGRAM NB.',ID,' DOES NOT EXIST'
        RETURN
      END IF
      CALL HNOENT(ID,NOENT)
      IF(NOENT.LE.0)THEN
        WRITE(LOUT,*)' HISTOGRAM NB.',ID,' IS EMPTY'
        RETURN
      END IF
      S4=' '
      JPR=IABS(IPR)
      IF (JPR.GE.1)  CALL HPRINT(ID)
      IF(IPR.EQ.1)THEN
        CALL HDELET(ID)
        RETURN
      END IF
      CALL HGIVE(ID,TIT0,NX,XMI,XMA,NY,YMI,YMA,NWT,IAD)
      CW=(XMA-XMI)/NX
      IF(IPR.LT.0)THEN  !Print histo content on user's output file
        WRITE(LOUT,*)XMI,NX,CW
        WRITE(LOUT,*)'TITRE'
        WRITE(LOUT,1003)ID, TIT0
 1003   FORMAT('1 HISTO ID',I6,12X,A80)
        WRITE(LOUT,'(10F7.1)')(HI(ID,I),I=1,NX)
        WRITE(LOUT,*)'FIN'
        IF(JPR.EQ.1)THEN
          CALL HDELET(ID)
          RETURN
        ENDIF
      ENDIF
      DO I =  1, 3
        LINS(I)=' '
      ENDDO
      TIT=TIT0
      SUM= 0
      TOT=0
      DO  I=1,NX
        SUM=SUM+HI(ID,I-1)
        SUMH(I)=SUM
      ENDDO
      TOT= SUM+HI(ID,NX)+HI(ID,NX+1)
      IF (JPR.EQ.3 ) THEN
        TIT(74:80)='PERCENT'
        CALL HRESET(ID,TIT)
      ENDIF
C
      DO I=1,NX
        PERCEN(I)= (1.-SUMH(I)/TOT)*100.
C--       IF(MOD(I,5).EQ.1) PRINT*,'!!- I,SUM,PERCEN: ',I,SUMH(I),PERCEN(I)
        X=XMI+(I-1)*CW
        IF (JPR.EQ.3)  CALL HF1(ID,X,PERCEN(I))
        IF (JPR.EQ.2)THEN
          N3= 10*PERCEN(I)
          WRITE(S4,'(I4)')N3
          DO J = 1 ,3
            LINS(J)(16+I:16+I)=S4(J+1:J+1)
          ENDDO
        ENDIF
      ENDDO
      IF( JPR.EQ.2) THEN
        LINS(1)(:14) = ' PERCENT   10 '
        LINS(2)(:14) = '            1.'
        WRITE (LOUT,'((A132))') LINS
        WRITE(LOUT,*) '-TOTAL debords inclus:',TOT
      ENDIF
      IF (JPR.EQ.3) THEN
        CALL HPRINT(ID)
      ENDIF
      IF (IPR.LE.0) THEN
        WRITE(LOUT,'(A1,A10,A80/)') '0',' PERCENT:   ', TIT,' '
        WRITE(LOUT,'((10(I3,F5.1)))') (I,PERCEN(I),I=1,NX)
      ENDIF
  999 RETURN
      END
