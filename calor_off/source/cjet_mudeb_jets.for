      SUBROUTINE CJET_MUDEB_JETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-JAN-1993   Alex Smith
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
C
      INTEGER PRUNIT,LLJETS,NJETS,IER,I,LPMUO,GZPMUO
      INTEGER K,GZJETS,LCAPH,GZCAPH,RUN,EVENT
      REAL    TEMPLATE(20)
      LOGICAL FIRST,EZERR
      CHARACTER*10  HEADER(12)
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA PRUNIT/30/
      DATA HEADER / '  N','    Px','    Py','    Pz','     P'
     &  ,'    Pt',' Theta ','  Phi','  Eta ',' Emip',' sigP',' IFW4'/
C----------------------------------------------------------------------
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CAJETS','CJET_MUDEB_JETS',
     &      'CAJETS RCP bank not found in DEB_JET.','W')
        ELSE
          CALL EZGET('MUJET_DEBUG_LUNIT',PRUNIT,IER)
          CALL EZRSET
        ENDIF
      ENDIF
      CALL EVNTID(RUN,EVENT)
      WRITE(PRUNIT,*)'*******************************
     &**********************'
      WRITE(PRUNIT,*)'RUN = ',RUN, ' EVENT = ', EVENT
      WRITE(PRUNIT,*)'******************************
     &***********************'
      LPMUO = GZPMUO(0)
      I=1
      DO WHILE (LPMUO.GT.0)
        WRITE(PRUNIT,*) ' MUON PMUO DATA:'
        WRITE(PRUNIT,*) ' ---------------'
        WRITE(PRUNIT,33) (HEADER(K),K=1,12)
   33   FORMAT(A3,5A8,   5A6  , A5)
        WRITE(PRUNIT,32) I,Q(LPMUO+10),Q(LPMUO+11),
     &    Q(LPMUO+12),Q(LPMUO+13),Q(LPMUO+14),Q(LPMUO+15),
     &    Q(LPMUO+17),Q(LPMUO+16),Q(LPMUO+33),Q(LPMUO+21)
     &    ,IQ(LPMUO+9)
   32   FORMAT(I3,5F8.2, 5F6.2, I5)
C
C *** Print only JETS bank that is associated with this muon:
C
        LLJETS = LQ(LPMUO-IQ(LPMUO-2)-5)
        IF (LLJETS .NE. 0) THEN
          WRITE(PRUNIT,*) ' ASSOCIATED JET FROM MUON ALGORITHM:'
          WRITE(PRUNIT,*) ' -----------------------------------'
          CALL CJET_MUPRJETS(PRUNIT,LLJETS,NJETS,'ONE',2)
        END IF
        LPMUO = LQ(LPMUO)
        I = I + 1
      ENDDO
C
C *** Print JETS from CONE algorithm:
C
      TEMPLATE(1)=1
      TEMPLATE(2)=6
      TEMPLATE(3)=0.7
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      LCAPH = GZCAPH( )
      IF ( IER.NE.0 ) GOTO 999
      LLJETS = GZJETS()
      IF (LLJETS .GT. 0) THEN
        WRITE(PRUNIT,*) ' JETS FROM CONE ALGORITHM:'
        WRITE(PRUNIT,*) ' -------------------------'
        CALL CJET_MUPRJETS(PRUNIT,LLJETS,NJETS,'LINEAR',2)
      END IF
  999 RETURN
      END
