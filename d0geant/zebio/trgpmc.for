      SUBROUTINE TRGPMC (ITRG,ISTACK,IW,PED,UR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get one pedestal or Uranium typical event
C-                         for layer ISTACK and wire IW
C-
C-
C-   Returned value  :
C-   Inputs  :
C-             IW:   WIRE OR STRIP NUMBER
C-             ISTACK: ISTACK=1,2,3 FOR WIRES ISTACK=4,5,6 FOR STRIPS
C-   Outputs :
C-             PED(NMFADC): PEDESTAL OR URANIUM BACKGROUND EVENT (FADC)
C-             UR: = .TRUE. IF AN URANIUM EVENT IS CHOSEN
C-
C-   Controls:
C-            ITRG: (flag) ITRG=0 CHOOSE HIT WIRES AND STRIPS FOR X RAY
C-                                URANIUM (CALLED ONE TIME PER EVENT)
C-                         ITRG=1 CHOOSE ONE PEDESTAL OR URANIUM EVENT
C-                                FOR WIRE IW
C-                         ITRG=2 CHOOSE ONE PEDESTAL OR URANIUM EVENT
C-                                FOR STRIP IW
C-
C-   Created  21-DEC-1992   J.P. Cussonneau
C-   Updated   4-May-1996   sss - Fix phiw indexing error
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C --
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:PEDTRD.INC'
      INCLUDE 'D0$INC:NORTRD.INC'
C --
      INTEGER NEVEPED
      PARAMETER (NEVEPED=512)  ! nb of pedestal events
      INTEGER NEVEUR
      PARAMETER (NEVEUR=512)   ! nb of uranium events
      INTEGER NBIN1
      PARAMETER (NBIN1=20)   ! nb of bins for histogram of number
C                              of hit Ur wires
      INTEGER NBIN2
      PARAMETER (NBIN2=30)
C --
      LOGICAL FIRST,UR
C --
      INTEGER ISTACK,IEVEPED,I,J,IUNIT,IER,ISTRIP(256)
      INTEGER IEVEUR,IEVESAV(3,256)
      REAL PHIW(512,3)
      INTEGER ITRG,IW,K,K1,K2,IWUR(6,256)
      INTEGER KWR,KWRB,EVEUNP(NMFADC),EVEPAC1(32),EVEPAC2(64)
      INTEGER MPACK1(2),MPACK2(2),IFOIS
C --
      REAL ORIGI,FACTI
      REAL PEDEVE(NEVEPED,NMFADC),UREVE(NEVEUR,NMFADC),PED(NMFADC)
      REAL RM,RNDM,EMIN,ELEN,URGRO1(NBIN1),URGRO2(NBIN1)
      REAL URGRO3(NBIN1),ATTENC
      REAL URANO1(NBIN2),URANO2(NBIN2),URANO3(NBIN2)
      REAL XLO1,XWID1,FK1,TWOPI,Z,PHICL,PHICAT,DPHIST
      REAL XLO2,XWID2,FK2
      REAL NORML,SUR_REF
C --
      DATA FIRST/.TRUE./
      DATA XLO1,XWID1/0.,1./
      DATA XLO2,XWID2/0.,1./
      DATA ORIGI,FACTI/10.,10./
      DATA MPACK1/8,4/
      DATA MPACK2/12,2/
      DATA TWOPI/6.28318/
C      DATA SIGPED/1.00,1.00,1.67,1.46,1.46,1.83/   ! In agreement with
C                                                   threshold of run 58064
      DATA ATTENC/0.5/    ! ATTENUATION FACTOR FOR STRIPS
      DATA SUR_REF/1000./
C -- URGRO1 URGRO2 URGRO3 : number of groups of wires from Uranium data
      DATA URGRO1/
     &6196.0,4125.0,1915.0,697.0,244.0,63.0,27.0,10.0,
     &4.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0/
      DATA URGRO2/
     &255.0,4376.0,2612.0,1318.0,484.0,177.0,46.0,10.0,
     &1.0,2.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0/
      DATA URGRO3/
     &3297.0,4165.0,2987.0,1614.0,774.0,295.0,108.0,30.0,
     &7.0,1.0,2.0,0.0,0.0,0.0,0.0,0.0,
     &1.0,0.0,0.0,0.0/
C -- URANO1 URANO2 URANO3 : number of wires per group from Uranium data
      DATA URANO1/
     &0.0,6500.0,3671.0,907.0,324.0,113.0,47.0,24.0,
     &8.0,5.0,1.0,0.0,0.0,1.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0/
      DATA URANO2/
     &0.0,9524.0,5230.0,1192.0,432.0,167.0,95.0,49.0,
     &21.0,19.0,7.0,4.0,1.0,4.0,1.0,0.0,
     &0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0/
      DATA URANO3/
     &0.0,14364.0,4879.0,914.0,233.0,65.0,35.0,11.0,
     &5.0,2.0,3.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0/
C --
      IF (FIRST) THEN
        FIRST = .FALSE.
        SIGPED(1) = 1.00
        SIGPED(2) = 1.00
        SIGPED(3) = 1.67
        SIGPED(4) = 1.46
        SIGPED(5) = 1.46
        SIGPED(6) = 1.83
C -- Read pedestal data file
        CALL GTUNIT(70,IUNIT,IER)
C&IF IBMAIX
C&        OPEN(FILE='NOSUB0',UNIT=IUNIT,STATUS='OLD')
C&ELSE
        OPEN(FILE='NOSUB0',UNIT=IUNIT,STATUS='OLD',READONLY)
C&ENDIF
        DO I = 1,NEVEPED
          READ (IUNIT,101) (EVEPAC1(J),J=1,32)
          CALL UPKBYT(EVEPAC1,1,EVEUNP,NMFADC,MPACK1)
          DO J = 1,NMFADC
            PEDEVE(I,J) = FLOAT(EVEUNP(J))/FACTI-ORIGI
          ENDDO
        ENDDO
        CLOSE (IUNIT)
        CALL RLUNIT(70,IUNIT,IER)
C -- Read uranium data file
        CALL GTUNIT(70,IUNIT,IER)
C&IF IBMAIX
C&        OPEN(FILE='URL1L2',UNIT=IUNIT,STATUS='OLD')
C&ELSE
        OPEN(FILE='URL1L2',UNIT=IUNIT,STATUS='OLD',READONLY)
C&ENDIF
        DO I = 1,NEVEUR
          READ (IUNIT,102) (EVEPAC2(J),J=1,64)
          CALL UPKBYT(EVEPAC2,1,EVEUNP,NMFADC,MPACK2)
          DO J = 1,NMFADC
            IF (I.LE.256) THEN ! LAYER 1
              NORML = SUR_REF/SURCAL(1)
            ELSE
              NORML = SUR_REF/SURCAL(2)
            ENDIF
            UREVE(I,J) = (FLOAT(EVEUNP(J))/FACTI-ORIGI)*NORML
          ENDDO
        ENDDO
        CLOSE (IUNIT)
        CALL RLUNIT(70,IUNIT,IER)
C
C -- URGROI & URANOI  I=1,2,3: Uranium ratio
C
        CALL HISPRE(URGRO1,NBIN1)
        CALL HISPRE(URGRO2,NBIN1)
        CALL HISPRE(URGRO3,NBIN1)
        CALL HISPRE(URANO1,NBIN2)
        CALL HISPRE(URANO2,NBIN2)
        CALL HISPRE(URANO3,NBIN2)
C --
        DO 31 I =  1,3
C  DEFINE PHI FOR EACH WIRE IN THE TRD REFERENCE FRAME
          DO 41 K =  1,NWIRE(I)
            PHIW(K,I)=(K-.5)*DPHIAN(I)
   41     CONTINUE
   31   CONTINUE
      ENDIF
C
C -- CHOOSE HIT ANODES & HIT CATHODES IF ITRG=0
C
      IF(ITRG.EQ.0) THEN
        CALL VZERO(IWUR,1536)
        DO I = 1,3       ! loop on layer
          IF(I.EQ.1)
     &       CALL HISRAN(URGRO1,NBIN1,XLO1,XWID1,FK1)
          IF(I.EQ.2)
     &       CALL HISRAN(URGRO2,NBIN1,XLO1,XWID1,FK1)
          IF(I.EQ.3)
     &       CALL HISRAN(URGRO3,NBIN1,XLO1,XWID1,FK1)
          K1 = INT(FK1) ! nb of groups
          IF (K1.GT.0) THEN
            DO K =1,K1      ! loop on groups
              IF (I.EQ.1)
     &          CALL HISRAN(URANO1,NBIN2,XLO2,XWID2,FK2)
              IF (I.EQ.2)
     &          CALL HISRAN(URANO2,NBIN2,XLO2,XWID2,FK2)
              IF (I.EQ.3)
     &          CALL HISRAN(URANO3,NBIN2,XLO2,XWID2,FK2)
              K2 = INT(FK2) ! nb of anodes per group
C --
              DO J = 1,K2  ! loop on anodes per group
                IF (J.EQ.1) THEN
                  IFOIS = 0
   99             IFOIS = IFOIS+1
                  KWR = INT(255.*RNDM()+1.)
                  IF (IWUR(I,KWR).EQ.1.AND.IFOIS.LE.5) GO TO 99
                ELSE
                  KWR = KWR+1
                  IF(KWR.GT.256) KWR = KWR-256
                ENDIF
                IWUR(I,KWR) = 1           ! HIT WIRE IS NUMBER KWR
C -- Search hit cathode number associated with hit wire number KWR
                KWRB = KWR
                IF (I.EQ.3) KWRB = 2*KWR-1                 ! 512 wires for
                IF (I.EQ.3.AND.RNDM().LT.0.5) KWRB = 2*KWR ! LAYER 3
                PHICL = PHIW(KWRB, I)
     &            +(1.-2.*RNDM())*TWOPI/(2.*FLOAT(NWIRE(I)))
                Z = 83.*(1.-2.*RNDM())
                PHICAT = PHICL - OFSDPH(I) - DPHIDZ(I)*Z + TWOPI
                DPHIST = AMOD(PHICAT-OFSCAT(I),TWOPI)
                ISTRIP(KWR) = DPHIST/DPHICA(I) + 1.0
                ISTRIP(KWR) = MAX0(ISTRIP(KWR),1)
                ISTRIP(KWR) = MIN0(ISTRIP(KWR),NSTRIP(I))
                IWUR(I+3,ISTRIP(KWR)) = 1           ! HIT STRIP IS NUMBER ISTRIP
              ENDDO
            ENDDO
          ENDIF
        ENDDO
        GO TO 999
      ENDIF
C
C -- Uranium for wires (ISTACK = 1,2,3)
C
      IF(ITRG.EQ.1) THEN
        UR = .FALSE.
        IF (IWUR(ISTACK,IW).EQ.1) THEN
          UR = .TRUE.
          RM = RNDM()
          IEVEUR = INT(FLOAT(NEVEUR-1)*RM)+1
          IEVESAV(ISTACK,ISTRIP(IW)) = IEVEUR
C --
          IF ( PTRD.GE.4 ) THEN
            IF (IEVEUR.LT.1.OR.
     &         IEVEUR.GT.NEVEUR) THEN
              WRITE(LOUT,*) ' PROBLEM_TRD TRGPMC,IEVEUR =',IEVEUR
            ENDIF
          ENDIF
          DO J = 1,NMFADC
            PED(J) = UREVE(IEVEUR,J)*SURCAL(ISTACK)/SUR_REF
          ENDDO
        ELSE
C
C -- Pedestal for wires (ISTACK = 1,2,3)
C
          RM = RNDM()
          IEVEPED = INT(FLOAT(NEVEPED-1)*RM)+1
C --
          IF (PTRD.GE.4) THEN
            IF (IEVEPED.LT.1.OR.IEVEPED.GT. NEVEPED) THEN
              WRITE(LOUT,*) ' PROBLEM_TRD TRGPMC, PEDESTAL'
c             WRITE(LOUT,*) ' EMIN',EMIN,' ELEN =',ELEN
              WRITE(LOUT,*) ' IEVEPED =',IEVEPED,' ISTACK =',ISTACK
            ENDIF
          ENDIF
          DO J = 1,NMFADC
            PED(J) = PEDEVE(IEVEPED,J)*SIGPED(ISTACK)/SIGPED(1)
          ENDDO
        ENDIF
      ENDIF
C
C -- Uranium for strips (ISTACK = 4,5,6)
C
      IF(ITRG.EQ.2) THEN
        UR = .FALSE.
        IF (IWUR(ISTACK,IW).EQ.1) THEN
          UR = .TRUE.
          IEVEUR = IEVESAV(ISTACK-3,IW)
C --
          IF ( PTRD.GE.4 ) THEN
            IF (IEVEUR.LT.1.OR.
     &         IEVEUR.GT.NEVEUR) THEN
              WRITE(LOUT,*) ' PROBLEM_TRD TRGPMC,IEVEUR =',IEVEUR
            ENDIF
          ENDIF
          DO J = 1,NMFADC
            PED(J) = UREVE(IEVEUR,J)*(SURCAL(ISTACK-3)/SUR_REF)*ATTENC
          ENDDO
        ELSE
C
C -- Pedestal for strips (ISTACK = 4,5,6)
C
          RM = RNDM()
          IEVEPED = INT(FLOAT(NEVEPED-1)*RM)+1
C --
          IF (PTRD.GE.4) THEN
            IF (IEVEPED.LT.1.OR.IEVEPED.GT. NEVEPED) THEN
              WRITE(LOUT,*) ' PROBLEM_TRD TRGPMC, PEDESTAL'
c              WRITE(LOUT,*) ' EMIN',EMIN,' ELEN =',ELEN
              WRITE(LOUT,*) ' IEVEPED =',IEVEPED,' ISTACK =',ISTACK
            ENDIF
          ENDIF
          DO J = 1,NMFADC
            PED(J) = PEDEVE(IEVEPED,J)*SIGPED(ISTACK)/SIGPED(1)
          ENDDO
        ENDIF
      ENDIF
C --
  100 FORMAT (16(/,6X,8(1X,F5.1)))
  101 FORMAT (4(/,7I11),/,4I11)
  102 FORMAT (9(/,7I11),/,I11)
C----------------------------------------------------------------------
  999 RETURN
      END
