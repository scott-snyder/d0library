      LOGICAL FUNCTION STPTRD()
C ----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Compute the total range in the T.E.C.
C-                         Call ENTRD when leaving the TEC volume
C                          Called when IHSET = TRD from GUSTEP
C-                         ZEBRA banks 'GTRH'  and 'GTLY' are filled
C-
C-   INPUTS  :GEANT BANK GCTRAK
C-            INWVOL=1 ENTERING VOLUME =2 LEAVING VOLUME  =0 STEPPING
C-            PTRD=TRD PRINT FLAG
C-                =0  NO PRINT
C-                =1
C-                =2 DIGITIZATION INFORMATION: DECODED ADRESS OF HIT
C-                                                CHANNNEL
C-                =3 dQ/dT FOR 50 FADC CHANNELS
C-                =4
C-                =5
C-                =6
C-                =7
C-                =8   TRACKS INFORMATION FOR 5 FIRST TRACKS
C-                =9   GLOBAL CLUSTER INFORMATIONS FOR 5 FIRST TRACKS
C-                =10  FULL CLUSTERS FOR 5 FIRST TRACKS
C-                +n*100 TO PRINT THE FIRST n TRACKS
C-
C-   OUTPUTS :
C-
C-   Created   18 JUN  1986  SLL, AZ
C-   Rewritten 21-DEC-1987   A. ZYLBERSTEJN
C-   Updated  10-FEB-1988   A. ZYLBERSTEJN   call ENTRD after every step
C-   Updated  29-SEP-1988   A. ZYLBERSTEJN   STEP TROUGH VOLUME
C-   Updated  10-JUL-1989   A. Zylberstejn  Cleaning
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into pbd interface function. Added common GCSETS.
C-   Updated  20-JUL-1989   A. Zylberstejn  Include Saclay corrections
C-   Updated  22-APR-1991   A. Zylberstejn  : Suppress the definition of 
C-                                            TRHIST(done in INITRD)
C-   Updated  18-JUN-1993   J.P. Cussonneau : Remove hfill (done in INITRD)   
C-                                            
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:TRDVOL.INC'
C      INCLUDE 'D0$INC:GTRHLN.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      LOGICAL FIRST,FIRST1
      REAL R1,R2,RM,TGEND,TGENX,X1,Y1,Z1,RXEN,SM,VIN(6),VCYL(6)
      INTEGER IERR,NDEBM
      INTEGER I,IFOIS,ISM,IZOFS,IZSTAR,NLOOP
      INTEGER ISAVER,ISATRA,ITRAI
      INTEGER INDEX(5),J,PTRDI,NEVI
      INTEGER LGTLY,NUMTR,IRUN0,TRD
      REAL LENGTH,MINLEN,VECTIN(7),VERTIN(3),VBEFOR(7)
      PARAMETER (ISM=4)
      REAL TIM0,TIM,DT
      COMMON/GCNUM/IBID(10),NMBIT
C
      INTEGER   IBID,NMBIT
      INTEGER NRANDO,NBIT2,JBYT,NRANDI,NHASAR(2)
      DATA NRANDI/0/
      DATA LENGTH/0./,IFOIS/0/,MINLEN/.2/
      DATA NLOOP/0/,ITRAI/0/,NDEBM/5/
      DATA TIM/0./,NEVI/0/,NUMTR/0/
      DATA FIRST/.TRUE./,FIRST1/.TRUE./
      DATA TGEND,TGENX/0.,0./
C      -----------------------------------------------------------------
      NBIT2=NMBIT/2
      IF(CHARGE.EQ.0.)RETURN
      STPTRD = .TRUE.
      IF ( DTRD .LT. 2 ) GOTO 999
C
      IF ( FIRST1 ) THEN
        FIRST1 = .FALSE.
        CALL UCTOH('TRD ',TRD,4,4)
      ENDIF
C
      IF ( .NOT. (DTRD .GT. 1 .AND. IHSET .EQ. TRD) ) GOTO 999
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=6
        IPRNT=1
        IF(PTRD.GT.100)THEN
          NDEBM=PTRD/100
          PTRD=MOD(PTRD,100)
        END IF
        PTRDI=PTRD
      END IF
      IFOIS=IFOIS+1
      IF(IDEBUG.EQ.0)THEN
        PTRD=0
        IPRNT=0
      ELSE
C   CHANGE PRINT FLAG AFTER NDEBM TRACKS
        IF(IFOIS.GT.NDEBM)THEN
          PTRD=MIN0(PTRD,5)
          IPRNT=0
        END IF
C        IF(PTRD.GE.8   )THEN
C        END IF
      END IF
C    -----------------------------------------------------------------
C
C INITIALIZATION WHEN ENTERING VOLUME
C
      IF(IPRNT.NE.0 .AND. IDEVT.NE.NEVI)THEN !Check if same event
        NUMTR=0
        IF(PTRD.GE.5)
     +   WRITE(LOUT,*)'---------------idevt',IDEVT,'NEVI',NEVI
        NEVI=IDEVT
      END IF
 4732 FORMAT(' ---ENTER STPTRD WITH P=',G10.4,'  RADIUS',G10.4,' Z',
     +G10.4,' INWVOL',I2,' ISTOP',I2,' ISTAK',I3,' ITRA',I4)
C                          !---------------
      IF(INWVOL.EQ.1)THEN  !ENTERING VOLUME
C                          !---------------
  180   CONTINUE
        NLOOP=0
        ITRAI=0
        LENGTH=0.
        X0=VECT(1)
        Y0=VECT(2)
        Z0=VECT(3)
        CALL UCOPY(VERT,VERTIN,3)
        CALL UCOPY(VECT(1),VECTIN(1),7)
        CALL UCOPY(VECT,VBEFOR,7)
        CALL TLAYER
        CALL TIMEX(TIM0)
        NUMTR=1+NUMTR
        IF(PTRD.GE.8)THEN
          WRITE(LOUT,*)' TIME BETWEEN 2 ENTRANCES IN THE VOLUME',
     +           TIM0-TIM
          WRITE(LOUT,*)' ENTERING VOLUME '
          WRITE(LOUT,*)'R in,Z in     ',SQRT(VECT(1)**2+VECT(2)**2),
     &      VECT(3)
          WRITE(LOUT,*)'R vert,Z vert ',SQRT(VERT(1)**2+VERT(2)**2),
     &      VERT(3)
C           WRITE(LOUT,3443)LEVEL,NAMES(NLEVEL)
 3443     FORMAT(' NLEVEL',I4,' NAMES',A4)
        ENDIF
        RETURN
      END IF
      IF(ISTOP.NE.0 .OR. INWVOL.EQ.2) GO TO 300! Take care of stopping
C                                              ! or decaying tracks
C                          +-------------------------+
C       INWVOL=0           | STEPPING TROUGH VOLUME  |
C                          +-------------------------+
      IF(NLOOP.GT.50)RETURN

C  IF MIDDLE POINT IN THE VOLUME,STEP TROUGH VOLUME
      RM=SQRT((VECT(1)+X0)**2+(VECT(2)+Y0)**2)*.5
      IF(PTRD.GE.8)THEN
        R1=SQRT(X0**2+Y0**2)
        R2=SQRT(VECT(1)**2+VECT(2)**2)
        WRITE(LOUT,*)' R1,R2,RM',R1,R2,RM
      ENDIF
      NLOOP=NLOOP+1
      IF(RM.LT.RADWIN(TSTACK) .OR. RM.GT.RADEXT(TSTACK))THEN
        IF(NLOOP.EQ.1)THEN ! if for first step particle outside the TEC,
C                          ! Take intercept of the track with inner cylinder
          SM=SQRT((VECT(1)-VECTIN(1))**2+(VECT(2)-VECTIN(2))**2+
     &      (VECT(3)-VECTIN(3))**2)
          VIN(1)=X0
          VIN(2)=Y0
          VIN(3)=Z0
          VIN(4)=(VECT(1)-VECTIN(1))/SM
          VIN(5)=(VECT(2)-VECTIN(2))/SM
          VIN(6)=(VECT(3)-VECTIN(3))/SM
          CALL EXTCYL ( VIN, VCYL, RADWIN(TSTACK), IERR)
          IF(PTRD.GE.5)THEN
            WRITE(LOUT,*)' Problem_TRD: Exit TRD volume Step 1'
            WRITE(LOUT,*)' rin,zin   ',SQRT(X0**2+Y0**2),Z0
            WRITE(LOUT,*)' rout,zout ',SQRT(VECT(1)**2+VECT(2)**2),VECT(
     &        3)
            WRITE(LOUT,*)' r cyl,zcyl',SQRT(VCYL(1)**2+VCYL(2)**2),VCYL(
     &        3)
          END IF
          CALL UCOPY(VECT,VBEFOR,3)
          IF(IERR.NE.0)RETURN
          IF(ABS(VCYL(3)-Z0).GT.30.)RETURN
          STEP=0.
          CALL UCOPY(VCYL,VBEFOR,3)
          LENGTH=LENGTH+SQRT((VECTIN(1)-VCYL(1))**2+
     &                       (VECTIN(2)-VCYL(2))**2+
     +                       (VECTIN(3)-VCYL(3))**2)
          NLOOP=41
        END IF
        GO TO 300
      ELSE
        IF(NLOOP.EQ.50)THEN
          WRITE(LOUT,*)'PROBLEM_TRD WARNING:NUMBER OF LOOP =',NLOOP,
     +   ' IN STPTRD FOR THE SAME TRACK ITRA=',ITRA,' ifois',IFOIS
          CALL UCOPY(VECT,VBEFOR,7)
          NLOOP=51
          GO TO 300
        END IF
      END IF
      X1=VECT(1)
      Y1=VECT(2)
      Z1=VECT(3)
      CALL UCOPY(VECT,VBEFOR,7)
      LENGTH=LENGTH+STEP
      IF( PTRD.GE.8) THEN
        WRITE(LOUT,*)'ISTOP',ISTOP,' LENGTH',LENGTH
      ENDIF
      RETURN
C                !---------------------------------+
  300 CONTINUE   !END OF STEPPING,CALL TRD ROUTINES|
C                !---------------------------------+
      LENGTH=LENGTH+STEP
      IF(LENGTH.GE.MINLEN)THEN
        RXEN= SQRT((VECT(1)-VECTIN(1))**2+(VECT(2)-VECTIN(2))**2+
     &    (VECT(3)-VECTIN(3))**2)
        IF(RXEN.LT.MINLEN)THEN
          WRITE(LOUT,*)' PROBLEM_TRD: PATH TOO SMALL: X,Y,Z IN',
     &         (VECTIN(I),I=1,3),' X,Y,Z OUT',(VECT(I),I=1,3),' length',
     &         LENGTH,' rxen',RXEN
C          GO TO 999
        END IF
C
C  DEFINE ZEBRA BANKS GTLY
C
        CALL BKGTLY(LGTLY,TSTACK)
        IZOFS=0
        IZSTAR=LGTLY+IZOFS
        CALL TRNUMB(ITRA,ISAVER,ISATRA)!ISAJET VERTEX AND TRACK NUMBER
        Q(IZSTAR+1)=X0
        Q(IZSTAR+2)=Y0
        Q(IZSTAR+3)=Z0
        Q(IZSTAR+4)=VECT(1)
        Q(IZSTAR+5)=VECT(2)
        Q(IZSTAR+6)=VECT(3)
C ORIGIN VERTEX
        Q(IZSTAR+7)=VERTIN(1)
        Q(IZSTAR+8)=VERTIN(2)
        Q(IZSTAR+9)=VERTIN(3)
        Q(IZSTAR+10)=GETOT/AMASS     !GAMMA OF THE PART.
        IF(INWVOL.NE.0 .OR. ISTOP.NE.0)THEN
          IF(LENGTH.GT.0.1)THEN
            CALL ENTRD(AMASS,GETOT,VECTIN,VECT,LENGTH)
            LENGTH=0.
            STEP=0.
          END IF
          NLOOP=0
          ITRAI=ITRA
        ELSE
          IF(LENGTH.GT.0.1)
     +              CALL ENTRD(AMASS,GETOT,VECTIN,VBEFOR,LENGTH)
          LENGTH=STEP
          X0=X1
          Y0=Y1
          Z0=Z1
          CALL UCOPY(VBEFOR,VERTIN,3)
          CALL UCOPY(VBEFOR,VECTIN,7)
          CALL UCOPY(VBEFOR,Q(IZSTAR+4),3)
          CALL UCOPY(VECT,VBEFOR,7)
          RETURN
        END IF
        IF( TRHIST)THEN
          TGEND=TGEND+EGENRD
          TGENX=TGENX+EGENRX
          TGENX=0.
          TGEND=0.
        END IF
        IF(PTRD.GE.8)THEN
          WRITE(LOUT,*)' CALL TO ENTRD WITH LENGTH=',LENGTH
        END IF
        Q(IZSTAR+11)=EGENST          !TOTAL ENERGY
        Q(IZSTAR+12)=EGSDRI          !TOTAL ENERGY IN DRIFT ZONE
        IQ(IZSTAR+13)=0             !SPARE
        IQ(IZSTAR+14)=IPART
        IQ(IZSTAR+25)=ITRA+1000*ISTAK
C  FILL THE BANKS WITH ENERGY DEPOSITION ON THE WIRES
C  FIRST SORT IN DESCENDING ORDER
        CALL SORTZV(ETOTWI,INDEX,4,1,1,0)
        DO 360 I=1,4
          J=INDEX(I)
          IF(ETOTWI(J).GT. 1000. .OR. ETOTWI(J).LT.0.)THEN
            WRITE(LOUT,*)' PROBLEM_TRD IN STPTRD: DEPOSITED ENERGY =',
     +      ETOTWI(J),' ON WIRE ',IHITWN(J),' FOR TRACK',ITRA,
     +      ' EVENT',IDEVT
            ETOTWI(J)=1000.
          END IF
          IQ(IZSTAR+14+I)=IHITWN(J)+1000*INT(100.*ETOTWI(J))
  360   CONTINUE
        IF(STRD(2).NE.1.)THEN! cathodes strip
          CALL SORTZV(ETOTSN,INDEX,6,1,1,0)
          DO 370 I=1,6
            J=INDEX(I)
            IF(ETOTSN(J).GT. 1000. .OR. ETOTSN(J).LT.0.)THEN
              WRITE(LOUT,*)
     &            ' PROBLEM_TRD IN STPTRD: DEPOSITED ENERGY =',
     +      ETOTSN(J),' ON STRIP',IHITSN(J),' FOR TRACK',ITRA,
     +      ' EVENT',IDEVT
              ETOTSN(J)=1000.
            END IF
            IQ(IZSTAR+18+I)=IHITSN(J)+1000*INT(100.*ETOTSN(J))
  370     CONTINUE
        END IF  ! cathodes
        CALL TIMEX(TIM)
        DT=TIM-TIM0
        IF(INWVOL.EQ.0)GO TO 180
        IF(TRHIST)THEN
          I=7004
          IF(ISTAK.NE.0)I=7005
          CALL HF1(I,DT,1.)
        END IF
        IF(INWVOL.EQ.0)THEN
          CALL UCOPY(VECT(1),VECTIN(1),7)
          CALL TIMEX(TIM0)
        END IF
        IF( PTRD.GE.8)THEN
          WRITE(LOUT,*)' TIME SPENT IN TRD ROUTINES',DT,
     +       'ITRA' ,ITRA,' LAYER',TSTACK
          WRITE(LOUT,*)' EXIT STPTRD WITH ETOT',EGENST,' E DRIFT
     &        ',EGSDRI
        END IF
      END IF
  999 CONTINUE
      RETURN
      END
