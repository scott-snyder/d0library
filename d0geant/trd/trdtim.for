      REAL FUNCTION TRDTIM(XTIM,YTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMPUTE DRIFT TIME FOR TRD CLUSTERS.
C-   PARAMETRIZATION FOR ISOCHRONES ARE TAKEN FROM TEST CHAMBER FILLED
C-   XE/CH4
C-
C-   Inputs  :X,Y OF THE CLUSTER
C X=Y=0 ON THE ANODE.ACTUALLY XJFD=YAZ,YJFD=(XAZ-XANOD)
C IN THE DATA Y =YJFD-YANO
C Y DANS LA DIRECTION DE DRIFT
C UNITS: LENGTHS ARE IN CM , TIMES ARE IN  NANOSECONDS
C values for dift time at the anode are given for
C N points (N<50)  regularly spaced on the line 
C issu de (X0D,Y0D). A droite de cette demi-droite les temps de
C drift a l'anode ne dependent que du rayon. a gauche la vitesse
C est dans la direction OY et egale a VD.
C-   Outputs :DRIFT TIME
C-
C
C
C-   Created                J.F. DETOEUF  ESQ.
C-   Updated  20-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated  10-MAY-1991   A. Zylberstejn  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X,XTIM,Y,YTIM
      LOGICAL FIRST
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      REAL TUD(50)
      REAL VD,YANO, X0D,DXD
      DATA X0D,DXD/0.0000, 0.1000E-01/
      REAL DTUD(50),TRD(60),DTRD(60)
      INTEGER I,J,IU,IR1,IR2,IR,IR0,NR,NX
      REAL DRD,XU,YU,R1,R2,TG,R0,R,DR,TR,DX
      REAL Y0D,DYD
      DATA Y0D,DYD/1.700 ,0.0000     /
      DATA VD,YANO / .0025 , 2.0 /
      DATA FIRST/.TRUE./
      DATA TUD/
     +  196.1   ,196.2   ,196.5   ,196.8   ,197.4,
     +  198.0   ,198.8   ,199.7   ,200.7   ,201.8,
     +  203.1   ,204.5   ,206.0   ,207.6   ,209.2,
     +  210.8   ,212.5   ,214.4   ,216.2   ,218.4,
     +  220.7   ,222.6   ,224.7   ,227.0   ,229.3,
     +  231.7   ,234.3   ,236.7   ,239.1   ,241.7,
     +  244.4   ,247.3   ,250.2   ,253.4   ,256.9,
     +  260.7   ,266.5   ,273.2   ,279.1   ,361.9,
     +  10*0.0000                                    /
C ...................................................
      IF (.NOT.FIRST) GO TO 100
      FIRST=.FALSE.
C      PRINT *, ' VD: ' , VD,' YANO: ',YANO
C-C      PRINT '(A1,A20,2G12.4)',  ' ',' X0D,DXD: ',X0D,DXD
C-C      PRINT '(A1,A20,2G12.4)', ' ',' Y0D,DYD: ',Y0D,DYD
      Y0D= Y0D-YANO
C-C      PRINT *,'TUD LU: '
C-C      PRINT *,(TUD(I),I=1,50)
      DO 10 I= 1,49
        J=50-I
   10 TUD(J+1)= TUD(J)
      DO 11 I=1,49
   11 DTUD(I)=TUD(I+1)-TUD(I)
      DRD=SQRT(DXD**2+DYD**2)
C      PRINT *, 'DRD: ',DRD
      DO 13 IU= 1,49
        XU= X0D+(IU-1)*DXD
        YU= Y0D+(IU-1)*DYD
        R1= SQRT(XU**2+YU**2)
        R2= SQRT( (XU+DXD)**2+(YU+DYD)**2 )
        IR1= R1/DRD
        IR2= R2/DRD
        IF(IR2.LE.IR1) GO TO 13
        DO 12 IR= IR1+1,IR2
          IF (IR.GT.60) THEN
            WRITE(LOUT,*)' PROBLEM_TRD ',
     +      'TRDTIM-ERROR IN QNU-TRD FOR IU,IR: ',IU,IR
            GO TO 12
          ENDIF
          TRD(IR)= TUD(IU)+(IR*DRD-R1)*DTUD(IU)/DRD
C-       PRINT '(1X,8G12.4)','IU','IR','R1','TRD','TUD','DRD','IR1','DTU
C-       PRINT '(4X,8G12.4)',IU,IR,R1,TRD(IR),TUD(IU),DRD,IR1,DTUD(IU)
   12   CONTINUE
   13 CONTINUE
      TRD(1)=0
      DO 14 I=1,59
   14 DTRD(I)= TRD(I+1)-TRD(I)
C      WRITE(LOUT,*) 'TUD NEW: '
C      WRITE(LOUT,*) '(/1X,A20/(4(I4,2G12.4)))',
C     +    'IU,TUD,DTUD: ',(I,TUD(I),DTUD(I),I=1,50)
C      WRITE(LOUT,*)'(/1X,A20/(4(I4,2G12.4)))',
C     +    'IR,TRD,DTRD: ',(I,TRD(I),DTRD(I),I=1,50)
      TG= DYD/DXD
      R0= SQRT(X0D**2+Y0D**2)
      IR0= R0/DRD
C     PRINT *, 'R0,IR0: ' ,R0,IR0
C???????      CLOSE(32)
C 100  ====================---- QNU TRDTIM
  100 CONTINUE
      X=AMIN1(XTIM,.39)
      Y=-ABS(YTIM)
      YU= Y0D+TG*(X-X0D)
      IF(Y.GE.YU) THEN
C                          --- Z AMPLI
        R= SQRT(X**2+Y**2)
        NR= R/DRD +1
        nr=min0(nr,49)
        DR= R-NR*DRD
        TR=TRD(nr)
        IF(NR.LE.50)TR= TRD(NR)+DTRD(NR)*DR/DRD
        IF(R.LT.R0) TR= R*TUD(1)/(IR0*DRD)
        TRDTIM=TR
      ELSE
C                           --- Z DERIV
        NX= (X-X0D)/DXD +1
        DX= X-X0D-NX*DXD
        TR= TUD(NX)+DTUD(NX)*DX/DXD
        TRDTIM= TR-(Y-YU)/VD
      ENDIF
      IF(TRDTIM.LE.0.)THEN
        WRITE(LOUT,*)
     +       ' PROBLEM_TRD   TRDTIM COMPUTED NEGATIVE DRIFT TIME ! ',
     +       'X: ',X,'Y: ',Y,'TRDTIM: ',TRDTIM,'TR: ',TR,'R: ',R
        WRITE(LOUT,*)'NR:',NR,'DR: ',DR,'YU: ',YU,'NX: ',NX,
     +                    'DX: ',DX
      END IF
      RETURN
C                           ---- ERREUR LECTURE
  999 CONTINUE
      WRITE(LOUT,*)'**** ERREUR DE LECTURE TUD DANS TRDTIM(X,Y)'
      TRDTIM= 99999999.99
      RETURN
C                           ---- NEXT END FUNCTION TRDTIM
      END
