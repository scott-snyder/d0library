      SUBROUTINE CACLFL(IHAD,SCEXP,NCLUST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill CACL and CACH banks using Clustering info
C-                         from CATE status word and CAEH
C-
C-   *****NOTE*****
C-        SOME OF THE QUANTITIES FILLED HERE CAN BE OVERWRITTEN AT A LATER
C-        TIME. THIS IS ESPECIALLY TRUE OF THE POSITIONS, WHICH ARE
C-        RECALCULATED IN CAPHEL
C-   *****NOTE*****
C-
C-   Inputs  : IHAD =1 EM IHAD =2 EM+hadronic clusters
C-             SCEXP = cell weight for center determination
C-   Outputs:  NCLUST = number of clusters found
C-   Controls:
C-
C-   Created   7-MAY-1989   Rajendran Raja
C-   Updated  29-JUL-1989   Rajendran Raja
C-   Modified 11-OCT-1989   Gerald C. Blazey
C-   Updated  27-MAR-1990   Norman Graf, Harrison B. Prosper
C-      Added sum of EM plus hadronic energy for the clusters. This is
C-      redundant for Hadronic clusters.
C-   Updated  21-AUG-1990   Norman A. Graf  Added FH layer 1 contribution
C-                          explicitly as 19th word
C-   Updated  10-AUG-1993   Norman A. Graf  remove filling of version
C-                          numbers in CACL and CACH banks. This is done
C-                          in BK* routines and was inadvertently over-
C-                          written here.
C-   Updated   9-NOV-1993   Norman A. Graf:Moved CALWIN call to PELC/PPHOFL
C
C          ****  CHANGE BANK VERSION  NUMBERS IN BK ROUTINES  ****
C-   Updated   6-SEP-1994   Meenakshi Narain   take out calls to JBIT
C-                            to optimize time on SGI (suggested by H. Greenlee)
C-   Updated   8-SEP-1994   Meenakshi Narain
C-                            Replace CELXYZ by CELXYZ_FAST
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CLUPAR.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER NCLUST,IER
      INTEGER I,J,K
      INTEGER CLASS,NEXT,CATW
C
      REAL    HLFTHT
      INTEGER NRCAEH,POINTER,ILYR,IJ
      INTEGER ICL,PCATE,IHAD
      INTEGER  ETAI,PHII
      INTEGER NLYRI,NRCATE,LYRS(NLYRL),KK,CHECK_BIT
C
      INTEGER JLYR,PCATEMP,LYRT(NLYRL)
C
      INTEGER NMCACH                    ! Maximum number of cells per cluster
      PARAMETER( NMCACH = 1000 )
      INTEGER NCACH,PCACH(NMCACH)
C
      REAL    XX,YY,ZZ,EE,XBAR,YBAR,ZBAR,ETOT,SCEXP
      REAL    XBAR3,YBAR3,ZBAR3,EM3TOT
      INTEGER IOK
      REAL    ECMX,ENCATE
      INTEGER CATEMX
C
      INTEGER CC,JJ
      LOGICAL DONE
      LOGICAL FIRST,LOOP,DO_WINDOW
      DATA FIRST/.TRUE./
C
      CHARACTER*40 STRING
C
      INTEGER IETAMX,IPHIMX,SIZE
      REAL WINDOW_ETOT,WINDOW_ET
C----------------------------------------------------------------------
C
C ****  defining statement functions class,next
C
      CATW(J) = (J-1)*NREP+LCATE
C
C NREP,LCATE are the rep. num + addr of the CATE bank and will be
C defined later.
C
      CLASS(J) = CATW(J)+ICLASS
C
C gives the class number address of  tower J
C
      NEXT(J) = CATW(J)+INEXT
C
C gives the connection to tower J
C
C ****  statement function definitions complete
C
C----------------------------------------------------------------------
c      IF ( FIRST ) THEN
c        CALL EZPICK('CAPHEL_RCP')
c        CALL EZGET('DO_WINDOW',DO_WINDOW,IER)
c        CALL EZRSET
c        FIRST = .FALSE.
c      ENDIF
 
      NRCATE = IQ(LCATE+2)
      NRCAEH = IQ(LCAEH+2)              ! Repetition number
C
      ICL = 0
      NCLUST = 0
      IER = 0
C
C ****  Identify end of each class by checking for the elements in
C       which NEXT and CLASS are equal.
C
C
      DO 100 CC = ILO,IHI
        IF( IQ(CLASS(CC)) .NE. IQ(NEXT(CC)) )GO TO 100
        JJ = CC
        NCACH = 0                       ! Zero it
C
C ****   book it and then drop if no need
C
        ICL = ICL + 1
        CALL BKCACL(LCACL)
C
C        IQ(LCACL+1) = 1               ! Bank version
        IQ(LCACL+2) = ICL             ! Cluster number
        IQ(LCACL+3) = IHAD            ! EM/HADr flag
        CALL UZERO(Q(LCACL+4),1,10)   ! Zero 10 words for summing. ET
        CALL UZERO(Q(LCACL+17),1,3)   ! Zero 3 words for summing. TOTAL ET
C                                       ! sum will be overwritten
        XBAR = 0.
        YBAR = 0.
        ZBAR = 0.
C
        XBAR3 = 0.
        YBAR3 = 0.
        ZBAR3 = 0.
C
        ETOT = 0.                     ! working out center of energy
        EM3TOT = 0.
        ECMX = -1.E8                       ! maximum CATE tower energy
        CATEMX = 0
C
C
C ****  Loop over a class starting at the element with next equal to
C       class.  Leave loop when that element is re-encountered.
C
        LOOP=.TRUE.
        DO WHILE (LOOP)               ! Test for wrap
C
          PCATE = CATW(JJ)
          NLYRI = IQ(PCATE+11)
          ETAI = IQ(PCATE+12)
          PHII = IQ(PCATE+13)
C
C ****  now to find CATE tower with maximum energy
C
          ENCATE = Q(PCATE+7)
          IF(ENCATE.GT.ECMX)THEN
            ECMX = ENCATE
            CATEMX = JJ                  ! Tower number
          ENDIF
C
          ILYR =  0
          CHECK_BIT = IQ(PCATE+17)
          DO 20 KK = 1,NLYRL
C            IF ( JBIT(IQ(PCATE+17),KK).NE.0 ) THEN
            IF ( 2*(CHECK_BIT/2) .NE. CHECK_BIT ) THEN
              ILYR = ILYR+1
              LYRS(ILYR) = KK
            ENDIF
            CHECK_BIT = CHECK_BIT/2
   20     CONTINUE
          IF(ILYR.NE.NLYRI)THEN
            CALL ERRMSG('CALORIMETER','CACLFL',
     &        'MIS-MATCH IN LAYER NUMBER ','W')
            NLYRI = ILYR
          ENDIF
C
C ****  WANT TO GET TOTAL SUM FOR ELECTRONS, SO USE BIT LIST FROM
C ****  TOTAL ENERGY CATE TOWER (USE PTCATE TO GET POINTER)
C
          IF(IHAD .EQ. 1) THEN
            J = PTCATE(ETAI,PHII,2)
            PCATEMP = CATW(J)
            JLYR = 0
            CHECK_BIT = IQ(PCATEMP+17)
            DO 30 KK = 1,NLYRL
C              IF ( JBIT(IQ(PCATEMP+17),KK).NE.0 ) THEN
              IF ( 2*(CHECK_BIT/2) .NE. CHECK_BIT ) THEN
                JLYR = JLYR+1
                LYRT(JLYR) = KK
              ENDIF
              CHECK_BIT = CHECK_BIT/2
   30       CONTINUE
          ENDIF
C
C ****  Sum over layers
C
          DO 500 IJ = 1 , NLYRI
            ILYR = LYRS(IJ)
C
C ****  now get the PTCAEH flags
C
            POINTER = (PTCAEP(ETAI,PHII,ILYR)-1)*NRCAEH+LCAEH   ! To
                                                                ! CAEH
C
            IF(IHAD .NE. 1) THEN    ! Done later for Electron
              Q(LCACL+17) = Q(POINTER+7)+Q(LCACL+17)        ! E
              Q(LCACL+18) = Q(POINTER+8)+Q(LCACL+18)        ! ET
            ENDIF
C
            IF(ILYR.EQ.MNLYFH) THEN ! First FH layer (needed for Em ratio)
              Q(LCACL+19) = Q(POINTER+7)+Q(LCACL+19)
            ENDIF
C
            IF(NCACH.LT.NMCACH)THEN
              NCACH = NCACH+1             ! CACH store
              PCACH(NCACH) = PTCAEP(ETAI,PHII,ILYR)
            ELSE
              CALL ERRMSG('CALORIMETER','CACLFL',
     &          'TOO MANY ELEMENTS PER CLUSTER ','W')
            ENDIF
C
            DO 350 K = 4,10
  350       Q(LCACL+K) = Q(POINTER+K)+Q(LCACL+K)
C
            IF(Q(POINTER+7).LT.0) THEN
              EE = 0.
              IF(Q(POINTER+7).LT.-5.) THEN
                WRITE(STRING,1) ETAI,PHII,ILYR
                CALL ERRMSG('CALORIMETER','CELL LT 5 GEV!',STRING,'W')
              ENDIF
            ELSE
              EE = Q(POINTER+7)**SCEXP           ! ENERGY OF CELL WEIGHTED
            ENDIF
C
C ****  The scheme here is to use Energy weighting by SCEXP
C ****  to get the closest Center of shower to the true center.
C
            CALL CELXYZ_FAST(ETAI,PHII,ILYR,XX,YY,ZZ,IOK)
            IF(IOK.NE.0)THEN
              CALL ERRMSG('CALORIMETER','CACLFL',
     &          ' CELXYZ_FAST ERROR ','W')
              WRITE(STRING,1) ETAI,PHII,ILYR
              CALL ERRMSG('CALORIMETER','DIAGNOSTIC',STRING,'W')
            ELSE
              XBAR = XBAR + XX*EE
              YBAR = YBAR + YY*EE
              ZBAR = ZBAR + ZZ*EE
              ETOT = ETOT +EE
C
              IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D) THEN
                XBAR3 = XBAR3 + XX*EE
                YBAR3 = YBAR3 + YY*EE
                ZBAR3 = ZBAR3 + ZZ*EE
                EM3TOT = EM3TOT +EE
              ENDIF
C
            ENDIF
C
  500     CONTINUE
C
C ****  Sum E and ET over ALL layers for electrons
C
          IF(IHAD .EQ. 1) THEN
            DO 600 IJ = 1,JLYR
              ILYR = LYRT(IJ)
C
C ****  now get the PTCAEH flags
C
              POINTER = (PTCAEP(ETAI,PHII,ILYR)-1)*NRCAEH+LCAEH   ! To CAEH
              Q(LCACL+17) = Q(POINTER+7)+Q(LCACL+17)        ! E
              Q(LCACL+18) = Q(POINTER+8)+Q(LCACL+18)        ! ET
  600       CONTINUE
          ENDIF
          JJ = IQ(NEXT(JJ))
          LOOP = IQ(NEXT(JJ)) .NE. IQ(CLASS(JJ))
        ENDDO
C
C ****  CLUSTER SUMMING DONE
C
        IF(Q(LCACL+7).GT.0.0)THEN
          Q(LCACL+8) = SQRT(Q(LCACL+4)**2+Q(LCACL+5)**2)        ! ET
        ELSE
          Q(LCACL+8) = -SQRT(Q(LCACL+4)**2+Q(LCACL+5)**2)        ! ET
        ENDIF
        Q(LCACL+11) = ATAN2(ABS(Q(LCACL+8)),Q(LCACL+6))     ! Theta
        HLFTHT = Q(LCACL+11)/2.0
        IF(Q(LCACL+4).EQ.0.0 .AND. Q(LCACL+5).EQ.0.0)THEN
          Q(LCACL+12)=0.0
        ELSE
          Q(LCACL+12) = ATAN2(Q(LCACL+5),Q(LCACL+4))     ! Phi
        ENDIF
        IF(Q(LCACL+12).LT.0.0)Q(LCACL+12)=Q(LCACL+12)+TWOPI     ! 0 TO
C                                        ! TWO PI
        IF(SIN(HLFTHT)/COS(HLFTHT).GT.0.)THEN
          Q(LCACL+13) = -ALOG(SIN(HLFTHT)/COS(HLFTHT))  ! pseudorap of cluster
        ELSEIF(SIN(HLFTHT).EQ.COS(HLFTHT))THEN
          Q(LCACL+13) = 0.0
        ELSE
          Q(LCACL+13) = 9999.
        ENDIF
C
C ****  Now to store the center of energy of the cells
C
        IF(ETOT.NE.0)THEN
          XBAR = XBAR/ETOT
          YBAR = YBAR/ETOT
          ZBAR = ZBAR/ETOT
C
          IF(EM3TOT.GT.0) THEN
            XBAR3 = XBAR3/EM3TOT
            YBAR3 = YBAR3/EM3TOT
            ZBAR3 = ZBAR3/EM3TOT
          ENDIF
C
          Q(LCACL+14) = XBAR
          Q(LCACL+15) = YBAR
          Q(LCACL+16) = ZBAR
C
          Q(LCACL+20) = XBAR3
          Q(LCACL+21) = YBAR3
          Q(LCACL+22) = ZBAR3
C
C        ELSE
C          CALL ERRMSG('CALORIMETER','CACLFL',
C     &          ' ZERO ENERGY CLUSTER ','W')
        ENDIF
        CALL BKCACH(LCACL,NCACH+3,LCACH)
C        IQ(LCACH+1) = 1               ! Bank version
        IQ(LCACH+2) = NCACH           ! Number of cells in cluster
        CALL UCOPY(PCACH(1),IQ(LCACH+3),NCACH)        ! Pointers
        IQ(LCACH+NCACH+3) = CATEMX
C
C ****  NOW FOR THE LBL WINDOW...
C
c        IF(IHAD .EQ.1 .AND. DO_WINDOW) THEN  !only interesting for electrons
c          IETAMX = IQ(LCATE + (CATEMX-1)*14 + 12)
c          IPHIMX = IQ(LCATE + (CATEMX-1)*14 + 13)
c          CALL CALWIN(IETAMX,IPHIMX,WINDOW_ETOT,WINDOW_ET,SIZE)
c          Q(LCACL+23) = WINDOW_ETOT
c        ENDIF
C
  100 CONTINUE
      NCLUST = ICL                      ! TOTAL NUMBER OF CLUSTERS
      RETURN
    1 FORMAT(' ','ETAI,PHII,ILYR',3I5)
      END
