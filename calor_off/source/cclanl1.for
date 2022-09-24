      SUBROUTINE CCLANL1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze shower profiles
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:DEADM.INC'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER I,J
      INTEGER IDEPTH,SSUNIT,DMPUNI
      REAL    YDUM,DELI,DELJ
      REAL    RAPC,PHID,PHID1,RAPC1
      REAL SC(3),RADCNT
C
      INCLUDE 'D0$INC:CTRAK.INC'
C
      REAL RCENT(3),ALAM,DCLOSE(3),DCL
      REAL ELIMHI
      REAL ECLUS_TOT,EM_RATIO, ETRANS_RATIO
C
      REAL Z_CLUS,R_CLUS,THETA_ISA,Z_ISA,R_ISA,ZDIFF,RDIFF
      INTEGER LISP1,GZISV1,LISV1
C
      REAL    HM_PHILIM(2),BEAMEN
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      LOGICAL RERUN_DST,USE_CASH
C
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C BOOK HISTOGRAMS HERE
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET_l('RERUN_CAPHEL_FROM_DST',RERUN_DST,IER)
        USE_CASH= .FALSE.
        IF (RERUN_DST) USE_CASH = .TRUE.
        CALL EZGET('BEAM_ENERGY',BEAMEN,IER)
        CALL EZGET_rarr('HMATRIX_PHI_LIMITS',HM_PHILIM,IER)
        ELIMHI = 1.5*BEAMEN
        CALL EZRSET
C
C
        CALL HBOOK1(1,'ENERGY IN EM DEPTH 1',50,0.,0.02*ELIMHI,0.)
        CALL HBOOK1(2,'ENERGY IN EM DEPTH 2',50,0.,0.1*ELIMHI,0.)
        CALL HBOOK1(3,'ENERGY IN EM DEPTH 3',50,0.,ELIMHI,0.)
        CALL HBOOK1(4,'ENERGY IN EM DEPTH 4',50,0.,0.5*ELIMHI,0.)
        CALL HBOOK1(5,'ENERGY IN FH DEPTH 1',50,0.,0.025*ELIMHI,0.)
C
        CALL HBOOK1(11,'FRACTIONAL ENERGY IN EM DEPTH 1',50,0.,0.1,0.)
        CALL HBOOK1(12,'FRACTIONAL ENERGY IN EM DEPTH 2',50,0.,0.2,0.)
        CALL HBOOK1(13,'FRACTIONAL ENERGY IN EM DEPTH 3',50,0.,1.0,0.)
        CALL HBOOK1(14,'FRACTIONAL ENERGY IN EM DEPTH 4',50,0.,0.5,0.)
        CALL HBOOK1(15,'FRACTIONAL ENERGY IN FH DEPTH 1',50,0.,0.2,0.)
C
        CALL HBOOK1(21,'TOTAL LIVE ENERGY',50,0.,ELIMHI,0.)
        CALL HBOOK1(22,'TOTAL LIVE TRANSVERSE ENERGY',50,0.,ELIMHI,0.)
        CALL HBOOK1(23,'TOTAL DEAD ENERGY ',50,0.,0.5*ELIMHI,0.)
        CALL HBOOK1(24,'TOTAL LIVE+DEAD ENERGY ',50,0.,ELIMHI,0.)
        CALL HBOOK1(25,'TOTAL LIVE+DEAD ENERGY ',50,0.5*BEAMEN,
     &                 ELIMHI,0.)
C
        CALL HBOOK1(31,'TOTAL CLUSTER ENERGY',50,0.,ELIMHI,0.)
        CALL HBOOK1(32,'EM/TOTAL RATIO',50,0.,1.,0.)
        CALL HBOOK1(33,'ETRANS/EMTOTAL RATIO',50,0.,1.,0.)
C
        CALL HBOOK2(101,'DEPTH 1 VS 2 CORRELATION ',50,0.,0.1,
     &    50,0.,0.2,0.)
        CALL HBOOK2(102,'DEPTH 2 VS 3 CORRELATION ',50,0.,0.2,
     &    50,0.,1.0,0.)
        CALL HBOOK2(103,'DEPTH 3 VS 4 CORRELATION ',50,0.,1.0,
     &    50,0.,0.5,0.)
        CALL HBOOK2(104,'DEPTH 4 VS 5 CORRELATION ',50,0.,0.5,
     &    50,0.,0.1,0.)
C
        CALL HBOOK2(201,'TOTAL LIVE ENERGY VS TOTAL DEAD ENERGY '
     &    ,50,0.,ELIMHI,50,0.,0.5*ELIMHI,0.)
        CALL HBOOK2(202,'PHI OF TRACK VS DEAD MATERIAL ENERGY',
     &    50,-7.5,17.5,50,0.,0.5*ELIMHI,0.)

C ****  DISTANCE OF CLOSEST APPROACH
C
        CALL HBOOK1(501,'X OF WT. SHOWER CENTER ',50,75.,125.,0.)
        CALL HBOOK1(502,'Y OF WT. SHOWER CENTER ',50,-25.0,25.0,0.)
        CALL HBOOK1(503,'Z OF WT. SHOWER CENTER ',50,-5.0,5.0,0.)

        CALL HBOOK1(511,'DISTANCE OF CLOSEST APPROACH ',50,0.,2.5,0.)
        CALL HBOOK1(512,'DISTANCE OF CLOSEST APPROACH EM3 ',50,0.,2.5,
     &    0.)
C
        CALL HBOOK2(520,'PHI OF TRACK VS PHI EM3WT CENTER OF SHOWER',
     &    50,HM_PHILIM(1),HM_PHILIM(2),50,HM_PHILIM(1),HM_PHILIM(2),
     &    0.)
        CALL HBOOK2(521,'PHI OF TRACK VS PHI CLUS CENTER OF SHOWER',
     &    50,HM_PHILIM(1),HM_PHILIM(2),50,HM_PHILIM(1),HM_PHILIM(2),
     &    0.)
        CALL HBOOK1(522,'PHI EM3 - PHI ISA(DEG)',100,-2.,2.,0.)
        CALL HBOOK1(523,'PHI CLUS - PHI ISA(DEG)',100,-2.,2.,0.)
C        
        CALL HBOOK2(524,'PHI(ISAJET) vs DIFFPHI(EM3WT)',
     &              50,HM_PHILIM(1),HM_PHILIM(2),
     &               50,-1.0,1.0,0.)
        CALL HBOOK2(525,'PHI(ISAJET) vs DIFFPHI',
     &              50,HM_PHILIM(1),HM_PHILIM(2),
     &               50,-1.0,1.0,0.)
C
        CALL HBOOK2(601,'X OF CENTER VS Y OF CENTER ',50,75.,125.,
     &    50,-25.,25.,0.)
        CALL HBOOK2(602,'X OF CENTER VS ENERGY OF SHOWER',50,75.,125.,
     &    50,0.,ELIMHI,0.)
        CALL HBOOK2(603,'PHI OF TRACK VS RADIUS WT. CENTER OF SHOWER',
     &    50,0.0,12.5,50,75.,150.,0.)
        CALL HBOOK2(604,'PHI OF TRACK VS PHI WT. CENTER OF SHOWER',
     &    50,HM_PHILIM(1),HM_PHILIM(2),50,HM_PHILIM(1),HM_PHILIM(2),0.)
        CALL HBOOK2(605,'PHI OF WT. CENTER VS PHI OF UNWT. CENTER',
     &    50,HM_PHILIM(1),HM_PHILIM(2),50,HM_PHILIM(1),HM_PHILIM(2),0.)
        CALL HBOOK2(606,'RAPIDITY OF TRACK VS RAP. WT. CENTER',
     &    50,-1.5,1.5,50,-1.5,1.5,0.)
        CALL HBOOK2(607,'RAP. OF WT. CENTER VS RAP.UNWT. CENTER',
     &    50,-1.5,1.5,50,-1.5,1.5,0.)
C
        CALL HBOOK1(701,'EM3 Z - ISA PROJ. Z',100,-1.,1.,0.)
        CALL HBOOK1(702,'EM3 R - ISA PROJ. R',100,-1.,1.,0.)
        CALL HBOOK1(703,'Z clus - ISA PROJ. Z',100,-1.,1.,0.)
        CALL HBOOK1(704,'R clus - ISA PROJ. R',100,-1.,1.,0.)
        CALL HBARX(522)
        CALL HBARX(523)        
        CALL HBARX(524)
        CALL HBARX(525)
      ENDIF
C
C ****  FILL HISTOGRAMS HERE
C
C
      CALL CEDEAD
C
C ****  Pick up dead material layers
C

C
      CALL CEMENR(NDPTH,ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX,
     &  ETAMX,USE_CASH)
C
      DO 30 IDEPTH = 1,NDPTH
        CALL HFILL(IDEPTH,ENDPTH(IDEPTH),YDUM,1.0)
        CALL HFILL(IDEPTH+10,PEDPTH(IDEPTH),YDUM,1.0)
   30 CONTINUE
C
      CALL HFILL(21,ETOT,YDUM,1.)
      CALL HFILL(22,ET,YDUM,1.0)
      CALL HFILL(23,EDEADT,YDUM,1.0)
      CALL HFILL(24,ETOT+EDEADT,YDUM,1.0)
      CALL HFILL(25,ETOT+EDEADT,YDUM,1.0)
C
      ECLUS_TOT = Q(LCACL+17)
      EM_RATIO = ETOT/ECLUS_TOT
      ETRANS_RATIO = ETRANS/ETOT
      CALL HFILL(31,ECLUS_TOT,YDUM,1.)
      CALL HFILL(32,EM_RATIO,YDUM,1.)
      CALL HFILL(33,ETRANS_RATIO,YDUM,1.)
C
      CALL HFILL(101,PEDPTH(1),PEDPTH(2),1.0)
      CALL HFILL(102,PEDPTH(2),PEDPTH(3),1.0)
      CALL HFILL(103,PEDPTH(3),PEDPTH(4),1.0)
      CALL HFILL(104,PEDPTH(4),PEDPTH(5),1.0)
C
      CALL HFILL(201,ETOT,EDEADT,1.0)
C
      CALL HFILL(202,PHI(1),EDEADT,1.0)  ! SCATTER
C
      SC(1) = Q(LCACL+14)
      SC(2) = Q(LCACL+15)
      SC(3) = Q(LCACL+16)             ! CENTER OF SHOWER
      RCENT(1) = SC(1)-VERT(1)
      RCENT(2) = SC(2)-VERT(2)
      RCENT(3) = SC(3)-VERT(3)  ! CENTER OF ENERGY WRT VERTEX
      RADCNT = SQRT(SC(1)**2+SC(2)**2)        ! RADIUS OF SHOWER CENTER
      PHID = ATAN2(RCENT(2),RCENT(1))
      PHID = PHID/RADIAN              ! IN DEGREES
      RAPC1 = Q(LCACL+13)             ! UNW RAP OF CENTER
      PHID1 = Q(LCACL+12)/RADIAN
C
C ****  RAPC,PHID1 ARE UNWEIGHTED RAPIDITY AND PHI OF CENTER
C
C
C ****  FIND DISTANCE OF CLOSEST APPROACH OF THE ISAJET TRAJECTORY
C ****  TO THE CENTER OF ENERGY
C
      CALL CLOSE_DIST(SC,VERT(1),UVEC(1,1),DCL)
C
      DO 50 I = 1,3
        CALL HFILL (500+I,SC(I),YDUM,1.0)
   50 CONTINUE
C
      CALL HFILL (511,DCL,YDUM,1.0)
      CALL HFILL(601,RCENT(1),RCENT(2),1.)
      CALL HFILL(602,RCENT(1),ETOT,1.)
      CALL HFILL(603,PHI(1),RADCNT,1.0)
      CALL HFILL(604,PHI(1),PHID,1.0)
      CALL HFILL(605,PHID,PHID1,1.0)
      CALL HFILL(606,RAP(1),RAPC,1.0)
      CALL HFILL(607,RAPC,RAPC1,1.0)
C
      CALL HFILL(521,PHI(1),PHID,1.)
      CALL HFILL(523,PHID-PHI(1),0.,1.)
      CALL HFILL(525,PHI(1),PHID-PHI(1),1.)
C
C
C
      LISV1 = GZISV1()
      IF(LISV1.EQ.0) GOTO 200
      LISP1 = LQ(LISV1-IZISP1)
      THETA_ISA = Q(LISP1+8)
      R_CLUS = SQRT(SC(1)**2 + SC(2)**2)
      IF(abs(SC(3)) .LT. 170) THEN         !CC
        Z_ISA  = VERT(3) + R_CLUS*TAN(HALFPI-THETA_ISA)
        ZDIFF = Z_ISA - SC(3)
        CALL HFILL(703,ZDIFF,YDUM,1.)
      ELSE
        Z_CLUS = SC(3)
        R_ISA  = (Z_CLUS-VERT(3))*TAN(THETA_ISA)
        RDIFF = R_ISA - R_CLUS
        CALL HFILL(704,RDIFF,YDUM,1.)
      ENDIF
C
C
C ****  NOW FOR EM3 SUBLAYER...
C
200   SC(1) = Q(LCACL+20)
      SC(2) = Q(LCACL+21)
      RCENT(1) = SC(1)-VERT(1)
      RCENT(2) = SC(2)-VERT(2)
      PHID = ATAN2(RCENT(2),RCENT(1))
      PHID = PHID/RADIAN              ! IN DEGREES
      CALL HFILL(520,PHI(1),PHID,1.)
      CALL HFILL(522,PHID-PHI(1),0.,1.)
      CALL HFILL(524,PHI(1),PHID-PHI(1),1.)
C
      SC(1) = Q(LCACL+20)
      SC(2) = Q(LCACL+21)
      SC(3) = Q(LCACL+22)             ! CENTER OF SHOWER
      CALL CLOSE_DIST(SC,VERT(1),UVEC(1,1),DCL)
      CALL HFILL (512,DCL,YDUM,1.0)
C
C ****  NOW MATCH IN ETA DIRECTION. Compare Z at radius of cluster 
C ****        center if in CC, or r at z of cluster center if in EC
C
      LISV1 = GZISV1()
      IF(LISV1.EQ.0) GOTO 999
      LISP1 = LQ(LISV1-IZISP1)
      THETA_ISA = Q(LISP1+8)
      R_CLUS = SQRT(SC(1)**2 + SC(2)**2)
      IF(abs(SC(3)) .LT. 170) THEN         !CC
        Z_ISA  = VERT(3) + R_CLUS*TAN(HALFPI-THETA_ISA)
        ZDIFF = Z_ISA - SC(3)
        CALL HFILL(701,ZDIFF,YDUM,1.)
      ELSE
        Z_CLUS = SC(3)
        R_ISA  = (Z_CLUS-VERT(3))*TAN(THETA_ISA)
        RDIFF = R_ISA - R_CLUS
        CALL HFILL(702,RDIFF,YDUM,1.)
      ENDIF
  999 RETURN
      END
