      SUBROUTINE FMUCHK(MUTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pi to mu decays that occur in the FDC.
C-
C-   Outputs : MUTRAK = TRUE if there is a pi->mu kink in FDC
C-
C-   Created  16-AUG-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INCLUDE 'D0$LINKS:IZISP2.LINK'
C
      INTEGER ID1, IDV1, ID2, IDV2
      INTEGER LISV1,LISP1,LISV2,LISP2
      INTEGER NISV1,NISP1,NISV2,NISP2,MISP1,MISP2
      INTEGER IOUT,IERR,NMUOUT,NMUONS,NPIONS,MAXHIS,HOFSET
      INTEGER JUSTIN,JUSTMU,INONE,MMUONS
      REAL    MASS1,PV1(4),PP1(4),X1,Y1,Z1,PHI1,THETA1,ETA1
      REAL    MASS2,PV2(4),PP2(4),X2,Y2,Z2,PHI2,THETA2,ETA2
      REAL    ANGLE,COSANGLE,RADIUS,NUM,DENOM1,DENOM2
      LOGICAL MUTRAK,INSIDE_FDC,SEC_MUON
      PARAMETER( MAXHIS = 9 )
      PARAMETER( HOFSET = 100 )
      INTEGER NID(MAXHIS),IDH,NID2
      LOGICAL FIRST
      CHARACTER*34 NAME(MAXHIS)
      REAL ISHIST(4,MAXHIS)
      DATA FIRST/.TRUE./
      DATA NAME/' Position of all ISV2 in Z',' Radius of all ISV2',
     &          ' Number of ISP2 muons',' Angle between pion and muon',
     &          ' Energy diff bet pion and muon',' Num ISP2 mus per pi',
     &          ' Angle bet pi,mu inside FDC',
     &          ' Energy diff bet pi,mu inside FDC',' '/ 
      DATA NMUOUT,JUSTIN,JUSTMU,INONE /0,0,0,0/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('ISHIST(1)',ISHIST(1,1),IERR)
        CALL EZRSET
        DO 10 IDH=1,MAXHIS
          IF (ISHIST(1,IDH).EQ.0.) GO TO 10
          CALL HBOOK1(IDH+HOFSET,NAME(IDH),
     &           NINT(ISHIST(2,IDH)),ISHIST(3,IDH),ISHIST(4,IDH),0.)
   10   CONTINUE
        CALL HBOOK2(10+HOFSET,' All ISV2 R vs Z',100,-150.,150.,
     &                          100,-0.5,99.5,0.)
        FIRST = .FALSE.
      END IF
C
      MUTRAK = .FALSE.
      INSIDE_FDC=.FALSE.
      SEC_MUON=.FALSE.
      NISV1=0
      NISP1=0
      NISV2=0
      NISP2=0
      NPIONS=0
      NMUONS=0
C
      LISV1=0
  100 CALL GTISV1(LISV1,LISV1,IDV1,PV1,X1,Y1,Z1)        ! CHECK FOR ISV1
      IF(LISV1.GT.0) THEN
        NISV1=NISV1+1
        MISP1=0
        LISP1=LISV1-IZISP1
  200   CALL GTISP1(LISP1,LISP1,ID1,PP1,PHI1,THETA1,ETA1)   ! CHECK FOR ISP1
        IF(LISP1.GT.0) THEN
          NISP1=NISP1+1
          MISP1=MISP1+1
          IF(IABS(ID1).NE.120) THEN
C            WRITE(*,*) ' FAILED - NO PION, ID=',ID1
          ELSE
            NPIONS=NPIONS+1
            LISV2=LISP1-IZISV2
  300       CALL GTISV2(LISV2,LISV2,IDV2,PV2,X2,Y2,Z2)    ! CHECK FOR DECAY
            IF(LISV2.GT.0) THEN
              NISV2=NISV2+1
              IF (ISHIST(1,1).EQ.1.) CALL HFF1(1+HOFSET,NID(1),Z2,1.)
              RADIUS=SQRT(X2**2.+Y2**2.)
              IF (ISHIST(1,2).EQ.1.) CALL HFF1(2+HOFSET,NID(2),RADIUS,
     &          1.)
              CALL HFF2(10+HOFSET,NID2,Z2,RADIUS,1.)
              IF( ABS((ABS(Z2)-120.0)) .LE. 20.0 ) THEN
                IF( RADIUS .LE. 60. .AND. RADIUS .GE. 11.0 ) THEN
                  INSIDE_FDC=.TRUE.
                ENDIF
              ENDIF
              MISP2=0
              MMUONS=0
              LISP2=LISV2-IZISP2
  400         CALL GTISP2(LISP2,LISP2,ID2,PP2,PHI2,THETA2,ETA2) ! CHECK FOR MUON
              IF(LISP2.GT.0) THEN
                NISP2=NISP2+1
                MISP2=MISP2+1
                IF(IABS(ID2).EQ.14) THEN
                  SEC_MUON=.TRUE.
                  NMUONS=NMUONS+1
                  NUM = PP1(1)*PP2(1) + PP1(2)*PP2(2) + PP1(3)*PP2(3) 
                  DENOM1 = SQRT(PP1(1)**2. + PP1(2)**2. + PP1(3)**2.)
                  DENOM2 = SQRT(PP2(1)**2. + PP2(2)**2. + PP2(3)**2.)
                  COSANGLE = NUM / (DENOM1*DENOM2)
                  ANGLE = ACOS(COSANGLE)
                  IF(ISHIST(1,4).EQ.1.) CALL HFF1(4+HOFSET,NID(4),ANGLE,
     &              1.)
                  IF(ISHIST(1,5).EQ.1.) CALL HFF1(5+HOFSET,NID(5),
     &              (PP1(4)-PP2(4)),1.)
                  IF(INSIDE_FDC) THEN
                    MMUONS=MMUONS+1
                    IF(ISHIST(1,7).EQ.1.) CALL HFF1(7+HOFSET,NID(7),
     &                ANGLE,1.)
                    IF(ISHIST(1,8).EQ.1.) CALL HFF1(8+HOFSET,NID(8),
     &                (PP1(4)-PP2(4)),1.)
                  ENDIF
                ENDIF                   ! End of secondary muon check
                GOTO 400
              ENDIF                     ! End of valid ISP2 banks
              IF (ISHIST(1,6).EQ.1.) CALL HFF1(6+HOFSET,NID(6),
     &                  FLOAT(MMUONS),1.)
              GOTO 300
            ENDIF                       ! End of valid ISV2 banks
          ENDIF                         ! End of initial pion check
          GOTO 200
        ENDIF                           ! End of valid ISP1 banks
        GOTO 100
      ENDIF                             ! End of valid ISV1 banks
C
      IF (ISHIST(1,3).EQ.1.) CALL HFF1(3+HOFSET,NID(3),FLOAT(NMUONS),1.)
C
  900 CONTINUE
      IF(INSIDE_FDC .AND. SEC_MUON) THEN
        NMUOUT=NMUOUT+1
        MUTRAK=.TRUE.
      ELSEIF(INSIDE_FDC) THEN
        JUSTIN=JUSTIN+1
      ELSEIF(SEC_MUON) THEN
        JUSTMU=JUSTMU+1
      ELSE
        INONE=INONE+1
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
