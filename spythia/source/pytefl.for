      SUBROUTINE PYTEFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CREATES PARTICLE BANKS FOR PYTHIA EVENT
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-JAN-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAQ.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAJ.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'

      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ISALNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:SEED.INC/LIST'

      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER N,K,MDCY,MDME,KFDP,MSEL,MSUB,KFIN,MSTP,MSTI
      REAL P,V,BRAT,CKIN,PARP,PARI

      INTEGER ITAG(4000)

      INTEGER IOISAE,IOISAQ,IOISAJ,IOISV1,IOISP1,IUH
      INTEGER LISAQ,LISAJ,LISAE,LISV1,LISP1
C
      INTEGER I,J,DTR,NVERTX,IWPOS,IWNEG,ID,IV,NPART,LVP,NJT,NQS
      INTEGER IZ,PNTR, eventno
      INTEGER NPJET,LPJHD,GZPJHD
      REAL XYZB(3),XYZ(3,NVD),MOM(5),THETA,PHI,ETA
      LOGICAL FIRSTVTX
C
      LOGICAL FIRST,FIRST_VERTEX
      DATA FIRST /.TRUE./
      SAVE eventno
C----------------------------------------------------------------------
      IF(FIRST) THEN              ! only for first event
        CALL MZFORM('ISAE','10I 6F 2D',IOISAE)
        CALL MZFORM('ISAQ','1I-F',IOISAQ)
        CALL MZFORM('ISAJ','1I-F',IOISAJ)
        CALL MZFORM('ISV1','1I-F',IOISV1)   ! format for ISV1
        CALL MZFORM('ISP1','1I-F',IOISP1)   ! format for ISP1
        eventno = 0
        FIRST=.FALSE.
      ENDIF
C
C ****  zero some things
C
      LISAQ = 0
      NJT   = 0
      NQS   = 0
      NPART = 0
      DO I = 1,N
        ITAG(I) = 1
      ENDDO
      IWPOS = 0
      IWNEG = 0
      IZ    = 0
C
C  create head bank
C
      CALL MZWIPE(0)      ! make sure division is wiped before starting
      CALL BKHEAD
C  identify this as a PYTHIA event record
      IQ(LHEAD+1)=1005
      CALL UCTOH('PYTH',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('EVNT',IUH,4,4)
      IQ(LHEAD+3)=IUH
      IQ(LHEAD+6) = 1111   ! Set the run number
      eventno = eventno + 1
      IQ(LHEAD+9)= eventno
      IQ(LHEAD+10)=MSTI(1)
      IQ(LHEAD+14)=1
C
C
C  create ZEBRA bank ISAE (main supporting bank)
C
      CALL MZBOOK(IXMAIN,LISAE,LHEAD,-IZISAE,
     $            'ISAE',9,9,18,IOISAE,-1)
      Q(LISAE+11)=PARI(1)      ! cross section in microbarns
      Q(LISAE+12)=PARI(2)      ! weight
      Q(LISAE+13)=PARI(22)     ! effective q**2
      Q(LISAE+17)=0            ! part 1 of SEED   DUMMIED
      Q(LISAE+18)=0            ! part 2 of SEED   DUMMIED
      IQ(LISAE+1)=0            ! event id         DUMMIED
      IQ(LISAE+2)=0            !   "              DUMMIED
      IQ(LISAE+3)=MSTI(5)      ! event number
      IQ(LISAE+4)=MSTI(1)      ! reaction type
C
C  First make a Zebra bank for primary vertex
C
      CALL INTVTX(XYZ)       ! generate a primary vertex
      CALL VZERO(MOM,3)
      MOM(4)=PARI(11)                 ! ENERGY IN CM
      MOM(5)=PARI(11)
      ID = -1
      NVERTX=1
      CALL BKISV1(NVERTX,LISV1)
      IQ(LISV1-5)=1
      IQ(LISV1+1)=ID
      CALL UCOPY(MOM,Q(LISV1+2),5)
      CALL UCOPY(XYZ,Q(LISV1+7),3)
      LQ(LISV1-2)=0                !  no parent vertex
C
C ****  LOOP OVER ALL PARTICLES TO TAG SECONDARY VERTICES
C
      DO 50 I = 1,N
        IF(K(I,1).EQ.1 .OR. K(I,1).EQ.4) GOTO 50   ! stable 
        IF(K(I,2).EQ.92)                 GOTO 50   ! string
        IF(K(I,2).EQ.91 .OR. K(I,2).EQ.93) GOTO 50 ! cluster or indep
C
C ****  Multiple events?
C
        IF(MSTI(41).GT.1) THEN
C
C ****  This is sloppy here. Check for events with a proton initiating
C ****  a pileup event, but not the first.
C
          IF(I.NE.1 .AND. (K(I,1).EQ.21 .AND. K(I,2).EQ.2212)) THEN
C
C
C  create ZEBRA bank ISAE (main supporting bank)
C
C
C ****  FILL WITH DUMMY INFORMATION JUST TO GET STARTED...
C
            CALL MZBOOK(IXMAIN,LISAE,LHEAD,-IZISAE,
     $            'ISAE',9,9,18,IOISAE,-1)
            Q(LISAE+11)=PARI(1)      ! cross section in microbarns
            Q(LISAE+12)=PARI(2)      ! weight
            Q(LISAE+13)=PARI(22)     ! effective q**2
            Q(LISAE+17)=0            ! part 1 of SEED   DUMMIED
            Q(LISAE+18)=0            ! part 2 of SEED   DUMMIED
            IQ(LISAE+1)=0            ! event id         DUMMIED
            IQ(LISAE+2)=0            !   "              DUMMIED
            IQ(LISAE+3)=MSTI(5)      ! event number
            IQ(LISAE+4)=MSTI(1)      ! reaction type
C
C   Make a ZEBRA bank for pileup vertex
C
            CALL INTVTX(XYZ)       ! generate a primary vertex
            CALL VZERO(MOM,3)
            MOM(4)=PARI(11)                 ! ENERGY IN CM
            MOM(5)=PARI(11)
            ID = -1
            NVERTX=NVERTX + 1
            CALL BKISV1(NVERTX,LISV1)
            IQ(LISV1-5)=1
            IQ(LISV1+1)=ID
            CALL UCOPY(MOM,Q(LISV1+2),5)
            CALL UCOPY(XYZ,Q(LISV1+7),3)
            LQ(LISV1-2)=0                !  no parent vertex
          ENDIF
        ENDIF
C
        IF(K(I,2).EQ.89 .OR. K(I,2).EQ.24) THEN
          IWPOS = I                      ! Tag W+
          GOTO 50
        ELSEIF(K(I,2).EQ.-89 .OR. K(I,2).EQ.-24) THEN
          IWNEG = I                      ! Tag W-
          GOTO 50
        ELSEIF(K(I,2).EQ.23) THEN
          IZ = I                         ! Tag Z0
          GOTO 50
        ENDIF
C
        IF(ITAG(I).NE.1 .AND. K(I,4).NE.0) THEN ! tag granddaughters
          DO 40 DTR = K(I,4),K(I,5)
   40     ITAG(DTR) = ITAG(I)
        ENDIF
        IF(K(I,4).NE.0) THEN            ! any daughters?
          FIRST_VERTEX = .FALSE.
          DO 41 DTR = K(I,4), K(I,5)    ! loop over daughters
            IF(K(DTR,1).EQ.1) THEN      ! any daughters stable?
              FIRST_VERTEX = .TRUE.     ! need to book ISV1
            ENDIF
   41     CONTINUE
          IF(FIRST_VERTEX) THEN         ! book and fill ISV1
C
C ****  Is vertex different from primary vertex?
C       This is a temporary situation, until it is decided how much of the
C       event history we want to keep.
C
            IF((V(I,1).NE.V(1,1)) .OR. (V(I,2).NE.V(1,2)) .OR.
     &         (V(I,3).NE.V(1,3))) THEN
              IV = ITAG(I)
              LVP = LVD(IV)
              NVERTX = NVERTX + 1
              CALL BKISV1(NVERTX,LISV1)
              CALL PDG2IS(K(I,2),ID)
              IQ(LISV1+1) = ID
              DO J = 1,5
                Q(LISV1+1+J) = P(I,J)     ! momenta and mass
              ENDDO
              DO J = 1,3
                Q(LISV1+6+J) = V(I,J)/10. ! vertex in cm
              ENDDO
              Q(LISV1+9) = Q(LISV1+9) + XYZ(3,1) ! Z of interaction
              LQ(LISV1-2)=LVP             ! reference link to parent vertex
              DO 42 DTR = K(I,4), K(I,5)  ! tag daughters
                ITAG(DTR) = NVERTX
   42         CONTINUE
            ENDIF
          ENDIF
        ENDIF
   50 CONTINUE
C
C ****  LOOP OVER PARTICLES TO CREATE PARTICLE BANKS
C
C
C ****  NEED TO CREATE ISAJ AND ISAQ LINKS HERE...
C
      DO 500 I = 1,N                    ! LOOP OVER EVENT COMMON
        IF(K(I,1).EQ.21) GOTO  500      ! REMOVE DOCUMENTATION LINES
C
C ****   add t to ISAJ
C
        IF(ABS(K(I,2)).EQ.6) THEN
          NJT = NJT + 1
          CALL BKISAJ(LISAJ)
          ID = K(I,2)
          IQ(LISAJ+1)=ID                  ! particle id
          DO J = 1,5
            MOM(J) = P(I,J)
            Q(LISAJ+1+J) = P(I,J)         ! momenta and mass
          ENDDO
          CALL ETOETA(MOM,PHI,THETA,ETA)
          Q(LISAJ+7)=PHI
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
C   set pointers
C              PQREF(NJT)=LISAJ
C              JOR=IABS(IORIG(I))
C              JET=JOR/IPACK
C              LQ(LISAJ-1)=PQREF(JET)
C              IORIG(I)=-NJT*IPACK          ! remake ORIG to point to ISAJ
        ENDIF
C
C ****   add b to ISAJ
C
        IF(ABS(K(I,2)).EQ.5) THEN
          NJT = NJT + 1
          CALL BKISAJ(LISAJ)
          ID = K(I,2)
          IQ(LISAJ+1)=ID                  ! particle id
          DO J = 1,5
            MOM(J) = P(I,J)
            Q(LISAJ+1+J) = P(I,J)         ! momenta and mass
          ENDDO
          CALL ETOETA(MOM,PHI,THETA,ETA)
          Q(LISAJ+7)=PHI
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
C   set pointers
C              PQREF(NJT)=LISAJ
C              JOR=IABS(IORIG(I))
C              JET=JOR/IPACK
C              LQ(LISAJ-1)=PQREF(JET)
C              IORIG(I)=-NJT*IPACK          ! remake ORIG to point to ISAJ
        ENDIF
C
C ****   add W to ISAJ
C
        IF(I.EQ.IWPOS.OR.I.EQ.IWNEG) THEN
          NJT = NJT + 1
          CALL BKISAJ(LISAJ)
          ID = 80
          IF(I.EQ.IWNEG) ID = -80
          IQ(LISAJ+1)=ID                  ! particle id
          DO J = 1,5
            MOM(J) = P(I,J)
            Q(LISAJ+1+J) = P(I,J)         ! momenta and mass
          ENDDO
          CALL ETOETA(MOM,PHI,THETA,ETA)
          Q(LISAJ+7)=PHI
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
C   set pointers
C              PQREF(NJT)=LISAJ
C              JOR=IABS(IORIG(I))
C              JET=JOR/IPACK
C              LQ(LISAJ-1)=PQREF(JET)
C              IORIG(I)=-NJT*IPACK          ! remake ORIG to point to ISAJ
        ENDIF
C
C ****  fill in ISAQ    (light quarks or gluons)
C
        IF(ABS(K(I,2)).LT.5 .OR. K(I,2) .EQ. 21) THEN
          CALL BKISAQ(LISAQ)
          NQS=NQS+1
          ID = K(I,2)
          IF(K(I,2).EQ.21) ID=9   ! gluon
          IQ(LISAQ+1)=ID                  ! particle id
          DO J = 1,5
            MOM(J) = P(I,J)
            Q(LISAQ+1+J) = P(I,J)         ! momenta and mass
          ENDDO
          CALL ETOETA(MOM,PHI,THETA,ETA)
          Q(LISAQ+7)=PHI
          Q(LISAQ+8)=THETA
          Q(LISAQ+9)=ETA
C
        ENDIF
C
C ****  fill in ISAQ    (primary leptons from W+/- or Z)
C
        IF(ABS(K(I,2)).GE.11 .AND. ABS(K(I,2)) .LE. 16) THEN
          PNTR = K(I,3)
          IF(PNTR.EQ.IWPOS .OR. PNTR.EQ.IWNEG .OR. PNTR.EQ.IZ) THEN
            CALL BKISAQ(LISAQ)
            NQS=NQS+1
            CALL PDG2IS(K(I,2),ID)
            IQ(LISAQ+1)=ID                  ! particle id
            DO J = 1,5
              MOM(J) = P(I,J)
              Q(LISAQ+1+J) = P(I,J)         ! momenta and mass
            ENDDO
            CALL ETOETA(MOM,PHI,THETA,ETA)
            Q(LISAQ+7)=PHI
            Q(LISAQ+8)=THETA
            Q(LISAQ+9)=ETA
          ENDIF
C
        ENDIF
C
        IF(K(I,1).EQ.1 .OR. K(I,1).EQ.4) THEN   ! final state particle
C
C ****  NO NEUTRINOS
C
          IF(K(I,2).NE.12 .AND. K(I,2).NE.14 .AND. K(I,2).NE.16) THEN
            IV=ITAG(I)
            LISV1=LVD(IV)
            CALL MZBOOK(IXMAIN,LISP1,LISV1,-IZISP1,
     $                  'ISP1',5,1,9,IOISP1,-1)
C              JOR=IABS(IORIG(I))
C   set reference link to parent parton
C              JP=MOD(JOR,IPACK)
C              IF(IORIG(I).GT.0) THEN   ! trace back ancestry if needed
C   91           JOR=IABS(IORIG(JP))
C                IF(IORIG(JP).GT.0) THEN
C                  JP=MOD(JOR,IPACK)
C                  GOTO 91
C                ENDIF
C                JP=MOD(JOR,IPACK)
C              ENDIF
C              IF(JP.GT.0) LQ(LISP1-2)=QREF(JP)
C              JET=JOR/IPACK
C              LQ(LISP1-3)=PQREF(JET)
C
C   fill Zebra bank
C
            NPART=NPART+1
            CALL PDG2IS(K(I,2),ID)
            IQ(LISP1-5)=NPART              ! impose part. no.
            IQ(LISP1+1)=ID                 ! particle id
            DO J = 1,5
              MOM(J) = P(I,J)
              Q(LISP1+1+J) = P(I,J)         ! momenta and mass
            ENDDO
            CALL ETOETA(MOM,PHI,THETA,ETA)
            Q(LISP1+7) = PHI
            Q(LISP1+8) = THETA
            Q(LISP1+9) = ETA
C
          ENDIF
        ENDIF
  500 CONTINUE
C
C   renumber vertices in sequence
C
      NVERTX=0
      LISV1=LVD(1)
      DO WHILE(LISV1.NE.0)
        NVERTX=NVERTX+1
        IQ(LISV1-5)=NVERTX        ! impose a vertex number
        LISV1=LQ(LISV1)
      ENDDO
C
C ****  fill PJET
C
      CALL PJETFL
      LPJHD=GZPJHD()
      NPJET=IQ(LPJHD+3)
C
C ****  fill in summaries in ISAE
C
      LISAE=LQ(LHEAD-IZISAE)
      IQ(LISAE+5)=NJT               ! number of primary parton banks
      IQ(LISAE+6)=NQS               !  "     of stable parton banks
      IQ(LISAE+7)=NPJET             !  "     of PJET banks
      IQ(LISAE+8)=NPART             !  "     of particle banks
      IQ(LISAE+9)=NVERTX            !  "     of vertex banks
C      IQ(LISAE+10)=NLEP             !  "     of lepton banks
C
  999 RETURN
      END
