      SUBROUTINE ISAMFL(NMUONS)
C-------------------------------------------------------------------
C-
C-      Fill Zebra banks  ISAM, fake muon bank
C-      leptons with reference links to ISP1, ISAQ, ISAJ
C-      Needs banks ISP1 and ISV1
C-
C-      OUTPUT:
C-      NMUONS = number of muons
C-
C-               D. Hedin November 1, 1990
C-   updated for upgrade DH 4/91
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAM.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LISAE,LISV1,LISP1,LISAM,IOISAM,JBIAS,ID1
      INTEGER NMUONS,ISOURCE,ITRIG,ID,IPUN,I,IDEC
      LOGICAL FIRST
      REAL ETAMAX,PMU,XAIR,ABS1,ABS2,DE1,DE2,PROP,PROC,
     A PX,PY,PZ,PXO,PYO,PZO,THET,PTOT,ANGMU,EMU,PRODEC
      DATA ETAMAX/4.0/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        CALL MZFORM('ISAM','4I -F',IOISAM)
        FIRST=.FALSE.
      ENDIF
C
C        set pointers for first muon bank
C
      LISAE=LQ(LHEAD-IZISAE)
      JBIAS=-IZISAM
      LISAM=LISAE
      NMUONS=0
C
C          Find muons; keep only those who make it through the iron
C
      LISV1=LISAE-IZISV1
C
  100 LISV1=LQ(LISV1)        ! loop over vertices
      IF(LISV1.GT.0) THEN
        LISP1=LISV1-IZISP1
C
  200   LISP1=LQ(LISP1)      ! loop over particles
        IF(LISP1.GT.0) THEN
CC  MAKE MAXIMUM ETA CUT
          IF(ABS(Q(LISP1+9)).LT.ETAMAX) THEN
            THET=Q(LISP1+8)*180./3.14159
            IF(THET.GT.90.) THET=180.-THET
CC
CCCC    DO SIMPLE GEOMETRY
CC
            CALL ISAMPROJ(Q(LISP1+8),Q(LISP1+7),XAIR,ABS1,ABS2,DE1,DE2)
            ID1=IQ(LISP1+1)
            ID=0
            PTOT=SQRT(Q(LISP1+2)**2+Q(LISP1+3)**2+Q(LISP1+4)**2)
            IF(IABS(ID1).EQ.14) THEN     ! PROMPT MUONS
              ID=IQ(LISP1+1)
              ISOURCE=0
              PMU=PTOT
              IF(PMU.GE.DE1.AND.PMU.LT.DE2) ISOURCE=10
              IF(PMU.LT.DE1) ISOURCE=20
            ELSE
              CALL ISAMDECAY(ID1,XAIR,PTOT,Q(LISP1+5),IDEC,PRODEC,EMU,
     A                                                         ANGMU)
              IF(IDEC.EQ.1) THEN    ! HAVE DECAY
                ID=-14*IQ(LISP1+1)/IABS(IQ(LISP1+1))    ! NOT QUITE RIGHT
                ISOURCE=1
                PMU=EMU
                IF(PMU.GE.DE1.AND.PMU.LT.DE2) ISOURCE=11
                IF(PMU.LT.DE1) ID=0
              ELSE                       ! SEE IF PUNCH IN BC
                IPUN=0
                IF(PTOT.GT.1.5*DE2) 
     C           CALL ISAMPUNCH(ID1,PTOT,ABS2,IPUN,PROP,PROC,EMU)
                IF(IPUN.EQ.1) THEN
                  ID=ID1
                  ISOURCE=2
                  PMU=EMU
                  IF(PMU.LT.DE2) PMU=DE2
                ELSE                    ! SEE IF PUNCH IN A
                  IF(PTOT.GT.1.5*DE1)
     C             CALL ISAMPUNCH(ID1,PTOT,ABS1,IPUN,PROP,PROC,EMU)
                  IF(IPUN.EQ.1) THEN
                    ID=ID1
                    ISOURCE=12
                    PMU=EMU
                    IF(PMU.LT.DE1) PMU=DE1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
C
              IF(ID.NE.0) THEN                     ! BOOK ISAM BANK
CCC   SEE ABOUT TRIGGER EFFICIENCY
                ITRIG=0
                IF(ISOURCE.LT.9)
     A              CALL ISAMTREFF(Q(LISP1+8),Q(LISP1+7),PMU,ITRIG)
CCC
CCC   ADD RESOLUTION TO MUON
                PX=Q(LISP1+2)*PMU/Q(LISP1+5)
                PY=Q(LISP1+3)*PMU/Q(LISP1+5)
                PZ=Q(LISP1+4)*PMU/Q(LISP1+5)
                IF(ISOURCE.LT.9) THEN
                  CALL ISAMRES(PX,PY,PZ,PXO,PYO,PZO,PMU)
                ELSE IF(ISOURCE.GE.10.AND.ISOURCE.LT.19) THEN
                  PMU=DE1+DE2/2.      ! RANGES OUT IN IRON
                  PXO=Q(LISP1+2)*PMU/Q(LISP1+5)
                  PYO=Q(LISP1+3)*PMU/Q(LISP1+5)
                  PZO=Q(LISP1+4)*PMU/Q(LISP1+5)
                ELSE IF(ISOURCE.GE.20) THEN
                  PMU=DE1*.8      ! RANGES OUT IN CALOR
                  PXO=Q(LISP1+2)*PMU/Q(LISP1+5)
                  PYO=Q(LISP1+3)*PMU/Q(LISP1+5)
                  PZO=Q(LISP1+4)*PMU/Q(LISP1+5)
                ENDIF
                CALL MZBOOK(IXMAIN,LISAM,LISAM,JBIAS,
     $               'ISAM',4,1,12,IOISAM,-1)
                NMUONS=NMUONS+1
                IQ(LISAM+1)=ID           ! ID
                IQ(LISAM+2)=ISOURCE      ! SOURCE
                IQ(LISAM+3)=ITRIG        ! TRIGGER
                IQ(LISAM+4)=IQ(LISV1+1)            ! PARENT ID
                Q(LISAM+5)=PXO
                Q(LISAM+6)=PYO
                Q(LISAM+7)=PZO
                Q(LISAM+8)=PMU
                Q(LISAM+9)=Q(LISP1+7)    ! PHI
                Q(LISAM+10)=Q(LISP1+8)   ! THETA
                Q(LISAM+11)=Q(LISP1+9)   ! ETA
                Q(LISAM+12)=0             ! SPARE
                LQ(LISAM-4)=LISP1         ! reference link to ISP1
                LQ(LISAM-2)=LQ(LISP1-2)   ! reference link to ISAQ
                LQ(LISAM-3)=LQ(LISP1-3)   ! reference link to ISAJ
               JBIAS=0
              ENDIF
C
          ENDIF
          GOTO 200
        ENDIF
C
        GOTO 100
      ENDIF
      RETURN
      END
