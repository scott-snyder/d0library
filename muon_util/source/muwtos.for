      SUBROUTINE MUWTOS(ITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : project a WAMUS track to SAMUS, add SAMUS
C-   hits, refit and update MUOT bank
C-
C-   Inputs  : ITRAK - track number
C-   Outputs : updated MUOT bank
C-   Controls:
C-
C-   Created  20-JUN-1991   Daria Zieminska
C-   Updated   1-APR-1992   Daria Zieminska  check if track passes through
C-                          SAMUS A station
C-   Updated  10-NOV-1992   Daria Zieminska  add 10 to MUOT+4
C    DH 1/93 require at least 3 NA hits
C    TD 1/93                  4
C    DZ 1/93 DIFMAX = 10.,5.
C    DF 1/93 MAKE DIFMAX AN RCP PARAMETER; REQUIRE AT LEAST 5 PLANES IN SA
C               FOR WS; NEW REQUIREMENTS FOR SAMUS HIT TAGGING
C    DF 4/93 WIDEN SAMUS VOLUME; ONLY PUT SAMUS POINTS ON TRACK IF
C       OVERLAP TRACKING DONE; SET IFW1=10 ONLY IF OVERLAP TRACK
C    DH 4/93 use MUOT flag word 3; fix inadverdent SAMUS hit bug
C    DH 4/93 different roads for X,Y,U
C    DH 12/93 eliminate SAPTST loss if FALSE; recall MTGOOD
C     DH 11/94 remove call to MUFITBC due to SAMUS redefinitions
C    Updated  07-FEB-1995 Igor Mandrichenko   New format of STTH
C    DH 3/95 remove MUFITWS call
C-   Updated   9-AUG-1995   Andrei Mayorov   use SAMUS triplets instead of
C-                                           clusters
C    DH 9/95   modify IFW3
C    AS 11/96 Remove loop over all stations and increase max # hits to 40. 
C               Now loops over 1st Station(N or S)
C   
C              
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
      INTEGER NSAM,ISAM(3),IPLANE,LMUOT,GZMUOT
      INTEGER N_DIR,I,II,JBEND,JNBEND
      INTEGER ITRAK,NPTRAK,NSAMUS,IFW1,IFW2,IFW3,QUAD,ISPARE
      INTEGER NHIT,K
      INTEGER IER
      REAL XMAGC,YMAGC,ZMAGC,XI,YI,ZI,SPARE1,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,THETA
      REAL CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE
      REAL PT(6),DIF(3),DIFMAX(2),FIT(4),DIFNB(3)
      REAL XYUFAC(3),XYUF(3)
      REAL GEOM(6,40),DRFT(40)
      INTEGER IADD(40),NHITS,IHIT
      LOGICAL IFL,SAPTST,FIRST
      INTEGER SAGTCT,ISTAT,IPL,NSAHT,N,N3P,IND
      INTEGER LSAHS,GZSAHS,LSAHT,GZSAHT,LSAMT,GZSAMT
      INTEGER LHIT,NW,NB,STATION,SECTION,TUBE,PM,LSSTG,JJ
      LOGICAL OK
      REAL WBEND,WNON,W1,W2,DIST(2)
      PARAMETER (WBEND=0.85, WNON=0.15)
      DATA FIRST/.TRUE./
      DATA XYUFAC/1.,3.,2.1/   ! multiply road for good/bad/U
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL   EZGET('DIFMAX_A',DIFMAX(1),IER)
        IF (IER.NE.0) DIFMAX(1) = 10.   ! DEFAULT
        CALL EZGET('DIFMAX_BC',DIFMAX(2),IER)
        IF (IER.NE.0) DIFMAX(2) = 5.    ! DEFAULT
      END IF
C
      CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X  XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,SPARE2)
      IF (QUAD.LE.4) GO TO 999
      THETA=ACOS(ZCOSIM)
C
      IF (THETA.GT.0.462.AND.THETA.LT.2.68) GO TO 999 ! outside SAMUS volume
      IF (THETA.LT.0.001) THEN     ! protection (it should not happen)
        GO TO 999
      END IF
      DO I=1,3
        XYUF(I)=XYUFAC(I)
      ENDDO
      W1=WBEND
      W2=WNON
      IF(MOD(QUAD,2).EQ.0) THEN    ! Y=GOOD WAMUS DIRECTION
        XYUF(1)=XYUFAC(2)
        XYUF(2)=XYUFAC(1)
        W1=WNON
        W2=WBEND
      ENDIF
      IF (QUAD.GT.4.AND.QUAD.LT.9) THEN
        N_DIR=1
      ELSE
        N_DIR=2
      END IF
      FIT(1)=XCOSIM/ZCOSIM
      FIT(2)=YCOSIM/ZCOSIM
      FIT(3)=XMAGC-FIT(1)*ZMAGC
      FIT(4)=YMAGC-FIT(2)*ZMAGC
      IF (.NOT.SAPTST(N_DIR,FIT,IFL)) THEN
      ELSE
        IF (.not.IFL) THEN
C
C  track does not pass through SAMUS A station; don't change the status
C
          GO TO 999
        END IF
      END IF
      LMUOT=GZMUOT(ITRAK)
      LSAMT = GZSAMT ()
      IF (LSAMT .EQ. 0) THEN
        CALL BKSAMT (LSAMT)
        CALL SAHTFL (OK)
        IF (.NOT. OK) THEN                       ! no hits
          CALL MZDROP (IXCOM, LSAMT, ' ')
          GO TO 999
        END IF
      END IF
      NSAM=0
      LSAHS=GZSAHS()
C      DO 100 I=1,3  ! search for SAMUS triplets along the track
      DO 100 I=1,1  ! search for SAMUS triplets along the track
        STATION=(N_DIR-1)*3+I
        DO IPL = 1,3
          IND = 3*(STATION-1) + IPL
          N = IQ(LSAHS+IND)
          IF (N .EQ. 0) GO TO 100
        END DO
        CALL SATG1A(STATION,N3P)
        IF(N3P.GT.0) THEN
          IF (I.LE.1) THEN
            PT(1)=XI
            PT(2)=YI
            PT(3)=ZI
            PT(4)=XCOSIM
            PT(5)=YCOSIM
            PT(6)=ZCOSIM
            K=1
          ELSE
            PT(1)=XMAGC
            PT(2)=YMAGC
            PT(3)=ZMAGC
            PT(4)=XCOSOM
            PT(5)=YCOSOM
            PT(6)=ZCOSOM
            K=2
          END IF
          DIF(I)=1000.
          ISAM(I)=0
          IF(N_DIR.EQ.1) THEN
            II=I
          ELSE
            II=I+3
          ENDIF
          NHIT=SAGTCT(II,PT,W1,W2,DIF(I),DIST)  ! get nearest triplet; -1 if no
          IF (NHIT.GT.0) THEN
            IF (DIST(1).LT.DIFMAX(K)*XYUF(1).AND.
     A          DIST(2).LT.DIFMAX(K)*XYUF(2)) THEN
              ISAM(I)=NHIT
              DIF(I)=DIST(1)
              DIFNB(I)=DIST(2)
              IF(MOD(QUAD,2).EQ.0) DIF(I)=DIST(2)
              IF(MOD(QUAD,2).EQ.0) DIFNB(I)=DIST(1)
              NSAM=NSAM+1
            END IF
          END IF
        END IF
  100 CONTINUE
C
      IF (ISAM(1).EQ.0) THEN
        GO TO 999  ! no hits in A station
      END IF
      IF (NSAM.GT.0) THEN         ! track has hits in SAMUS
C
C  Book and fill STTH
C
        LSAHS = GZSAHS()
        NHITS = 0
C        DO ISTAT=1,3
        DO ISTAT=1,1
          IF(ISAM(ISTAT).NE.0) THEN
            DO IPLANE=1,3
              IPL=9*(N_DIR-1)+3*(ISTAT-1)+IPLANE
              NSAHT=IQ(LSAHS+IPL)
              IF(NSAHT.GT.0) THEN
                LSAHT=GZSAHT(IPL)
                DO IHIT=1,NSAHT
                  LHIT=LSAHT+13*(IHIT-1)
                  NW = (ISAM(ISTAT) - 1) / 32
                  NB = ISAM(ISTAT) - NW * 32-1
                  IF (BTEST(IQ(LHIT+NW+5),NB)) THEN ! hit belongs to the given
                    NHITS=NHITS+1                    !tripl.
                    IF (NHITS.GT.40) THEN
                      NHITS=NHITS-1
                      GOTO 200
                    ENDIF
                    STATION=IBITS(IQ(LHIT+2),0,5)
                    SECTION=IBITS(IQ(LHIT+2),5,5)
                    TUBE=IBITS(IQ(LHIT+2),16,16)
                    CALL SATOPM(STATION,SECTION,PM)
                    IADD(NHITS)=PM*256+TUBE
                    DRFT(NHITS)=Q(LHIT+4)
                    LSSTG=IQ(LHIT+3)
                    DO JJ=1,6
                      GEOM(JJ,NHITS)=C(LSSTG+JJ)
                    END DO
                  END IF
                END DO
              END IF
            END DO
          END IF
        ENDDO
 200    CONTINUE
        CALL STTHFL(ITRAK,NHITS,IADD,GEOM,DRFT)

C
C  Refill MUOT bank
C
        LMUOT = GZMUOT(ITRAK)
        IQ(LMUOT+2) = NHITS
        IF (ISAM(1).GE.1) IQ(LMUOT+4)=10   ! 3-layer track, vertex used in fit
        CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X    YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     X    SPARE2)
CC    redo IFW2 determination
        CALL MTGOOD(QUAD,MOM,XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,
     X    ZCOSIM,XCOSOM,YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOMER,SPARE2,
     X    IFW1,IFW2)
        IQ(LMUOT+5)=IFW2
        JBEND=ABS(DIF(1))*10.+.5
        JNBEND=ABS(DIFNB(1))+.5
        IF(JBEND.GT.250) JBEND=250
        IF(JNBEND.GT.99) JNBEND=99
        IQ(LMUOT+6)=JBEND*100+JNBEND
C       WRITE (10,101) iq(lhead+9),quad,nhits,ifw2,DIF(1),difnb(1)
C  101 FORMAT(' MUWTOS quad,nhits,ifw2,DIF,difnb ',4i6,2f8.1)
      END IF    ! IF (NSAM.GT.0)
C
  999 RETURN
      END
