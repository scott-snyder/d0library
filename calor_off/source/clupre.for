      SUBROUTINE CLUPRE(MXPREC,NPRECL,PRECLU,IPRECL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FORM PRECLUSTERS FOR JET CLUSTER ALGORITHM 
C-
C-   Inputs  : TOWERS
C-   Outputs : PRECLUSTERS
C-
C-   Created  22-MAR-1989   Nicholas J. Hadley
C-   Updated   2-OCT-1990   Chip Stewart  - MODIFIED RCP INPUT 
C-   Updated   8-MAR-1994   Chip Stewart  - ET OF SEED IN PRECLU(9,NPRECL) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C
      REAL    EPRSED,EPRCAN,EPRMIN
C
      INTEGER RINGMX, RINTMP, RINX
      PARAMETER( RINGMX = 6 )
      PARAMETER( RINTMP = RINGMX-1 )
      PARAMETER( RINX = 8*RINTMP )
      INTEGER RING(RINGMX), RINGAD(RINGMX,RINX,2)
      INTEGER NPRECL, NPRECE, NPRECH, MXPREC
      INTEGER IPRECL(10,MXPREC)
      REAL PRECLU(10,MXPREC)
C
c     NPRECL = NUMBER OF PRECLUSTERS
C     MXPREC = MAXIMUM NUMBER OF PRECLUSTERS
C
C     PRECLU(1,I)  = ETA OF ITH PRECLUSTER CENTROID
C     PRECLU(2,I)  = PHI OF ITH PRECLUSTER CENTROID
C     PRECLU(3,I)  = EX OF ITH PRECLUSTER
C     PRECLU(4,I)  = EY OF ITH PRECLUSTER
C     PRECLU(5,I)  = EZ OF ITH PRECLUSTER
C     PRECLU(6,I)  = ETOT OF ITH PRECLUSTER
C     PRECLU(7,I)  = ET OF ITH PRECLUSTER
C     PRECLU(8,I)  = EM ET IN PRECLUSTER
C     PRECLU(9,I)  = ET OF SEED
C     IPRECL(10,I)  = NUMBER OF TOWERS IN PRECLUSTER
C     IPRECL(11,I)  = INDEX OF FIRST TOWER, SEE BELOW FOR UNPACKING
C     IPRECL(12,I) = INDEX OF SECOND TOWER
C      AND SO ON FOR ALL THE TOWERS
C         ETA = INT(IPRECL(11,I)/64.) -(NETAL+1)
C         PHI = MOD(IPRECL(11,I),64)
C
      INCLUDE 'D0$INC:PTCATE.INC'
      INTEGER I,J,K,L,LL, NTOW, LCATE, NTOTOW, NEMTOW
      INTEGER IPHI,IETA, IETAIN,IPHIIN, IETATP, IPHITP, IETAZE
      INTEGER NREP, POINT, PTEM, PTTP, PTIN, GZCATE
      REAL    ETIN, ETTMP
C
      INTEGER IEPRSED,IEPRCAN,IEPRMIN
      EQUIVALENCE ( IEPRSED , EPRSED  )
      EQUIVALENCE ( IEPRCAN , EPRCAN  )
      EQUIVALENCE ( IEPRMIN , EPRMIN  )

      LOGICAL FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST ) THEN
        FIRST = .FALSE.
        RING(1) = 1
        DO 10 I = 2, RINGMX
          RING(I) = 8*(I-1)
   10   CONTINUE
        DO 60 I = 1, RINGMX
          J = 1
          DO 20 K = 0 , 2*(I-1)
            RINGAD(I,J,1) = -(I-1)
            RINGAD(I,J,2) = -(I-1) + K
            J = J + 1
   20     CONTINUE
          DO 30 K = 1, 2*(I-1)
            RINGAD(I,J,1) = -(I-1) + K
            RINGAD(I,J,2) =  (I-1)
            J = J + 1
   30     CONTINUE
          DO 40 K =1 , 2*(I-1)
            RINGAD(I,J,1) =  (I-1)
            RINGAD(I,J,2) =  (I-1) - K
            J = J + 1
   40     CONTINUE
          DO 50 K =1 , 2*(I-1) - 1
            RINGAD(I,J,1) =  (I-1) - K
            RINGAD(I,J,2) =  -(I-1)
            J = J + 1
   50     CONTINUE
   60   CONTINUE
      ENDIF
C
C
      NPRECL = 0
      NPRECH = 0
      NPRECE = 0
      DO 62 I = 1 ,MXPREC  
        DO 61 J =1  ,10
          PRECLU(J,I) = 0.
   61   CONTINUE
   62 CONTINUE
C
C
C       ZERO THE CLUSTER NUMBER SO THAT IT CAN BE USED BY CLUSTERING
C
      LCATE = GZCATE()
      NTOTOW = MOD(IQ(LCATE+3),CATENM)
      NEMTOW = INT(IQ(LCATE+3)/CATENM)
      NREP = IQ(LCATE+2)
      DO 90 NTOW = NEMTOW+1, NTOTOW
        POINT = LCATE + NREP*(NTOW-1)
        IQ(POINT+16) = 0
   90 CONTINUE
C
C
      DO 700 NTOW = NEMTOW+1,NTOTOW
        POINT = LCATE + NREP*(NTOW-1)
        IPHI = IQ(POINT+13)
        IETA = IQ(POINT+12)
C                 OK FOR SEED TOWER
        IF (Q(POINT+8) .LT. EPRSED) GOTO 710
        IF (IQ(POINT+16) .NE. 0) GOTO 700
C
        NPRECL = NPRECL + 1
        IF (NPRECL.GT.MXPREC) GOTO 710  !TOO MANY PRECLUSTERS
        IQ(POINT+16) = 1000*NPRECL
        IPRECL(10,NPRECL) = 1
C***        IPRECL(11,NPRECL) = (IETA+NETAL+1)*64 + IPHI
        PRECLU(1,NPRECL) = FLOAT(IETA)
        PRECLU(2,NPRECL) = FLOAT(IPHI)
        DO 75 K = 1 ,5
          PRECLU(2+K,NPRECL) = Q(POINT+3+K)
   75   CONTINUE
        PTEM = PTCATE(IETA,IPHI,1)
        IF (PTEM.NE.0) THEN
          PTEM = NREP*(PTEM-1) + LCATE
          PRECLU(8,NPRECL) = Q(PTEM+8) 
        END IF
C
C ****  STICK ET OF SEED TOWER IN PRECLU WORD 9 (OVERWRITE CACL SIG**2(EX)
C
        PRECLU(9,NPRECL) = Q(POINT+8)    ! SAVE SEED 
C
        DO 150 K = 1 , 4
          DO 140 L = 1 , RING(K)
            IETAIN = RINGAD(K,L,1) + IETA
            IF (IETAIN .LT. -NETAL .OR. IETAIN .GT. NETAL) GOTO 140
            IPHIIN = RINGAD(K,L,2) + IPHI
            IF (IPHIIN.GT.NPHIL) IPHIIN = IPHIIN - NPHIL
            IF (IPHIIN.LE.0) IPHIIN = IPHIIN + NPHIL
            IETAZE = IETAIN
            IF (IETAZE.EQ.0) THEN
              IETAZE = 1
              IF(RINGAD(K,L,1).LT.0) IETAZE = -1
            END IF
            PTIN = PTCATE(IETAZE,IPHIIN,2)
            IF (PTIN.EQ.0) GOTO 140
            PTIN = NREP*(PTIN-1) + LCATE
            IF(IQ(PTIN+16).EQ.0) GOTO 140
            ETIN = Q(PTIN+8)
            DO 80 I = -1 ,1  
              IETATP = IETAIN + I
              IF (IETATP .LT. -NETAL .OR. IETATP .GT. NETAL) GOTO 80
              DO 70 J = -1 ,1
                IF(I.EQ.0 .AND. J.EQ.0) GOTO 70
                IPHITP = IPHIIN + J
                IF(ABS(IETATP-IETA).LT.(K-1) .AND. 
     &                ABS(IPHITP-IPHI).LT.(K-1))  GOTO 70
                IF (IPHITP.GE.NPHIL+1) IPHITP = IPHITP - NPHIL
                IF (IPHITP.LE.0) IPHITP = IPHITP + NPHIL
                IF (IETATP.EQ.0) THEN
                  IETATP = I
                  IF(IETATP.EQ.0) IETATP=IETAZE
                END IF
                IF(PTCATE(IETATP,IPHITP,2).EQ.0) GOTO 70
                PTTP = NREP*(PTCATE(IETATP,IPHITP,2)-1) + LCATE
                IF (IQ(PTTP+16).NE.0) GOTO 70
                ETTMP = Q(PTTP+8)
                IF (ETTMP.LT.EPRCAN) GOTO 70
                IF (ETTMP.GT.2.*ETIN) GOTO 70
C
C                   ADD TO TOWERS IN PRECLUSTER
C
                IQ(PTTP+16) = NPRECL*1000
                IPRECL(10,NPRECL) = IPRECL(10,NPRECL) + 1
C***                ITMP = 10 + IPRECL(10,NPRECL)
C***                IPRECL(ITMP,NPRECL) = (IETATP+NETAL+1)*64 + IPHITP
                DO 65 LL = 1 , 5 
                  PRECLU(2+LL,NPRECL) = PRECLU(2+LL,NPRECL) +
     &                                  Q(PTTP+3+LL)
   65           CONTINUE
                PTEM = PTCATE(IETATP,IPHITP,1)
                IF (PTEM.NE.0) THEN
                  PTEM = NREP*(PTCATE(IETATP,IPHITP,1)-1) + LCATE  
                  PRECLU(8,NPRECL) = PRECLU(8,NPRECL) +  Q(PTEM+8)
                END IF
   70         CONTINUE
   80       CONTINUE
  140     CONTINUE
  150   CONTINUE
C
C        IF NOT ENOUGH ET IN PRECLUSTER REMOVE IT
C
        IF (PRECLU(7,NPRECL).LT.EPRMIN) THEN
          DO 170 K = 1 ,10
            PRECLU(K,NPRECL) = 0.
  170     CONTINUE
          NPRECL = NPRECL - 1
        END IF
C
C
  700 CONTINUE
  710 CONTINUE
C
C       ZERO THE CLUSTER NUMBER SO THAT IT CAN BE USED BY CLUSTERING
C
      DO 800 NTOW = NEMTOW+1, NTOTOW
        POINT = LCATE + NREP*(NTOW-1)
        IQ(POINT+16) = 0
  800 CONTINUE
C
C      WRITE(55,*) ' NUMBER OF PRECLUSTERS', NPRECL
C      DO I = 1, NPRECL
C        WRITE(55,*) ' CLUSTER NUMBER', I
C        DO J = 1, 9
C          WRITE(55,*) ' PRECLU(J,I)', PRECLU(J,I)
C        END DO
C        WRITE(55,*) ' NUMBER OF TOWERS', IPRECL(10,I)
C        DO J = 1,IPRECL(10,I)
C          IPHI = MOD(IPRECL(10+J,I),64)
C          IETA = INT(IPRECL(10+J,I)/64) - NETAL - 1
C          WRITE(55,*) ' ETA, PHI', IETA, IPHI
C        END DO
C      END DO
C
C
  999 RETURN
C
      ENTRY CLUPRI
C
C                 get constants out of CAJETS.RCP
C
      IEPRSED = ALG_PARAMS(6)
      IEPRCAN = ALG_PARAMS(7)
      IEPRMIN = ALG_PARAMS(8)
      FIRST = .TRUE.
C
      RETURN
C
      END
