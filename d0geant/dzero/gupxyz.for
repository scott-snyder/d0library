      SUBROUTINE GUPXYZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print track and volume parameters
C-                         at current point
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-JUL-1989   Rajendran Raja
C-                          BASED ON GEANT CODE GPCXYZ
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCVOLU.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCNUM.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCMATE.INC'
      INCLUDE 'D0$INC:GCMAIL.INC'
C
      DIMENSION MECNAM(20)
      CHARACTER*4 KUN1,KUN2
      INTEGER IEVOLD,NTMOLD,NM,MECNAM,I,MEC
      REAL    R,DESU,GEKU,TOFGN
      DATA IEVOLD,NTMOLD/-1,-1/
C----------------------------------------------------------------------
C
C ****
C
      IF(IFINIT(9).EQ.0)THEN
        IFINIT(9)=1
        IEVOLD=-1
        NTMOLD=-1
      ENDIF
C
      NM=NMEC
      IF(NM.EQ.0)THEN
        MECNAM(1)=NAMEC(29)
        NM=1
      ELSE
        DO 10 I=1,NMEC
          MEC=LMEC(I)
          MECNAM(I)=NAMEC(MEC)
   10   CONTINUE
      ENDIF
C
      IF(IEVENT.EQ.IEVOLD.AND.NTMULT.EQ.NTMOLD)GO TO 20
C
C
      TOFGN=TOFG*1.E+9
      WRITE(CHMAIL,1000)ITRA,ISTAK,NTMULT,(NAPART(I),I=1,5),TOFGN
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,1200)
      CALL GMAIL(0,0)
      IEVOLD=IEVENT
      NTMOLD=NTMULT
C
   20 R=SQRT(VECT(1)**2+VECT(2)**2)
      CALL GEVKEV(DESTEP,DESU,KUN1)
      CALL GEVKEV(GEKIN ,GEKU,KUN2)
      WRITE(CHMAIL,2000)(VECT(I),I=1,3),R,NAMES(NLEVEL),NUMBER(NLEVEL)
     +      ,SLENG,STEP,DESU,KUN1,GEKU,KUN2,(MECNAM(I),I=1,NM)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,3000)NMAT,NAMATE,A,Z,DENS,RADL,ABSL
      CALL GMAIL(0,0)
C
 1000 FORMAT(' =====> TRACK ',I3,' STACK NR',I4,' NTMULT=',I5,5X,
     +5A4,5X,'TOFG =',F10.3,' NS')
 1100 FORMAT('      X         Y         Z         R      NAME  NUMBER',
     +'   SLENG      STEP      DESTEP     GEKIN    MECHANISMS')
 1200 FORMAT('  NMAT',' Name of material    ','  At.wt.','  At.N0.',
     &  '   Dens.','     Rd.Ln.','     Ab.Ln.')
 2000 FORMAT(1X,4F10.4,2X,A4,2X,I4,2X,2F10.4,F7.1,A4,F9.3,A4,2X,
     +          6(A4,1X))
 3000 FORMAT(1X,I5,1X,5A4,3(1X,F7.2),2(1X,E10.4))
C
  999 RETURN
      END
