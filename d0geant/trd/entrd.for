      SUBROUTINE ENTRD(MASS,GETOT,VECTIN,VECT,RANGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C
C  STEERING ROUTINE FOR ENERGY DEPOSITED IN TRD
C-
C-   Inputs  :
C  MASS      = MASS OF PARTICLE
C  GETOT      = CURRENT TRACK TOTAL ENERGY
C  VECT   (I) = TRACK PARAMETERS (X,Y,Z,PX/P,PY/P,PZ/P,P) AT THE EXIT
C  VECTIN (I) = TRACK PARAMETERS (X,Y,Z,PX/P,PY/P,PZ/P,P) AT THE ENTRANC
C-   Outputs :
C-
C-   Created  AZ  6/19/86   A.Z. 20/10/86 MODIF TO INCLUDE CLUSTERS
C-   Updated  22-DEC-1987   A. ZYLBERSTEJN  TAKE PROPERLY INTO ACCOUNT
C-                                           ENTRANCE POINT
C-   Updated  19-JUL-1989   A. Zylberstejn  Take into account the total range
C-                                          in Xenon
C-   Updated   9-JUL-1992   A. Zylberstejn  : call ENTRD with beta*gamma
C-                                                          instead of Gamma
C-   Updated  18-JUN-1993   J.P. Cussonneau : Add histo (id=7307)
C-
C*********************************************************************
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUGEN.INC/LIST'
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
C
      INTEGER ISTACK,NNPRNT
      REAL eta,LENGTH,MASS,GETOT,RANGE,VECTIN(7),VECT(7)
      INTEGER IFOIS,NPAR,NATT,NS0
      PARAMETER(NPAR=3,NATT=1)
      REAL PAR(NPAR)
      REAL DL2,DLT2,GAMMA
      COMMON/GCNUM/IBID(10),NMBIT
C
      INTEGER   IBID,NMBIT,I
      INTEGER NRANDO,NBIT2,JBYT,NRANDI,NHASAR(2)
      DATA NNPRNT,IFOIS/2*0/
C
      IFOIS = IFOIS+1
      NBIT2=NMBIT/2
C  COMPUTE GAMMA AND SINE(THETA) OF THE PARTICLE
      DLT2=(VECT(1)-VECTIN(1))**2+(VECT(2)-VECTIN(2))**2
      DL2=DLT2+(VECT(3)-VECTIN(3))**2
 8875 FORMAT(' GAMMA ',G10.4,' STET ',G10.4,' MASS ',G10.4,
     +  ' LENGTH',G10.4)
 9900 FORMAT (' CUSET ',A4,' CUDET ',A4)
      IF(DL2.LE.0.01)THEN
        IF(PTRD.GE.5)THEN
          WRITE(LOUT,*)' ENTER ENTRD WITH VECTIN',VECTIN,'VECT',VECT
          WRITE (LOUT,*)'LAYER', ISTACK,' IFOIS',IFOIS
        END IF
        RETURN
      END IF
      STET=SQRT(DLT2/DL2)
C  LENGTH     = PATH LENGTH IN THE TEC (TIME EXPANSION CHAMBER)
      LENGTH=SQRT(DL2)
      eta=vect(7)/mass
      GAMMA = GETOT/MASS
      if(vect(7)/getot.lt. .9 .and.ptrd.ge.5)
     + write(lout,*)' In entrd,p,e',vect(7),getot,' masse',mass,
     + ' beta',vect(7)/getot
      ISTACK = TSTACK
C  INITIALIZE TOTAL DEPOSITED ENERGIES
      EGENRX=0.
      EGENRD=0.
      EGENRT=0.
      EGENST=0.
      EGRDRI=0.
      EGSDRI=0.
C
      IF (STRD(1).EQ.2.) GO TO 500
C  -----------------
C  GENERATE CLUSTERS
C  -----------------
      NCLD = 0
      NCLX = 0
      NSMEAR = 0
      CALL CLUDEL(eta,LENGTH,RANGE)     ! DELTA RAYS
      NCLD = NSMEAR
      IF(PTRD.GE.9)CALL CLUPRN('AFTER ','CLUDEL')
      IF ( GAMMA.GT.1000. ) THEN        ! TRANSITION X RAYS
        CALL CLURAD(GAMMA,LENGTH,RANGE)
        IF(PTRD.GE.9)CALL CLUPRN('AFTER ','CLURAD')
        EGENRT=EGENRX+EGENRD
        NCLD = NSMEAR
      ENDIF
      IF ( NSMEAR.LE.0 )GO TO 990
C  OPERATION ON CLUSTERS
C ----------------------
      NS0=NSMEAR
      CALL CLUPOS(ISTACK,VECTIN,VECT)    !COMPUTE CLUSTERS POSITION IN
C                                            THE GENERAL FRAME
      IF(NSMEAR.LE.0)THEN
        WRITE(LOUT,*)' PROBLEM_TRD IN CLUPOS,NSMEAR=',NSMEAR,
     +   ' IFOIS',IFOIS
        GO TO 990
      END IF
      IF(PTRD.GT.9)CALL CLUPRN('AFTER ','CLUPOS')
      CALL CLUSOR            !SORT IN ASCENDING ORDER W.R.T TIME
      IF(PTRD.GE.9)CALL CLUPRN('AFTER ','CLUSOR')
      CALL CLUMRG            !MERGE THE CLUSTERS WHICH ARE VERY CLO
      IF(PTRD.GE.9)CALL CLUPRN('AFTER ','CLUMRG')
      CALL CLUSMR            !ATTACHMENT CALCULATION
      IF(PTRD.GE.9)CALL CLUPRN('AFTER ','CLUSMR')
      CALL CLUDIG            !DIGITISATION
      IF(PTRD.GE.8)THEN
        WRITE(LOUT,*)'EXIT ENTRD WITH :EGENRD',EGENRD,' DEXR',EGENRT
     +,' ETOT',EGENST
      END IF
      GO TO 900
C  --------------------------------
C  DEAL WITH TOTAL DEPOSITED ENERGY
C  --------------------------------
  500 CALL EDEGEN(GAMMA,EGENRD)
      EGENRD = EGENRD*LENGTH
      IF ( GAMMA.GT.1000. ) CALL EXRGEN(STET,GAMMA,EGENRT)
      EGENST=EGENRD+EGENRT
      IF ( PTRD.GE.8 ) THEN
        NNPRNT = NNPRNT+1
        WRITE (LOUT,*) ' STET ',STET,' GAMMA ',GAMMA,' DEDX ',EGENRD,
     +    ' DEXR ',EGENRT
      ENDIF
C
  900 CONTINUE
      IF(TRHIST)THEN
        CALL HF1(7307,LENGTH,1.)     
        CALL HF1(7001,EGENRD,1.)
        IF ( GAMMA.GT.1000. ) CALL HF1(7002,EGENRT,1.)
        CALL HF1(7003,EGENST,1.)
      END IF
  990 CONTINUE
      RETURN
      END
