      SUBROUTINE VKIN_BOECK(K,NF,NM,NX,F,B,A,GI,C,DX,GMFI,GXI,CHI,
     1           IPRINT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to make matrix multiplies 
C-                         for each fitting step.
C-
C-   Inputs  : K      -    input key for this routine
C-             K      =    0 - for each step of iteration
C-                    =    1 - for additional call before end of fit
C-             NF     =    4 - number of elemens dimension F
C-             NM     -    number of measured values
C-             NX     -    number of unmeasured values
C-             F(1)   =    P1x + P2x - P3x
C-             F(2)   =    P1y + P2y - P3y
C-             F(3)   =    P1z + P2z - P3z
C-             F(4)   =    E1  + E2  - E3, 
C-     where:  PIx - momentum projection of I-th track 
C-                   to the x- direction (for example)      
C-             EI  - energy of I-th track
C-             A      -    matrix of derivatives for current 
C-                         values of X (momenta)
C-             B      -    matrix of derivatives for current 
C-                         values of ETA (angels)
C-             GI     -    covariance error matrix for measured
C-                         values (copy of CY) 
C              IPRINT =    3 - print out able
C                     <    3 - print out disable 
C-   Outputs : 
C              C      -    change in ETA
C              DX     -    change in X
C-             GMFI   -    covariance error matrix for measured
C-                         values 
C-             GXI    -    covariance error matrix for unmeasured
C-                         values 
C              CHI    -    chi squared (K=0)
C-   Controls: 
C-
C-   Created original       V.A.Gapienko
C-   Updated                V.I.Sirotenko
C-   Updated  24-jul-1991   V. Burtovoy
C-   Updated  21-SEP-1993   H. Castilla, B. Gomez & O. Ramirez to put a
C-                          protection against problems with the inversion of a 
C-                          singular matrix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL*8 F(4),B(200),GI(1500),C(50),GMFI(1500),A(20),DX(50)
     +,    GXI(20),H(1500),HH(1500),R(6),AL(6),D(6),CHI
      REAL*8 DUNITN,DVMPY
      REAL RR(30),RRR(30)
      INTEGER K,NF,NM,NX,IPRINT,IFAIL
      DATA DUNITN / -1.D00 /
C
      IF(IPRINT.GE.3) THEN
        WRITE(2,7676) K
 7676   FORMAT(5X,'****** BOECK ******',I5/)
      END IF
      IF(K) 2,1,2
    1 CONTINUE
      CALL DMMPY(NF,NM,B(1),B(2),B(NM+1),C(1),C(2),H(1),H(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('1       ',h,nf,1)
      END IF
      CALL DVSUB(NF,F(1),F(2),H(1),H(2),R(1),R(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('2       ',r,nf,1)
      END IF
      CALL DMMLT(NM,NM,NF,GI(1),GI(2),GI(NM+1),B(1),B(NM+1),B(2)
     +,          HH(1),HH(2),HH(NF+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('3       ',hh,nm,nm)
      END IF
      CALL DMMLT(NF,NM,NF,B(1),B(2),B(NM+1),HH(1),HH(2),HH(NF+1)
     +,          H(1),H(2),H(NF+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('4       ',h,nf,nf)
      END IF
      CALL DVCPY(NF*NF,H(1),H(2),GMFI(1),GMFI(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('5       ',gmfi,nf,nf)
      END IF
      CALL DINV(NF,GMFI,NF,RR,IFAIL)
      IF(IPRINT.GE.3) THEN
         write(2,888) ifail
  888    format(15x,'!!!!! --gmfi-- ifail = ',i2)
         call mxwrt('6       ',gmfi,nf,nf)
      END IF
C
C ****  If inversion of matrix has problems go out and set Chi squared to -1.
C
      IF(IFAIL .NE. 0)THEN
        CHI = DBLE(-1.)
        GOTO 1000
      ENDIF  
      IF(NX) 11,11,12
   12 CONTINUE
      CALL DMMLT(NF,NF,NX,GMFI(1),GMFI(2),GMFI(NF+1),A(1),A(2),A(NX+1)
     +,          H(1),H(2),H(NX+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('7       ',h,nf,nx)
      END IF
      CALL DMMLT(NX,NF,NX,A(1),A(NX+1),A(2),H(1),H(2),H(NX+1)
     +,          HH(1),HH(2),HH(NX+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('8       ',hh,nx,nx)
      END IF
      CALL DVCPY(NX*NX,HH(1),HH(2),GXI(1),GXI(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('9       ',gxi,nx,nx)
      END IF
      CALL DINV(NX,GXI,NX,RRR,IFAIL)
      IF(IPRINT.GE.3) THEN
         write(2,889) ifail
  889    format(15x,'!!!!!  --gxi-- ifail = ',i2)
         call mxwrt('10      ',gxi,nx,nx)
      END IF
C
C ****  If inversion of matrix has problems go out and set Chi squared to -1.
C
      IF(IFAIL .NE. 0)THEN
        CHI = DBLE(-1.)
        GOTO 1000
      ENDIF  
      CALL DMMPY(NX,NF,H(1),H(NX+1),H(2),R(1),R(2),C(1),C(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('11      ',c,nx,1)
      END IF
      CALL DMMPY(NX,NX,GXI(1),GXI(2),GXI(NX+1),C(1),C(2),DX(1),DX(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('12      ',dx,nx,1)
      END IF
      CALL DVSCL(NX,DUNITN,DX(1),DX(2),DX(1),DX(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('13      ',dx,nx,1)
      END IF
      CALL DMMPA(NF,NX,A(1),A(2),A(NX+1),DX(1),DX(2),R(1),R(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('14      ',r,nf,1)
      END IF
   11 CONTINUE
      CALL DMMPY(NF,NF,GMFI(1),GMFI(2),GMFI(NF+1),R(1),R(2),AL(1),AL(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('15      ',al,nf,1)
      END IF
      D(1)=DVMPY(NF,AL(1),AL(2),R(1),R(2))
      CHI=D(1)
      IF(IPRINT.GE.3) THEN
         call mxwrt('16      ',d,1,1)
      END IF
      CALL DMMPY(NM,NF,B(1),B(NM+1),B(2),AL(1),AL(2),H(1),H(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('17      ',h,nm,1)
      END IF
      CALL DMMPY(NM,NM,GI(1),GI(2),GI(NM+1),H(1),H(2),C(1),C(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('18      ',c,nm,1)
      END IF
      CALL DVSCL(NM,DUNITN,C(1),C(2),C(1),C(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('19      ',c,nm,1)
      END IF
      RETURN
    2 CONTINUE
      IF(NX) 21,21,22
   22 CONTINUE
      CALL DMMLT(NX,NF,NF,A(1),A(NX+1),A(2),GMFI(1),GMFI(2),GMFI(NF+1)
     +,          H(1),H(2),H(NF+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('20      ',h,nx,nf)
      END IF
      CALL DVCPY(NX*NF,H(1),H(2),A(1),A(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('21      ',a,nx,nf)
      END IF
   21 CONTINUE
      CALL DMMLT(NF,NM,NM,B(1),B(2),B(NM+1),GI(1),GI(2),GI(NM+1)
     +,          H(1),H(2),H(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('22      ',h,nf,nm)
      END IF
      CALL DVCPY(NF*NM,H(1),H(2),B(1),B(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('23      ',b,nf,nm)
      END IF
      CALL DMMLT(NF,NF,NM,GMFI(1),GMFI(2),GMFI(NF+1),B(1),B(2),B(NM+1)
     +,          H(1),H(2),H(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('24      ',h,nf,nm)
      END IF
      CALL DMMLT(NM,NF,NM,B(1),B(NM+1),B(2),H(1),H(2),H(NM+1)
     +,          HH(1),HH(2),HH(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('25      ',hh,nm,nm)
      END IF
      CALL DMSUB(NM,NM,GI(1),GI(2),GI(NM+1),HH(1),HH(2),HH(NM+1)
     +,          GMFI(1),GMFI(2),GMFI(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('26      ',gmfi,nm,nm)
      END IF
      IF(NX) 23,23,24
   23 RETURN
   24 CONTINUE
      CALL DMMLT(NX,NF,NM,A(1),A(2),A(NF+1),B(1),B(2),B(NM+1)
     +,          H(1),H(2),H(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('27      ',h,nx,nm)
      END IF
      CALL DVCPY(NX*NM,H(1),H(2),HH(1),HH(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('29      ',hh,nx,nm)
      END IF
      CALL DMMLT(NM,NX,NX,HH(1),HH(NM+1),HH(2),GXI(1),GXI(2),GXI(NX+1)
     +,          B(1),B(2),B(NX+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('30      ',b,nm,nx)
      END IF
      CALL DVSCL(NM*NX,DUNITN,B(1),B(2),B(1),B(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('31      ',b,nm,nx)
      END IF
      CALL DMMLT(NM,NX,NM,B(1),B(2),B(NX+1),HH(1),HH(2),HH(NM+1)
     +,          H(1),H(2),H(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('32      ',h,nm,nm)
      END IF
      CALL DMSUB(NM,NM,GMFI(1),GMFI(2),GMFI(NM+1),H(1),H(2),H(NM+1)
     +,          HH(1),HH(2),HH(NM+1))
      IF(IPRINT.GE.3) THEN
         call mxwrt('33      ',hh,nm,nm)
      END IF
      CALL DVCPY(NM*NM,HH(1),HH(2),GMFI(1),GMFI(2))
      IF(IPRINT.GE.3) THEN
         call mxwrt('34      ',gmfi,nm,nm)
      END IF
 1000 CONTINUE
      RETURN
      END
