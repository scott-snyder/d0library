      SUBROUTINE FIND_WLNU(WMASS,LEP4VEC,NU2VEC,W4VEC,WPZ2,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          calculate W 4-vector given lepton and neutrino 2-vector
C-   Inputs  : 
C-        LEP4VEC= lepton 4-vector
C-        NU2VEC = neutrino px,py
C-   Outputs : 
C-       WMASS= MASS OF W
C-       W4VEC= W 4-vector
C-       WPZ2 = 2nd solution Pz for W
C-       OK   = false -> no solution
C-
C-   Created  26-NOV-1990   Serban D. Protopopescu
C-   Updated   3-JAN-1991   Rajendran Raja  ADDED W MASS AS AN ARGUMENT 
C-   Modified 25-JUN-1993   Stan M. Krzywdzinski 
C-                          Added ENTRY GET_WLNU_ERROR_MATRICES 
C-   Modified  9-FEB-1994   In M5 matrix - added x,y correlations 
C-                          between electron and neutrino; changed
C-                          error matrix for W accordingly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    LEP4VEC(4),NU2VEC(2),W4VEC(4),WZ2ND
      REAL    B,C,MWSQ,A1,A2,PZ1,PZ2,WPZ1,WPZ2,ROOT
      LOGICAL OK
      REAL    WMASS
C----------------------------------------------------------------------
      REAL    LLEP4VEC(4),LNU2VEC(2),LW4VEC(4),LWPZ2
      SAVE    LLEP4VEC   ,LNU2VEC   ,LW4VEC   ,LWPZ2
      LOGICAL LOK
      SAVE    LOK      
      SAVE    B,MWSQ,ROOT
C----------------------------------------------------------------------
      REAL    LEPMAT(3,3),NU2MAT(2,2)
      REAL    NUMAT1(3,3),NUMAT2(3,3),WMAT1(3,3),WMAT2(3,3)
      LOGICAL MOK1,MOK2
      LOGICAL MOK
      REAL    NUMAT(3,3,2),WMAT(3,3,2)
      REAL    NUPZ(2),M5(5,5),DNUPZDP5(5,2),DBDP5(5),DCDP5(5)
      REAL    ROW5(5),COL5(5),RMBIL
      INTEGER ISOL,I,J
      REAL    LEPTSQ,NUPTSQ,BOVLEPZ
      REAL    R(3),DET,TEMP(3,3)
      INTEGER IFAIL,JFAIL
C----------------------------------------------------------------------
C
C             solve for Pz of neutrino
C
      MWSQ = WMASS*WMASS
      A1=(MWSQ+2.*LEP4VEC(1)*NU2VEC(1)+2.*LEP4VEC(2)*NU2VEC(2))
     &  /LEP4VEC(4)/2.
      A2=LEP4VEC(3)/LEP4VEC(4)
      B=A2*A1/(A2*A2-1.)
      C=(A1*A1-NU2VEC(1)**2-NU2VEC(2)**2)/(A2*A2-1.)
      ROOT=B**2-C
      OK=.TRUE.
      IF ( ROOT.GT.0 ) THEN
        ROOT=SQRT(ROOT)
        PZ1=-B+ROOT
        PZ2=-B-ROOT
      ELSE
        PZ1=-B
        PZ2=-B
        IF(-ROOT.GT.400.) OK=.FALSE.
      ENDIF
C
C      fill W 4-vector
      W4VEC(1)=LEP4VEC(1)+NU2VEC(1)
      W4VEC(2)=LEP4VEC(2)+NU2VEC(2)
      WPZ1=LEP4VEC(3)+PZ1
      WPZ2=LEP4VEC(3)+PZ2
      IF(ABS(WPZ1).LT.ABS(WPZ2)) THEN
        W4VEC(3)=WPZ1
        W4VEC(4)=LEP4VEC(4)+SQRT(NU2VEC(1)**2+NU2VEC(2)**2+PZ1**2)
      ELSE
        W4VEC(3)=WPZ2
        W4VEC(4)=LEP4VEC(4)+SQRT(NU2VEC(1)**2+NU2VEC(2)**2+PZ2**2)
        WPZ2=WPZ1
      ENDIF
C
C *** Copy subroutine arguments into local variables for ENTRY use
C
      CALL UCOPY(LEP4VEC,LLEP4VEC,4)
      CALL UCOPY(NU2VEC,LNU2VEC,2)
      CALL UCOPY(W4VEC,LW4VEC,4)
      LWPZ2 = WPZ2
      LOK = OK
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY GET_WLNU_ERROR_MATRICES(LEPMAT,NU2MAT,
     &                              NUMAT1,WMAT1,MOK1,NUMAT2,WMAT2,MOK2)
C-
C-   Purpose and Methods : 
C-     Calculate neutrino and W error matrices in (Px,Py,Pz) 
C-     for the 2 solutions, given lepton 3x3 and neutrino 2x2
C-     error matrices.
C-     The routine has to be called immediately after FIND_WLNU.
C-
C-   Inputs  : LEPMAT(3,3)    [R]   Lepton   error matrix in (Px,Py,Pz)
C-             NU2MAT(2,2)    [R]   Neutrino error matrix in (Px,Py)
C-   Outputs : NUMAT1(3,3)    [R]   Neutrino error matrix in (Px,Py,Pz)
C-                                  for the 1-st solution
C-             WMAT1(3,3)     [R]   W error matrix in (Px,Py,Pz)
C-                                  for the 1-st solution
C-             MOK1           [L]   TRUE if both, 1-st solution matrices,
C-                                  are OK
C-             NUMAT2(3,3)    [R]   Neutrino error matrix in (Px,Py,Pz)
C-                                  for the 2-nd solution
C-             WMAT2(3,3)     [R]   W error matrix in (Px,Py,Pz)
C-                                  for the 2-nd solution
C-             MOK2           [L]   TRUE if both, 2-nd solution matrices,
C-                                  are OK
C-
C-   Created: 14-JUN-1993   Stan M. Krzywdzinski
C-   Modified  4-NOV-1993   Stan M. Krzywdzinski
C-                          Check if diagonal elements are positive
C-
C----------------------------------------------------------------------
C
      CALL VZERO(NUMAT1,9)
      CALL VZERO(WMAT1,9)
      CALL VZERO(NUMAT2,9)
      CALL VZERO(WMAT2,9)
      MOK = LOK
      MOK1 = MOK
      MOK2 = MOK
C
      IF (.NOT. MOK)  GO TO 1000
C
      LEPTSQ = LLEP4VEC(1)**2+LLEP4VEC(2)**2
      MOK = MOK .AND. (LEPTSQ .GT. 0.)
      MOK1 = MOK
      MOK2 = MOK
      IF (.NOT. MOK)  GO TO 1000
      BOVLEPZ = -(MWSQ+2.*LLEP4VEC(1)*LNU2VEC(1)+
     &                 2.*LLEP4VEC(2)*LNU2VEC(2) )/(2.*LEPTSQ)
      NUPTSQ = LNU2VEC(1)**2+LNU2VEC(2)**2
C
      NUPZ(1) = LW4VEC(3)-LLEP4VEC(3)
      NUPZ(2) = LWPZ2    -LLEP4VEC(3)
C
      DO ISOL=1,2
        DO I=1,2
          DO J=1,2
            NUMAT(I,J,ISOL) = NU2MAT(I,J)
            WMAT (I,J,ISOL) = LEPMAT(I,J)+NU2MAT(I,J)
          ENDDO
        ENDDO
      ENDDO
C
      CALL VZERO(M5,25)
      CALL UCOPY(LEPMAT(1,1),M5(1,1),3)
      CALL UCOPY(LEPMAT(1,2),M5(1,2),3)
      CALL UCOPY(LEPMAT(1,3),M5(1,3),3)
      CALL VCOPYN(LEPMAT(1,1),M5(1,4),3)
      CALL VCOPYN(LEPMAT(1,2),M5(1,5),3)
      CALL VCOPYN(LEPMAT(1,1),M5(4,1),2)
      CALL VCOPYN(LEPMAT(1,2),M5(4,2),2)
      CALL VCOPYN(LEPMAT(1,3),M5(4,3),2)
      CALL UCOPY(NU2MAT(1,1),M5(4,4),2)
      CALL UCOPY(NU2MAT(1,2),M5(4,5),2)
C
      DBDP5(1) = -(2.*B*LLEP4VEC(1)+LLEP4VEC(3)*LNU2VEC(1))/LEPTSQ
      DBDP5(2) = -(2.*B*LLEP4VEC(2)+LLEP4VEC(3)*LNU2VEC(2))/LEPTSQ
      DBDP5(3) = BOVLEPZ
      DBDP5(4) = -LLEP4VEC(1)*LLEP4VEC(3)/LEPTSQ
      DBDP5(5) = -LLEP4VEC(2)*LLEP4VEC(3)/LEPTSQ
      IF (ROOT .GT. 0.) THEN
        DCDP5(1) = 2.*BOVLEPZ*(BOVLEPZ*LLEP4VEC(1)+LNU2VEC(1)) -
     &             2.*LLEP4VEC(1)*LLEP4VEC(3)**2*NUPTSQ/LEPTSQ**2
        DCDP5(2) = 2.*BOVLEPZ*(BOVLEPZ*LLEP4VEC(2)+LNU2VEC(2)) -
     &             2.*LLEP4VEC(2)*LLEP4VEC(3)**2*NUPTSQ/LEPTSQ**2
        DCDP5(3) = 2.*LLEP4VEC(3)*NUPTSQ/LEPTSQ**2
        DCDP5(4) = 2.*BOVLEPZ*LLEP4VEC(1) + 
     &             2.*LLEP4VEC(4)**2*LNU2VEC(1)/LEPTSQ
        DCDP5(5) = 2.*BOVLEPZ*LLEP4VEC(2) +
     &             2.*LLEP4VEC(4)**2*LNU2VEC(2)/LEPTSQ
        DO ISOL = 1,2
          DO I = 1,5
            DNUPZDP5(I,ISOL) = -(NUPZ(ISOL)*DBDP5(I)+DCDP5(I)/2.)/
     &                          (NUPZ(ISOL)+B)
          ENDDO
        ENDDO
      ELSE
        DO ISOL = 1,2
          DO I = 1,5
            DNUPZDP5(I,ISOL) = -DBDP5(I)
          ENDDO
        ENDDO
      ENDIF
C
C
      DO ISOL = 1,2
        CALL UCOPY(DNUPZDP5(1,ISOL),COL5,5)
C
C ***   Variance on Pz of neutrino 
C
        CALL UCOPY(DNUPZDP5(1,ISOL),ROW5,5)
        NUMAT(3,3,ISOL)=RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,                 COL5(1),COL5(2))
C        
C ***   Correlation (Px, Pz) of neutrino
C
        CALL VZERO(ROW5,5)
        ROW5(4) = 1.
        NUMAT(1,3,ISOL)=RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,                 COL5(1),COL5(2))
        NUMAT(3,1,ISOL)=NUMAT(1,3,ISOL)
C
C ***   Correlation (Py, Pz) of neutrino
C
        CALL VZERO(ROW5,5)
        ROW5(5) = 1.
        NUMAT(2,3,ISOL)=RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,                 COL5(1),COL5(2))
        NUMAT(3,2,ISOL)=NUMAT(2,3,ISOL)
C
C
C ***   Variance on Px of W,    taking into account correlations
C
        WMAT(1,1,ISOL)=NUMAT(1,1,ISOL)-LEPMAT(1,1)
C
C ***   Variance on Py of W,    taking into account correlations
C
        WMAT(2,2,ISOL)=NUMAT(2,2,ISOL)-LEPMAT(2,2)
C
C ***   Variance on Pz of W,    taking into account 
C       correlation (Pz of electron, Pz of neutrino)
C
        CALL VZERO(ROW5,5)
        ROW5(3) = 1.
        WMAT(3,3,ISOL)=LEPMAT(3,3)+NUMAT(3,3,ISOL)+
     &                2.*RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,               COL5(1),COL5(2)) 
C
C ***   Correlation (Px,Py) of W
C 
        WMAT(1,2,ISOL)=NUMAT(1,2,ISOL)-LEPMAT(1,2)
        WMAT(2,1,ISOL)=WMAT(1,2,ISOL)
C
C ***   Correlation (Px, Pz) of W
C
        CALL VZERO(ROW5,5)
        ROW5(1) = 1.
        WMAT(1,3,ISOL)=NUMAT(1,3,ISOL)+
     &                RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,               COL5(1),COL5(2))
        WMAT(3,1,ISOL)=WMAT(1,3,ISOL)
C
C ***   Correlation (Py, Pz) of W
C
        CALL VZERO(ROW5,5)
        ROW5(2) = 1.
        WMAT(2,3,ISOL)=NUMAT(2,3,ISOL)+
     &                RMBIL(5,ROW5(1),ROW5(2),M5(1,1),M5(1,2),M5(2,1)
     &,               COL5(1),COL5(2))
        WMAT(3,2,ISOL)=WMAT(2,3,ISOL)
C
C ***   Check if diagonal elements are positive
C
        IF ( (NUMAT(1,1,ISOL) .LT. 0.)  .OR.
     &       (NUMAT(2,2,ISOL) .LT. 0.)  .OR.
     &       (NUMAT(3,3,ISOL) .LT. 0.)       ) THEN
          CALL VZERO(NUMAT(1,1,ISOL),9)
          IF (ISOL .EQ. 1) THEN
            MOK1 = .FALSE.
          ELSE
            MOK2 = .FALSE.
          ENDIF
        ENDIF
C
        IF ( (WMAT(1,1,ISOL) .LT. 0.)  .OR.
     &       (WMAT(2,2,ISOL) .LT. 0.)  .OR.
     &       (WMAT(3,3,ISOL) .LT. 0.)       ) THEN
          CALL VZERO(WMAT(1,1,ISOL),9)
          IF (ISOL .EQ. 1) THEN
            MOK1 = .FALSE.
          ELSE
            MOK2 = .FALSE.
          ENDIF
        ENDIF
C
C ***   Check if determinants of matrices are OK
C
        CALL UCOPY(NUMAT(1,1,ISOL),TEMP,9)
        CALL RFACT(3,TEMP,3,R,IFAIL,DET,JFAIL)
        IF ( (IFAIL.NE.0) .OR. (JFAIL.NE.0) ) THEN
          NUMAT(1,2,ISOL) = 0.
          NUMAT(2,1,ISOL) = 0.
          NUMAT(1,3,ISOL) = 0.
          NUMAT(3,1,ISOL) = 0.
          NUMAT(2,3,ISOL) = 0.
          NUMAT(3,2,ISOL) = 0.
          CALL UCOPY(NUMAT(1,1,ISOL),TEMP,9)
          CALL RFACT(3,TEMP,3,R,IFAIL,DET,JFAIL)
          IF ( (IFAIL.NE.0) .OR. (JFAIL.NE.0) ) THEN
            CALL VZERO(NUMAT(1,1,ISOL),9)
            IF (ISOL .EQ. 1) THEN
              MOK1 = .FALSE.
            ELSE
              MOK2 = .FALSE.
            ENDIF
          ENDIF
        ENDIF
C
        CALL UCOPY(WMAT(1,1,ISOL),TEMP,9)
        CALL RFACT(3,TEMP,3,R,IFAIL,DET,JFAIL)
        IF ( (IFAIL.NE.0) .OR. (JFAIL.NE.0) ) THEN
          WMAT(1,2,ISOL) = 0.
          WMAT(2,1,ISOL) = 0.
          WMAT(1,3,ISOL) = 0.
          WMAT(3,1,ISOL) = 0.
          WMAT(2,3,ISOL) = 0.
          WMAT(3,2,ISOL) = 0.
          CALL UCOPY(WMAT(1,1,ISOL),TEMP,9)
          CALL RFACT(3,TEMP,3,R,IFAIL,DET,JFAIL)
          IF ( (IFAIL.NE.0) .OR. (JFAIL.NE.0) ) THEN
            CALL VZERO(WMAT(1,1,ISOL),9)
            IF (ISOL .EQ. 1) THEN
              MOK1 = .FALSE.
            ELSE
              MOK2 = .FALSE.
            ENDIF
          ENDIF
        ENDIF
C
C
        IF (ISOL .EQ. 1) THEN
          CALL UCOPY(NUMAT(1,1,ISOL),NUMAT1,9)
          CALL UCOPY( WMAT(1,1,ISOL),WMAT1, 9)
        ELSE
          CALL UCOPY(NUMAT(1,1,ISOL),NUMAT2,9)
          CALL UCOPY( WMAT(1,1,ISOL),WMAT2, 9)
        ENDIF
C
      ENDDO
C
C
1000  RETURN
C----------------------------------------------------------------------
      END
