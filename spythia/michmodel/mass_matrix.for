      SUBROUTINE mass_matrix()        !Diagonalizes neutralino and chargino
                                      !mass matrices, calculating all
                                      !eigenvectors and eigenvalues.

C       Note that the signs of muz in the matrices ARE chosen to give same
C       eigenvalues as Ellis & Zwirner, NPB338:317 (1990). However, I use
C       the basis of Gunion & Haber; that is, the neutralino basis is given
C       by me (and G&H) to be (B,W,H1,H2) - E&Z apparently use (W,B,H2,H1)!
C       There will be some sign differences between the chargino eigenvalues
C       as given by me and E&Z, due to a very strange minus sign in the
C       chargino matrix squared expression that should be on the mu and not
C       on the M2 in their paper.

      IMPLICIT NONE
      INCLUDE 'D0$SPYTHIA$INC:VARS.INC'
      INCLUDE 'D0$SPYTHIA$INC:MASSMAT.INC'

      REAL*8 neu(4,4),cha(2,2)
      REAL*8 neu2(4,4),cha2u(2,2),cha2v(2,2)
      REAL*8 ztemp(4,4),chad(2,2),chatemp(2,2)
      REAL*8 mxtemp(4),mxc1(2)
      REAL*8 small,beta,cw,sw,temp,sgn
      INTEGER nrot,ni,nj,nk,i,j
      EXTERNAL sgn

      beta=atan(tbeta)
      sw=sqrt(sw2)
      cw=sqrt(1.-sw2)

      neu(1,1)=mgf(1)
      neu(1,2)=0.
      neu(1,3)=-mz*sw*cos(beta)
      neu(1,4)=mz*sw*sin(beta)
      neu(2,2)=mgf(2)
      neu(2,3)=mz*cw*cos(beta)
      neu(2,4)=-mz*cw*sin(beta)
      neu(3,3)=0.
      neu(3,4)=muz
      neu(4,4)=0.
      neu(2,1)=neu(1,2)
      neu(3,1)=neu(1,3)
      neu(3,2)=neu(2,3)
      neu(4,1)=neu(1,4)
      neu(4,2)=neu(2,4)
      neu(4,3)=neu(3,4)

      CALL djacobi(neu,4,mxtemp,ztemp,nrot)

      neu(1,2)=neu(2,1)               !Restore superdiagonal terms of NEU
      neu(1,3)=neu(3,1)
      neu(2,3)=neu(3,2)
      neu(1,4)=neu(4,1)
      neu(2,4)=neu(4,2)
      neu(3,4)=neu(4,3)

      DO ni=1,4                       ! Square mass matrix
        DO nj=1,4
          neu2(ni,nj)=0.
          DO nk=1,4
            neu2(ni,nj)=neu2(ni,nj)+neu(ni,nk)*neu(nk,nj)
          ENDDO
        ENDDO
      ENDDO

      CALL djacobi(neu2,4,mxn,z,nrot)
      CALL deigsrt(mxn,z,4)

      small=0.1
      DO i=1,4
        IF (mxn(i).LT.small) mxn(i)=0.
        mxn(i)=sqrt(mxn(i))
      ENDDO

      DO i=1,4
        DO j=1,4
          IF (abs(abs(mxtemp(i))-mxn(j)).LT.small) THEN
            eps(j)=mxtemp(i)/abs(mxtemp(i))
          ENDIF
        ENDDO
      ENDDO

      CALL dtrpose(z,4)

C       Now do the Chargino mass matrix

      cha(1,1)=mgf(2)
      cha(1,2)=mw*sin(beta)*sqrt(2.)
      cha(2,1)=mw*cos(beta)*sqrt(2.)
      cha(2,2)=-muz

      DO ni=1,2                       ! Square mass matrix X*(X trans)
        DO nj=1,2
          cha2u(ni,nj)=0.
          DO nk=1,2
            cha2u(ni,nj)=cha2u(ni,nj)+cha(ni,nk)*cha(nj,nk)
          ENDDO
        ENDDO
      ENDDO

      DO ni=1,2                       ! Square mass matrix (X trans)*X
        DO nj=1,2
          cha2v(ni,nj)=0.
          DO nk=1,2
            cha2v(ni,nj)=cha2v(ni,nj)+cha(nk,ni)*cha(nk,nj)
          ENDDO
        ENDDO
      ENDDO

      CALL djacobi(cha2u,2,mxc,u,nrot)
      CALL djacobi(cha2v,2,mxc1,v,nrot)
      CALL deigsrt(mxc,u,2)
      CALL deigsrt(mxc1,v,2)
      CALL dtrpose(u,2)
      CALL dtrpose(v,2)

      small=0.5
      DO i=1,2
        IF (abs(mxc(i)-mxc1(i)).GT.small) THEN
          WRITE(*,*) 'Error in diagonalizing Chargino mass matrix'
        ENDIF
      ENDDO

      DO ni=1,2                       ! U times CHA
        DO nj=1,2
          chatemp(ni,nj)=0.
          DO nk=1,2
            chatemp(ni,nj)=chatemp(ni,nj)+u(ni,nk)*cha(nk,nj)
          ENDDO
        ENDDO
      ENDDO

      DO ni=1,2                       ! (U times CHA) times Vtranpose
        DO nj=1,2
          chad(ni,nj)=0.
          DO nk=1,2
            chad(ni,nj)=chad(ni,nj)+chatemp(ni,nk)*v(nj,nk)
          ENDDO
        ENDDO
      ENDDO

      mxc(1)=chad(1,1)
      mxc(2)=chad(2,2)
      IF (abs(mxc(1)).GT.abs(mxc(2))) THEN
        temp=mxc(1)
        mxc(1)=mxc(2)
        mxc(2)=temp
      ENDIF
      eps(5)=sgn(mxc(1))
      eps(6)=sgn(mxc(2))
      mxc(1)=abs(mxc(1))
      mxc(2)=abs(mxc(2))

      DO i=1,2
        IF (abs(mxc(i)-sqrt(mxc1(i))).GT.small) THEN
          WRITE(*,*) 'Error in diagonalizing Chargino mass matrix'
        ENDIF
      ENDDO

C       write(*,*)'Neutralino masses:'
C       do i=1,4
C          write(*,*) mxn(i)
C       enddo
C       write(*,*)'Epsilon values:'
C       do i=1,4
C          write(*,*) eps(i)
C       enddo
C       write(*,*)'Chargino masses:'
C       do i=1,2
C          write(*,*) mxc(i)
C       enddo
C       write(*,*)'Neutralino Diagonalizing Matrix:'
C       do i=1,4
C          write(*,*)Z(i,1),Z(i,2),Z(i,3),Z(i,4)
C       enddo
C       write(*,*)'Chargino Diagonalizing Matrices:'
C          write(*,*) U(1,1),U(1,2),'             ',V(1,1),V(1,2)
C          write(*,*) U(2,1),U(2,2),'             ',V(2,1),V(2,2)
C       write(*,*) 'Diagonalized chargino mass matrix:'
C       write(*,*) Chad(1,1),Chad(1,2)
C       write(*,*) Chad(2,1),Chad(2,2)
C       write(*,*) 'Original chargino masses:',sqrt(mxc1(1)),sqrt(mxc1(2))
C       write(*,*) 'Original mass matrix:'
C       write(*,*) cha(1,1), cha(1,2)
C       write(*,*) cha(2,1), cha(2,2)

      RETURN
      END
