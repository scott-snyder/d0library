      REAL*8 FUNCTION RTSEC(R,X1,X2,XACC)
C
C   Modified 30-Nov-1994 R. J. Genik II Added gtop_too_big flag and bail
C
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL GTOP_TOO_BIG ! now used as generic flag for calc is messed
                           ! up
      COMMON/FLAGS/ GTOP_TOO_BIG
      EXTERNAL R

      MAXIT=100

      FL=R(X1)
      F=R(X2)
      IF (ABS(FL).LT.ABS(F)) THEN
        RTSEC=X1
        XL=X2
        SWAP=FL
        FL=F
        F=SWAP
      ELSE
        XL=X1
        RTSEC=X2
      ENDIF
      DO J=1,MAXIT
        DX=(XL-RTSEC)*F/(F-FL)/5.       !/5 is temporary!
        XL=RTSEC
        FL=F
        RTSEC=RTSEC+DX
        F=R(RTSEC)
        IF((ABS(DX).LT.XACC).OR.(F.EQ.0.))
     &             RETURN
      ENDDO
      WRITE(*,*) 'RTSEC exceeded maximum iterations, Bailing'
      RTSEC=-9999.
      GTOP_TOO_BIG = .TRUE.
      RETURN

C       stop
      END
