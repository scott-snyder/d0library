      SUBROUTINE MGEOFL(IMOD,T1,R1,MGEO,IMGEO,IDATE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank MGEO
C-
C-   Inputs  : IMOD             [I]     module number
C-             T1(3),R1(3,3)    [R]     traslation and Rotation matrix SURVEY
C-             MGEO(*)          [R]
C-             IMGEO(*)         [I]
C-             IDATE            [I]     Date
C-   Outputs : IER              [I]     0 - OK
C-   Controls:
C-
C-   Created  17-AUG-90   from MSOPFL              S.T.Repond
C-
C-   NOTES:
C                T1     X,Y,Z position ( in Global Ref. ) of the center
C                       of the module
C                R1        ROTATION MATRIX FROM SURVEY *R1*
C
C                       X_ij = R(i,j) * X_j
C
C              where R(i,j) = RL_TO_G(i,j) = L_TO_G(j,k) * L_TO_RL(k,i)
C-
C-   Modified   21-AUG-90  get pointer LMGEO by GZMGEO in any case
C                          fill the rest of new bank as it is in old (MC)
C               25-SEP-90  get the rest (old bank) from WRT_MGEO
C           25-OCT-90  add flag NO_SRV: if =1, fill with FULL old bank
C           WARNING!: MGEO banks with different bank VRSN are
C                     NOT compatible!
C-   Updated   5-JUN-1991   Silvia Repond
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IMOD
      REAL  T1(3),R1(3,3)
      INTEGER IMGEO(*)
      REAL MGEO(*)
      INTEGER IDATE
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSOP.LINK'
      INCLUDE 'D0$LINKS:IZMSRH.LINK'
      INCLUDE 'D0$LINKS:IZMGEH.LINK'
C----------------------------------------------------------------------
      INTEGER LMGEO
      INTEGER GZMGEO
      INTEGER IVSN
      INTEGER IIC
      CHARACTER*60 MSGSTR
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
C ****  Get address of MGEO bank for module IMOD
C
      LMGEO = GZMGEO(IMOD)    ! GET LINK.
C
C    increment VSN number
      IVSN = IC(LMGEO+2)
      IVSN = IVSN+1
C
C Book the bank if argument = 0.
      IF(LMGEO.EQ.0)CALL BKMGEO('STPC',IMOD,IER)
      IF ( IER.NE.0 ) GOTO 999
C
C  FILL THE BANK

      IC(LMGEO+1) = IVSN               ! Bank version ! Tami:TYPE
      IC(LMGEO+2) = 1                     !Status
      IC(LMGEO+3) = -99                !Quality- avrg cell effic.
      IC(LMGEO+4) = 0                    !Low_Run #
      IC(LMGEO+5) = 0                    !High_Run #
      IC(LMGEO+6) = 0                    !Generated Run
      IC(LMGEO+7) = IDATE                !Generated Date
      IC(LMGEO+8) = 0                    !Generated Type of Run
      IC(LMGEO+9) = IMOD                 !Module Number
      IC(LMGEO+10) = IMGEO(10) !Number of planes per module
      IC(LMGEO+11) = IMGEO(11) !Number of cell per plane
      IC(LMGEO+12) = IMGEO(12)  !Module Orientation
      IC(LMGEO+13) = IMGEO(13)  !Stagger Type(TL=1,TR=2,BL=3,BR=4)
      IC(LMGEO+14) = IMGEO(14)  !spare
      IC(LMGEO+15) = IMGEO(15)  !spare
      IC(LMGEO+16) = IMGEO(16)  !spare
      IC(LMGEO+17) = IMGEO(17)  !spare
      C(LMGEO+18) =  MGEO(18)  ! Average resolution(drift)
      C(LMGEO+19) =  MGEO(19) ! Average resolution(wire)
C
C ****  Add survey data
C
      C(LMGEO+20) = T1(1)   ! X of local Origin(CENTER
C                                             of the module) in Global
C                                        corr.by trasl&rot from Survey
      C(LMGEO+21) =  T1(2)  ! Y       "
      C(LMGEO+22) =  T1(3)  ! Z       "
C                            Rotation Mtx R = from Global to Local !
      C(LMGEO+23) =  R1(1,1)!     cos(x,X)  R11
      C(LMGEO+24) =  R1(1,2) !    cos(x,Y)  R12
      C(LMGEO+25) =  R1(1,3) !    cos(x,Z)  R13
      C(LMGEO+26) =  R1(2,1) !    cos(y,X)  R21
      C(LMGEO+27) =  R1(2,2) !    cos(y,Y)  R22
      C(LMGEO+28) =  R1(2,3) !    cos(y,Z)  R23
      C(LMGEO+29) =  R1(3,1) !    cos(z,X)  R31
      C(LMGEO+30) =  R1(3,2) !    cos(z,Y)  R32
      C(LMGEO+31) =  R1(3,3) !    cos(z,Z)  R33
C
      DO IIC = 32,58
        C(LMGEO+IIC) =  MGEO(IIC)
      ENDDO
  999 RETURN
      END
