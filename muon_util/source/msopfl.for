      SUBROUTINE MSOPFL(IMOD,T1,R1,ORIGIN,IDATE,IERBK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank MSOP
C-
C-   Inputs  : IMOD             [I]     module number
C-             T1(3),R1(3,3)    [R]     translation and Rotation matrix SURVEY
C-             ORIGIN(*)        [R]     Survey local Origin (end pin0 deck0)
C-                                      in Global
C-             IDATE            [I]     Date
C-   Outputs : IERBK            [I]     0 - OK
C-   Controls:
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
C-   Created  27-JUNE-90                              S.T.Repond
C-   Modified 03-Aug-90   added IMOD parameter (bank filling happens
C                         inside MOD loop             STR
C                         added IVSN incrementing value when a new
C                          bank VRSN needs to be written [WRMSOP=1]
C             10-AUG-90     control WRMSOP deleted
C             13 AUG 90    IERBK  added from BKMSOP(...,IERBK)
C-   Updated   6-JUN-1991   Silvia Repond
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSOP.LINK'
      INCLUDE 'D0$LINKS:IZMSRH.LINK'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LMSOP, GZMSOP
      INTEGER IMOD,IVSN,IDATE
      REAL  T1(3),R1(3,3),ORIGIN(3)
      CHARACTER*60 MSGSTR
      INTEGER IERBK
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      LMSOP = GZMSOP(IMOD)    ! GET LINK.
C Book the bank if argument = 0.
      IF ( LMSOP .LE. 0 ) THEN
        CALL BKMSOP('STPC',IMOD,IERBK)
        IF(IERBK.EQ.0) GOTO 999
        IVSN = 0
      ENDIF
C
C  FILL THE BANK
C
C    increment VSN number
      IVSN = IC(LMSOP+2)
      IVSN = IVSN+1
      IC(LMSOP+1) = IVSN               ! Bank version ! Tami:TYPE
      IC(LMSOP+2) = 1                     !Status
      IC(LMSOP+3) = 100                  !Quality
      IC(LMSOP+4) = 1                    !Low_Run #
      IC(LMSOP+5) = 1                    !High_Run #
      IC(LMSOP+6) = 1                    !Generated Run
      IC(LMSOP+7) = IDATE                !Generated Date
      IC(LMSOP+8) = 0                    !Generated Type of Run
      IC(LMSOP+9) = IMOD                 !Module Number
      IC(LMSOP+10) = 0                    !spare
      IC(LMSOP+11) = 0                    !spare
      IC(LMSOP+12) = 0                !Module Orientation - not filled -
C                                          (as measured by Surveyors!!)
      IC(LMSOP+13) = 0                   !Stagger Type - not filled -
      IC(LMSOP+14) =  0                   !spare
      IC(LMSOP+15) =  0                   !spare
      IC(LMSOP+16) =  0                   !spare
      C(LMSOP+17) =  ORIGIN(1)        ! X of local Origin(end pin0 deck0
C                                     !   in Global from Survey
      C(LMSOP+18) =  ORIGIN(2)        ! Y       "
      C(LMSOP+19) =  ORIGIN(3)        ! Z       "

C  from ALI_SRV: T1,R1 :
      C(LMSOP+20) = T1(1)   !ex 154.9309 ! X of local Origin(CENTER
C                                             of the module) in Global
C                                        corr.by trasl&rot from Survey
      C(LMSOP+21) =  T1(2)  !ex 297.8603! Y       "
      C(LMSOP+22) =  T1(3)  !ex 259.0573! Z       "
      C(LMSOP+23) =  R1(1,1) !ex 0.0003001!    cos(X,x)  R11
C                                  (X,Y,Z) is Global; (x,y,x) is local
C                                                    i.e. raw data!!
C                                                    i.e. no rot. yet
      C(LMSOP+24) =  R1(1,2) !                 cos(X,y)  R12
      C(LMSOP+25) =  R1(1,3) !                 cos(X,z)  R13
      C(LMSOP+26) =  R1(2,1) !                 cos(Y,x)  R21
      C(LMSOP+27) =  R1(2,2) !                 cos(Y,y)  R22
      C(LMSOP+28) =  R1(2,3) !                 cos(Y,z)  R23
      C(LMSOP+29) =  R1(3,1) !                 cos(Z,x)  R31
      C(LMSOP+30) =  R1(3,2) !                 cos(Z,y)  R32
      C(LMSOP+31) =  R1(3,3) !                 cos(Z,z)  R33
      C(LMSOP+32) = 0                     !Status
      C(LMSOP+33) = 0                    !Status
  999 RETURN
      END
