      REAL FUNCTION LIK_REF_MC(LIKE,NHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute espl' for MC events
C-     tables have been computed using E fired on MC W-e,nu and Z-->ee.
C-     There are 70 bins for -5.lik2<9. The lik2 distributions have been 
C-     smoothed and integrated. 
C-     Under/over flows have been taken into account. Each value in
C-     the tables represent the integral of the distribution from to the right
C-     edge of the bin to infinity.
C-      For instance, the first bin value contains the probability for Lik2>xmi
C-      the second bin the probability for Lik2>xmi_step and so on.
C-      We get the current value by interpolating linearly between the 2
C-     values at the edges of the bin. There was no attempt to re-define the
C-     original distributions entering the likelihood calculation
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-OCT-1995   A. Zylberstejn
C-
C----------------------------------------------------------------------
C      REAL ENERGT,NHIT,IGEN
C      INCLUDE 'D0$INC:PAWIDN_TRD.INC'
      IMPLICIT NONE
      REAL EPSL1(71),EPSL2(71),EPSLR(71,2),LIKE,DX,SL
      EQUIVALENCE (EPSL1(1),EPSLR(1,1)),(EPSL2(1),EPSLR(1,2))
      INTEGER I,ICAS,NX,IB,IBI,IBS,NHIT(3),IFOIS
      REAL XMI,XMA,X(71),LIK,STEP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NX/71/,XMI/-5./,XMA/9./
      DATA EPSL1/
     +.99735,.99708,.99677,.99640,.99594,.99539,.99472
     +,.99391,.99295,.99180,.99046,.98890,.98708,.98497
     +,.98255,.97979,.97666,.97312,.96914,.96470,.95974
     +,.95419,.94801,.94115,.93354,.92514,.91589,.90574
     +,.89463,.88253,.86940,.85520,.83988,.82340,.80572
     +,.78681,.76662,.74522,.72267,.69902,.67434,.64869
     +,.62214,.59474,.56657,.53770,.50823,.47829,.44798
     +,.41741,.38668,.35591,.32521,.29469,.26454,.23500
     +,.20629,.17865,.15230,.12748,.10442,.08333,.06444
     +,.04781,.03353,.02168,.01232,.00554,.00141,.00000,.00000/
c
      DATA EPSL2/
     + .98926,.98785,.98648,.98515,.98383,.98249,.98113
     +,.97970,.97821,.97661,.97489,.97304,.97100,.96875
     +,.96624,.96342,.96025,.95669,.95270,.94823,.94324
     +,.93769,.93154,.92478,.91738,.90931,.90054,.89106
     +,.88083,.86983,.85804,.84543,.83197,.81763,.80240
     +,.78624,.76914,.75106,.73197,.71186,.69069,.66846
     +,.64519,.62095,.59576,.56969,.54276,.51504,.48657
     +,.45738,.42753,.39711,.36631,.33535,.30443,.27376
     +,.24355,.21400,.18533,.15775,.13146,.10670,.08378
     +,.06301,.04469,.02913,.01664,.00751,.00206,.00059,.00059/
      IF(FIRST)THEN
        IFOIS=0
        STEP=(XMA-XMI)/FLOAT(NX-1)
        DO I=1,NX
          X(I)=XMI+FLOAT(I-1)*STEP ! bin left edge
        END DO
        FIRST=.FALSE.
      END IF
      IFOIS=IFOIS+1
      LIK_REF_MC=1.
      IF(NHIT(1).LE.0 .OR.NHIT(2).LE.0 .OR.NHIT(3).EQ.0)GO TO 999
      ICAS=1
      IF(NHIT(1).GT.1 .OR.NHIT(2).GT.1 .OR.NHIT(3).GT.1)ICAS=2
      IF(LIKE.LE.XMI)THEN
        LIK_REF_MC=EPSLR(1,ICAS)
        GO TO 999
      END IF
      IF(LIKE.GE.XMA)THEN
        LIK_REF_MC=EPSLR(NX,ICAS)
        GO TO 999
      END IF
      IB=(LIKE-XMI)/STEP+1
      IB=MIN0(IB,NX)
      IBI=IB+1
      DX=LIKE-X(IB)
      SL=(EPSLR(IBI,ICAS)-EPSLR(IB,ICAS))/step
      LIK_REF_MC=EPSLR(IB,ICAS)+DX*SL
      IF(IFOIS.LE.10
C     +.or.  ib.le.0 .or.ib.ge.50
C     + .or.(LIKE_ref_mc.GT..998.and.ib.gt.5).or.
C     +  .or. LIKE_ref_mc.lt.0.02
     +  )THEN
        PRINT*,'like',LIKE,'ib,step,xmi',IB,STEP,XMI
        PRINT*, 'dx',DX,'sl',SL
        PRINT*,' ib,ibi',IB,IBI,' x',
     +    X(IB),X(IBI),' ref',EPSLR(IB,ICAS),EPSLR(IBI,ICAS),' value',
     +    LIK_REF_MC
      END IF
  999 RETURN
      END
