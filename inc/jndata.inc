C...JetNet block DATA
      INTEGER MAXV,MAXM,MAXI,MAXO
      INTEGER MSTJN,IGFN,NSELF,M,MV0,MM0,NG,NL,IPOTT
      REAL PARJN,OIN,OUT,TINV,O,A,D,T,DT,W,DW
C...Initial values for parameters and switches for JETNET

      PARAMETER(MAXV=2000,MAXM=100000,MAXI=1000,MAXO=1000)

      COMMON /JNDAT1/ MSTJN(20),PARJN(20),OIN(MAXI),OUT(MAXO)
      COMMON /JNDAT2/ TINV(10),IGFN(10)
      COMMON /JNINT1/ O(MAXV),A(MAXV),D(MAXV),T(MAXV),DT(MAXV),
     &                W(MAXM),DW(MAXM),NSELF(MAXM)
      COMMON /JNINT2/ M(0:10),MV0(10),MM0(10),NG(10),NL,IPOTT


C...Brief explanation of parameters and switches :
C...
C...
C...MSTJN(1) (D=3)  number of layers in net
C...MSTJN(2) (D=10) number of patterns per update in JNTRAL
C...MSTJN(3) (D=1)  overall sigmoid function used in net
C...        1 -> g(x)=1/(1+exp(-2x))
C...        2 -> g(x)=tanh(x)
C...        3 -> g(x)=exp(x) (only used internally for Potts-nodes)
C...        4 -> g(x)=x
C...        5 -> g(x)=1/(1+exp(-2x)) (only used internally for
C...             entropy error)
C...MSTJN(4) (D=0)  error measure
C...        0 -> summed square error
C...        1 -> entropy error
C...      >=2 -> Kullback measure, using Potts nodes of dimension MSTJN(4)
C...MSTJN(5) (D=0)  Manhattan updating
C...        0 -> off
C...        1 -> on
C...MSTJN(6) (D=6)  file number for output statistics
C...MSTJN(7) (I)    number of calls to JNTRAL
C...MSTJN(8) (I)    initialization done
C...MSTJN(9) not used
C...MSTJN(10+I)     number of nodes in layer I (I=0 => input layer)
C...MSTJN(10) (D=16)
C...MSTJN(11) (D=8)
C...MSTJN(12) (D=1)
C...MSTJN(13-20) (D=0)
C...
C...
C...PARJN(1) (D=0.001)   learning parameter eta
C...PARJN(2) (D=0.5)    momentum term alfa
C...PARJN(3) (D=1.0)    overall inverse temperature beta
C...PARJN(4) (D=0.1)    width of initial weights
C...PARJN(5) (D=-1.0)   pruning parameter (<0 => prunig is turned off)
C...PARJN(6-20) not used
C...
C...
C...TINV(I) (D=0.0)    inverse temperature of layer I (if 0 use PARJN(3))
C...
C...IGFN(I) (D=0)      sigmoid function for layer I (if 0 use MSTJN(3))
C...
C...End of description


      DATA MSTJN/3,10,1,0,0,6,0,0,0,16,8,1,8*0/
      DATA PARJN/0.001,0.5,1.0,0.1,-1.0,15*0.0/
      DATA TINV/10*0.0/
      DATA IGFN/10*0/
      DATA O/MAXV*0.0/
      DATA A/MAXV*0.0/
      DATA T/MAXV*0.0/
      DATA D/MAXV*0.0/
      DATA DT/MAXV*0.0/
      DATA NSELF/MAXM*1/
      DATA W/MAXM*0.0/
      DATA DW/MAXM*0.0/
