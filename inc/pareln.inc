C
C     Include file for using with HISTogram system in ELN
C
      integer maxhis,maxsel,maxops
      parameter (maxhis=20)
      parameter (maxsel=100)
      parameter (maxops=10)
      integer maxnow,idlst
      common /hisdef/maxnow,idlst(maxhis)
      integer maxvar,varlst(2,maxsel),ioplst(maxops,maxsel),
     *        fillst(maxhis),maxiop
      character*16 varnam(maxsel)
      common /vardef/maxvar,varlst,maxiop,ioplst,fillst,varnam
      character*(maxops*4) chrlst(maxhis)
      equivalence (chrlst,ioplst)
      character*8 strlst(maxsel)
      equivalence (strlst,varlst)
