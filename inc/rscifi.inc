C
C       Include file for common block RSCIFI
C
      
      
      
      
      
      
      
      
      INTEGER RVLTYP
      PARAMETER (RVLTYP = 1)            


C Make pi available
      REAL PI
      PARAMETER( PI=3.14159265 )


C Maximum numbers of volumes and layers
      INTEGER MAXSL, MAXLAY
      PARAMETER (MAXSL=40, MAXLAY=40)
      
      
      
      INTEGER MAXVOL
      PARAMETER (MAXVOL=40)
      
      
      
      INTEGER NUMVOL

C Layer pointer ILAY0
C Volume V contains layers ILAY0(V)+1 through ILAY0(V+1)
      INTEGER ILAY0(MAXVOL)

C Layer ILG is in GEANT volume IVOL(ILG).
      INTEGER IVOL(MAXLAY)

C Volume shape numbers, # available shapes, available shape names.
C ISHAPE  CSHAPE      DESCRIPTION
C   1     cylinder    Axis along z; fibers in z-phi "plane"
C   2     disk        Axis along z; fibers in x-y plane
C   3     spiral      Axis along z; fibers in x-y plane in spiral shape
C   4     paddle      ADD COMMENT HERE
      INTEGER ISHAPE(MAXVOL)
      INTEGER NSHAPE
      PARAMETER (NSHAPE=4)
      CHARACTER*16 CSHAPE(NSHAPE)
      
      
      
      


      
      
      
C Superlayer size parameters VSIZE(IS,IV)
C ISHAPE   VSIZE(1,IV)      VSIZE(2,IV)     VSIZE(3,IV)
C   1      inner radius     outer radius    length
C   2      inner radius     outer radius    length
C   3      inner radius     outer radius    length
C   4      DX/2             DY/2            DZ/2    A LA GEANT
      REAL VSIZE(3,MAXSL)

C Position of center of superlayer (x,y,z).
      REAL VPOS(3,MAXSL)

C Description of the fiber type in layer ILG
      INTEGER IFIBTY(MAXLAY)
C Readout index for fiber
      INTEGER IRDOUT(MAXLAY)

C Fiber number offset for each layer within each superlayer.
C The fiber numbering within each superlayer begins at one [SFTDGI].
      INTEGER IFIB0(MAXLAY)

C The position of the center (or one END, depending on shape) of fiber IF in
C the layer IL relative to the center of the volume described by the array
C PFIB.  These positions are in internal coordinates, i.e. the system with
C origin at the center of the volume. [SFTDET]
C For ISHAPE = 1    r = PFIB(1,IL)
C                   phi = PFIB(3,IL)*[IF-1-PFIB(2,IL)] + PFIB(4,IL)*z
C                   PFIB(5,IL) = minimum radial extent
C                   PFIB(6,IL) = maximum radial extent
C                   PFIB(7,IL) = minimum z extent
C                   PFIB(8,IL) = maximum z extent
C                                1 = counterclockwise looking down negative z
      REAL PFIB(8,MAXLAY)

C Description of the fibers in layer IL --IFIBTY(IL) is the fiber type
C FIBER(IF,IL) defines the size and orientation of the fiber [SFTDET].
C IFIBTY(IL) x-section   FIBER(1,IL)   FIBER(2,IL)   FIBER(3,IL)
C    1       circle      spacing       diameter
C    2       square      spacing       width
C    3       rectangle   spacing       width         depth
      REAL FIBER(3,MAXLAY)


C Number of fibers in each layer [SFTDET].
      INTEGER NFIB(MAXLAY)

C Number of fibers in each superlayer [SFTDGI].
      INTEGER NFIBV(MAXSL)

C Size to spacing ratios [SFTDET].
      REAL FSPRAT(MAXLAY)

C length from one END of fiber to the other
      REAL FIBLEN(MAXLAY)

C parametrization of the distance D from (x,y,z) to one fiber end [SFTDET]
C ISHAPE    parametrization
C    1      D = [z + FIBEND(1,ILG)] * FIBEND(2,ILG)
C    2      needs to be written (if possible)
C    3      D = [FIBEND(1,ILG) - r*r] * FIBEND(2,ILG)
      REAL FIBEND( 2, MAXLAY )


      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      ! 2  IL - layer number WITHIN orientation ('local' layer number)
      
      
      
      
      
      
      
      
      REAL PLAY(4,MAXLAY)
      INTEGER IPLAY(2,MAXLAY)
      LOGICAL BPLAY(2,MAXLAY)



      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      !         ('local' orientation number)
      
      
      
      
      
      
      
      
      REAL PORI(4,MAXLAY)
      INTEGER IPORI(2,MAXLAY)
      LOGICAL BPORI(2,MAXLAY)

      
      
      
      
      CHARACTER*4 CTUSET(MAXVOL), CTUDET(MAXVOL)            
      
      
      
      INTEGER ITRVOL
      REAL XOFF(3), XSLOP(3), TOFF, TSLOP
      
      
      
      COMMON/RSCIFI/
          
          
     1    FIBER,PFIB,ILAY0,NFIB,
          
     2    PLAY,IPLAY,BPLAY, PORI,IPORI,BPORI,
          
     3    CTUSET, CTUDET, NUMVOL, VSIZE, VPOS,
     4    IFIBTY, IRDOUT, IVOL, ISHAPE, CSHAPE,
     5    FSPRAT, FIBLEN, FIBEND,
          
     6    IFIB0, NFIBV,
          
     7    ITRVOL, XOFF, XSLOP, TOFF, TSLOP
