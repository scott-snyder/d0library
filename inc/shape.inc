      PARAMETER (MXSHAP = 20)
      COMMON/SHAPE/MINBOU(7,MXSHAP),MAXBOU(7,MXSHAP)
C
C COMMON/SHAPE/ contains the boundary values that determine
C  the shapes of detectors allowed in the simulation
C
C	MXSHAP is the maximum number of shapes allowed
C
C       MINBOU(1,ISHAPE) is the number of planes that determine
C	the minimum surface of shape ISHAPE
C
C       MINBOU(2 ... N, ISHAPE) are the numbers of the
C	plane types that set the minimum surface
C       1 = X, 2 = Y, 3 = Z, 4 = R, 5 = PHI, 6 = THETA
C	Since there are a maximum of 6 different coordinates, there
C	is a maximum of 6 planes that determine the minimun (maximum)
C	surface of a shape.
C
C	MAXBOU(1,ISHAPE) is the number of planes that determine
C	the maximum surface of shape ISHAPE
C
C	MAXBOU(2... N, ISHAPE) are the numbers of the maxima planes 
C
