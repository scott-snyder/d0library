C----------------------------------------------------------------------
C-
C-   Created  25-JAN-1991   Harrison B. Prosper; JETNET common block
C-   Updated  26-APR-1993   Harrison B. Prosper  
C-   Updated  24-JAN-1994   Chip Stewart   
C-   Upgrade to JETNET V3.0 
C-   Updated  10-MAR-1995   Harrison B. Prosper  
C-        Add pattern weight PWT 
C-        - (make it parjn(40) so avoid common block clobbering
C.../JNDAT1/:
C...Feed-forward net:
C...MSTJN(1) (D=3)      number of layers in net
C...MSTJN(2) (D=10)     number of patterns per update in JNTRAL
C...MSTJN(3) (D=1)      overall transfer function used in net
C...        1 -> g(x)=1/(1+exp(-2x))
C...        2 -> g(x)=tanh(x)
C...        3 -> g(x)=exp(x) (only used internally for Potts-nodes)
C...        4 -> g(x)=x
C...        5 -> g(x)=1/(1+exp(-2x)) (only used internally for
C...             entropy error)
C...MSTJN(4) (D=0)      error measure
C...       -1 -> log-squared error:     E = -log(1-(o-t)**2)
C...        0 -> summed square error:   E = 0.5*(o-t)**2
C...        1 -> entropy error:         E = -t*log(o) + (1-t)*log(1-o)
C...      >=2 -> Kullback measure, using Potts nodes of dimension 
C...             MSTJN(4):              E = t*log(t/o)
C...MSTJN(5) (D=0)      updating procedure
C...        0 -> standard Back-Propagation updating
C...        1 -> Manhattan updating
C...        2 -> Langevin updating
C...        3 -> Quickprop
C...        4 -> Conjugate Gradient - Polak-Ribiere
C...        5 -> Conjugate Gradient - Hestenes-Stiefel
C...        6 -> Conjugate Gradient - Fletcher-Reeves
C...        7 -> Conjugate Gradient - Shanno
C...        8 -> Terminate Conjugate Gradient search
C...        9 -> No updating
C...       10 -> Scaled Conjugate Gradient - Polak-Ribiere
C...       11 -> Scaled Conjugate Gradient - Hestenes-Stiefel
C...       12 -> Scaled Conjugate Gradient - Fletcher-Reeves
C...       13 -> Scaled Conjugate Gradient - Shanno
C...       14 -> Terminate Scaled Conjugate Gradient Search
C...       15 -> Rprop
C...MSTJN(6) (D=6)      file number for output statistics
C...MSTJN(7) (I)        number of calls to JNTRAL
C...MSTJN(8) (I)        initialization done -> 0 = no
C...MSTJN(9) (D=100)    number of updates per epoch 
C...MSTJN(10+I)         number of nodes in layer I (I=0 => input layer)
C...MSTJN(10) (D=16)
C...MSTJN(11) (D=8)
C...MSTJN(12) (D=1)
C...MSTJN(13-20) (D=0)
C...MSTJN(21) (D=0)     pruning (>0 -> on)
C...MSTJN(22) (D=0)     saturation measure (<>0 -> on)
C...                    <0 -> update temperature to give measure ~0.5
C...MSTJN(23,24) (D=0)  geometry of input nodes for receptive fields
C...MSTJN(25,26) (D=0)  geometry of receptive fields
C...MSTJN(27)    (D=1)  number of hidden nodes per receptive field
C...MSTJN(28-30) (D=0)  precision in bits (0 -> machine precision) for
C...                    sigmoid functions (28), thresholds (29) and
C...                    weights (30)
C...MSTJN(31)    (D=1)  Warning procedure
C...        0 -> No action is taken after a warning
C...        1 -> The execution is stopped after the program 
C...             has experienced MSTJN(32) warnings
C...             in any case only MSTJN(32) warning messages are printed
C...             out.
C...MSTJN(32) (D=10)    Maximum number of warning messages to be  
C...                    printed. As described above.
C...MSTJN(33) (I)       code for latest warning issued by the program.
C...MSTJN(34) (I)       Number of warnings issued by the program so far.
C...MSTJN(35) (D=10)    Max. number of iterations allowed in line search.
C...MSTJN(36) (D=10)    Max. number of allowed restarts in line search.
C...MSTJN(37) (I)       Status of line search
C...        0 -> Minimum found
C...        1 -> Searching for minimum
C...MSTJN(38) (I)       Number of restarts in Quickprop/ConjGr/ScConjGr
C...MSTJN(39) (I)       Number of calls to JNHESS.
C...MSTJN(40)           not used
C...
C...
C...PARJN(1) (D=0.001)  learning parameter eta
C...PARJN(2) (D=0.5)    momentum term alfa
C...PARJN(3) (D=1.0)    overall inverse temperature beta
C...PARJN(4) (D=0.1)    width of initial weights
C...        > 0 -> [-width,+width]
C...        < 0 -> [0,+width]
C...PARJN(5) (D=0.0)    forgetting parameter epsilon
C...PARJN(6) (D=0.0)    noise width in Langevin equation
C...PARJN(7) (R)        last error per node
C...PARJN(8) (R)        mean error in last update
C...PARJN(9) (R)        mean error last epoch (equal to MSTJN(9) updates)
C...PARJN(10)(R)        weighted mean average used in pruning
C...PARJN(11) (D=1.0)   change in eta (scale factor per epoch)
C...        > 0 -> Geometric with "bold driver" dynamics
C...        < 0 -> Geometric decrease of eta
C...PARJN(12) (D=1.0)   change in momentum alpha (scale factor per epoch)
C...PARJN(13) (D=1.0)   change in temperature (scale factor per epoch)
C...PARJN(14) (D=0.0)   pruning parameter lambda
C...PARJN(15) (D=1.E-6) change in lambda
C...PARJN(16) (D=0.9)   parameter gamma used for calculation of PARJN(10)
C...PARJN(17) (D=0.9)   pruning "cut-off"
C...PARJN(18) (D=1.0)   scale parameter W(0), used in pruning
C...PARJN(19) (D=0.0)   target error when pruning
C...PARJN(20) (D=1.0)   decrease in Langevin noise (scale factor per epoch)
C...PARJN(21) (D=1.75)  maximum scale for Quickprop updating
C...PARJN(22) (D=1000.) maximum allowed size of weights in Quickprop
C...PARJN(23) (D=0.0)   constant added to g'(x) to avoid 'flat spot'
C...PARJN(24) (D=0.1)   line search convergence parameter (0 < ... < 1)
C...PARJN(25) (D=0.05)  tolerance of minimum in line search
C...PARJN(26) (D=0.001) minimum allowed change in error in line search
C...PARJN(27) (D=2.0)   maximum allowed step size in line search
C...PARJN(28) (D=1.E-4) constant sigma_0 used in SCG
C...PARJN(29) (D=1.E-6) initial value for lambda in SCG
C...PARJN(30) (D=1.2)   scale-up factor used in Rprop
C...PARJN(31) (D=0.5)   scale-down factor used in Rprop
C...PARJN(32) (D=50.)   maximum scale-up factor in Rprop
C...PARJN(33) (D=1.E-6) minimum scale-down factor in Rprop
C...PARJN(34-40)        not used
C...
C...
C...Self-organizing net:
C...MSTJM(1)   (D=1)    number of dimensions in net
C...MSTJM(2)   (D=0)    symmetry of initial weights
C...        0 -> [0,+width]
C...        1 -> [-width,+width]
C...MSTJM(3)   (D=2)    response function
C...        1 -> g(x)=0.5*(1.0+tanh(x) : for normalized data
C...        2 -> g(x)=exp(-x)          : for unnormalized data
C...MSTJM(4)   (D=1)    error measure
C...        1 -> summed square error
C...MSTJM(5)   (D=0)    updating procedure
C...        0 -> unsupervized clustering & topological ordering
C...        1 -> Learning Vector Quantization (LVQ 1)
C...        2 -> as 1, but with neighborhood function.
C...MSTJM(6)   (D=6)     output file number
C...MSTJM(7)   (D=0)     normalize weights or not
C...        0 -> unnormalized
C...        1 -> normalized
C...MSTJM(8) (I)         initialization done
C...MSTJM(9)   (D=0)     neighbourhood size
C...        0< -> square neighbourhood
C...        <0 -> circular neighbourhood
C...MSTJM(10)  (D=8)     number of input nodes
C...MSTJM(11)  (D=10)    number of nodes in dimension 1.
C...        <0 -> periodic boundary
C...MSTJM(12)  (D=1)     number of nodes in dimension 2.
C...        <0 -> periodic boundary
C...MSTJM(13-20)         not used
C...
C...
C...PARJM(1)   (D=0.001) learning parameter eta
C...PARJM(2)   (D=0.0)   not used
C...PARJM(3)   (D=0.01)  overall inverse temperature beta
C...PARJM(4)   (D=0.5)   initial width of weights
C...PARJM(5-20)          not used
C...
C...
C-
C----------------------------------------------------------------------
      INTEGER MAXI, MAXO
      PARAMETER(MAXI = 1000, MAXO = 1000)
      INTEGER MSTJN, MSTJM, MXNDJM
      REAL    PARJN, OIN, OUT, PARJM
      COMMON /JNDAT1/ MSTJN(40),PARJN(40),MSTJM(20),PARJM(20),
     &                OIN(MAXI),OUT(MAXO),MXNDJM
