C
C     The following code was excerpted from: initcb.f
C
      SUBROUTINE INITCB(TOLIN)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION TOLIN
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Initialize global variables in common blocks
C        GERROR, GCONST, and GPRINT. The latter is used for
C        printing debugging information.
C
C     Input parameters:
C	 TOLIN - relative tolerance used to determine TOL
C
C     Output parameters in common blocks:
C        IERR - error code, initialized to 0
C        PI - ACOS(-1.0D0)
C        TOL - relative tolerance MAX(TOLIN,100.0D0*EPS) where
C              EPS is approximation to machine epsilon
C        IPRT - standard output unit 6
C        MSGLVL - message level, initialized to 0
C
      INTEGER IERR,IPRT,MSGLVL
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      COMMON /GPRINT/ IPRT,MSGLVL
      SAVE /GERROR/,/GCONST/,/GPRINT/
C
      DOUBLE PRECISION EPS,EPSP1
C
      IERR = 0
      PI = ACOS(-1.0D0)
      EPS = 1.0D0
   10 CONTINUE
	 EPS = EPS/2.0D0
	 EPSP1 = 1.0D0 + EPS
      IF (EPSP1 .GT. 1.0D0) GO TO 10
      TOL = MAX(TOLIN,100.0D0*EPS)
      IPRT = 6
      MSGLVL = 0
      END
C
C     The following code was excerpted from: rotiar.f
C
      SUBROUTINE ROTIAR(N,ARR,SHIFT)
      IMPLICIT LOGICAL (A-Z)
      INTEGER N,SHIFT
      INTEGER ARR(0:N-1)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Rotate elements of integer array.
C
C     Input parameters:
C	 N - number of elements of array
C	 ARR(0:N-1) - integer array
C        SHIFT - amount of (left) shift or rotation; ARR(SHIFT) on input
C              becomes ARR(0) on output
C
C     Updated parameters:
C	 ARR(0:N-1) - rotated integer array
C
      INTEGER A,B,I,J,K,L,M,R,SH,T
C
      SH = MOD(SHIFT,N)
      IF (SH .LT. 0) SH = SH + N
      IF (SH .EQ. 0) RETURN
      A = N
      B = SH
   20 CONTINUE
	 R = MOD(A,B)
	 A = B
	 B = R
      IF (R .GT. 0) GO TO 20
      M = N/A - 1
      DO 40 I = 0,A-1
	 T = ARR(I)
	 K = I
	 DO 30 J = 1,M
	    L = K + SH
	    IF (L .GE. N) L = L - N
	    ARR(K) = ARR(L)
	    K = L
   30    CONTINUE
	 ARR(K) = T
   40 CONTINUE
      END
