C
C     The following code was excerpted from: angle.f
C
      DOUBLE PRECISION FUNCTION ANGLE(XA,YA,XB,YB,XC,YC)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION XA,XB,XC,YA,YB,YC
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Compute the interior angle (in radians) at vertex
C        (XB,YB) of the chain formed by the directed edges from
C        (XA,YA) to (XB,YB) to (XC,YC) - the interior is to the
C        left of the two directed edges.
C
C     Input parameters:
C        XA,YA, XB,YB, XC,YC - vertex coordinates
C
C     Returned function value:
C        ANGLE - angle between 0 and 2*PI (PI/2 in degenerate case)
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      DOUBLE PRECISION T,X1,X2,Y1,Y2
      DOUBLE PRECISION SIGNUS
C
      X1 = XA - XB
      Y1 = YA - YB
      X2 = XC - XB
      Y2 = YC - YB
      T = SQRT((X1**2 + Y1**2)*(X2**2 + Y2**2))
      IF (T .EQ. 0.0D0) T = 1.0D0
      T = (X1*X2 + Y1*Y2)/T
C
C     Eliminate the call to sign to avoid using the fortran math library
C     IF (ABS(T) .GT. 1.0D0 - TOL) T = SIGN(1.0D0,T)
C
      SIGNUS = -1.0D0
      IF (T .GT. 0.0D0) SIGNUS = 1.0D0
C
      IF (ABS(T) .GT. 1.0D0 - TOL) T = SIGNUS
      ANGLE = ACOS(T)
      IF (X2*Y1 - Y2*X1 .LT. 0.0D0) ANGLE = 2.0D0*PI - ANGLE
      END
C
C     The following code was excerpted from: areapg.f
C
      DOUBLE PRECISION FUNCTION AREAPG(NVRT,XC,YC)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NVRT
      DOUBLE PRECISION XC(NVRT),YC(NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Compute twice the signed area of a simple polygon with
C        vertices given in circular (CCW or CW) order.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of polygon (>= 3)
C	 XC(1:NVRT),YC(1:NVRT) - vertex coordinates in CCW or CW order
C
C     Returned function value:
C        AREAPG - twice the signed area of polygon, positive if CCW
C
      INTEGER I
      DOUBLE PRECISION SUM
C
      SUM = XC(1)*(YC(2) - YC(NVRT)) + XC(NVRT)*(YC(1) - YC(NVRT-1))
      DO 10 I = 2,NVRT-1
	 SUM = SUM + XC(I)*(YC(I+1) - YC(I-1))
   10 CONTINUE
      AREAPG = SUM
      END
C
C     The following code was excerpted from: areatr.f
C
      DOUBLE PRECISION FUNCTION AREATR(XA,YA,XB,YB,XC,YC)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION XA,XB,XC,YA,YB,YC
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Compute twice the signed area of the triangle with
C         vertices (XA,YA), (XB,YB), and (XC,YC) in CCW or CW order.
C
C     Input parameters:
C        XA,YA, XB,YB, XC,YC - vertex coordinates
C
C     Returned function value:
C        AREATR - twice the signed area of triangle, positive if CCW
C
      AREATR = (XB - XA)*(YC - YA) - (XC - XA)*(YB - YA)
      END
C
C     The following code was excerpted from: diaedg.f
C
      INTEGER FUNCTION DIAEDG(X0,Y0,X1,Y1,X2,Y2,X3,Y3)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION X0,X1,X2,X3,Y0,Y1,Y2,Y3
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine whether 02 or 13 is the diagonal edge chosen
C        based on the circumcircle criterion, where (X0,Y0), (X1,Y1),
C        (X2,Y2), (X3,Y3) are the vertices of a simple quadrilateral
C        in counterclockwise order.
C
C     Input parameters:
C        X0,Y0, X1,Y1, X2,Y2, X3,Y3 - vertex coordinates
C
C     Returned function value:
C        DIAEDG -  1 if diagonal edge 02 is chosen, i.e. 02 is inside
C                  quadrilateral + vertex 3 is outside circumcircle 012
C                 -1 if diagonal edge 13 is chosen, i.e. 13 is inside
C                  quadrilateral + vertex 0 is outside circumcircle 123
C                  0 if four vertices are cocircular
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      DOUBLE PRECISION CA,CB,DX10,DX12,DX30,DX32,DY10,DY12,DY30,DY32
      DOUBLE PRECISION S,TOLA,TOLB
C
      DX10 = X1 - X0
      DY10 = Y1 - Y0
      DX12 = X1 - X2
      DY12 = Y1 - Y2
      DX30 = X3 - X0
      DY30 = Y3 - Y0
      DX32 = X3 - X2
      DY32 = Y3 - Y2
      TOLA = TOL*MAX(ABS(DX10),ABS(DY10),ABS(DX30),ABS(DY30))
      TOLB = TOL*MAX(ABS(DX12),ABS(DY12),ABS(DX32),ABS(DY32))
      CA = DX10*DX30 + DY10*DY30
      CB = DX12*DX32 + DY12*DY32
      IF (CA .GT. TOLA .AND. CB .GT. TOLB) THEN
	 DIAEDG = -1
      ELSE IF (CA .LT. -TOLA .AND. CB .LT. -TOLB) THEN
	 DIAEDG = 1
      ELSE
	 TOLA = MAX(TOLA,TOLB)
	 S = (DX10*DY30 - DX30*DY10)*CB + (DX32*DY12 - DX12*DY32)*CA
	 IF (S .GT. TOLA) THEN
	    DIAEDG = -1
	 ELSE IF (S .LT. -TOLA) THEN
	    DIAEDG = 1
	 ELSE
	    DIAEDG = 0
	 ENDIF
      ENDIF
      END
C
C     The following code was excerpted from: diam2.f
C
      SUBROUTINE DIAM2(NVRT,XC,YC,I1,I2,DIAMSQ)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,NVRT
      DOUBLE PRECISION DIAMSQ,XC(NVRT),YC(NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Find the diameter of a convex polygon with vertices
C        given in CCW order and with all interior angles < PI.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(1:NVRT),YC(1:NVRT) - vertex coordinates in CCW order
C
C     Output parameters:
C        I1,I2 - indices in XC,YC of diameter edge; diameter is from
C              (XC(I1),YC(I1)) to (XC(I2),YC(I2))
C        DIAMSQ - square of diameter
C
C     Abnormal return:
C        IERR is set to 200
C
C     Routines called:
C        AREATR
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER J,JP1,K,KP1,M
      DOUBLE PRECISION AREATR
      DOUBLE PRECISION AREA1,AREA2,C1MTOL,C1PTOL,DIST

C
C     Find first vertex which is farthest from edge connecting
C     vertices with indices NVRT, 1.
C
      C1MTOL = 1.0D0 - TOL
      C1PTOL = 1.0D0 + TOL
      J = NVRT
      JP1 = 1
      K = 2
      AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
   10 CONTINUE
         AREA2 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K+1),YC(K+1))
         IF (AREA2 .GT. AREA1*C1PTOL) THEN
   	    AREA1 = AREA2
   	    K = K + 1
   	    GO TO 10
         ENDIF
      M = K
      DIAMSQ = 0.0D0
C
C     Find diameter = maximum distance of antipodal pairs.
C
      AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
   20 CONTINUE
	 KP1 = K + 1
	 IF (KP1 .GT. NVRT) KP1 = 1
         AREA2 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(KP1),YC(KP1))
	 IF (AREA2 .GT. AREA1*C1PTOL) THEN
	    K = K + 1
	    AREA1 = AREA2
	 ELSE IF (AREA2 .LT. AREA1*C1MTOL) THEN
	    J = JP1
	    JP1 = J + 1
	    AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
	 ELSE
	    K = K + 1
	    J = JP1
	    JP1 = J + 1
	    AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
	 ENDIF
	 IF (J .GT. M .OR. K .GT. NVRT) THEN
	    IERR = 200
	    RETURN
	 ENDIF
	 DIST = (XC(J) - XC(K))**2 + (YC(J) - YC(K))**2
	 IF (DIST .GT. DIAMSQ) THEN
	    DIAMSQ = DIST
	    I1 = J
	    I2 = K
	 ENDIF
      IF (J .NE. M .OR. K .NE. NVRT) GO TO 20
      END
C
C     The following code was excerpted from: lrline.f
C
      INTEGER FUNCTION LRLINE(XU,YU,XV1,YV1,XV2,YV2,DV)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION DV,XU,XV1,XV2,YU,YV1,YV2
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine whether a point is to the left of, right of,
C        or on a directed line parallel to a line through given points.
C
C     Input parameters:
C        XU,YU, XV1,YV1, XV2,YV2 - vertex coordinates; the directed
C              line is parallel to and at signed distance DV to the
C              left of the directed line from (XV1,YV1) to (XV2,YV2);
C              (XU,YU) is the vertex for which the position
C              relative to the directed line is to be determined
C        DV - signed distance (positive for left)
C
C     Returned function value:
C        LRLINE - +1, 0, or -1 depending on whether (XU,YU) is
C              to the right of, on, or left of the directed line
C              (0 if line degenerates to a point)
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      DOUBLE PRECISION DX,DXU,DY,DYU,T,TOLABS
      DOUBLE PRECISION SIGNUS
C
      DX = XV2 - XV1
      DY = YV2 - YV1
      DXU = XU - XV1
      DYU = YU - YV1
      TOLABS = TOL*MAX(ABS(DX),ABS(DY),ABS(DXU),ABS(DYU),ABS(DV))
      T = DY*DXU - DX*DYU
      IF (DV .NE. 0.0D0) T = T + DV*SQRT(DX**2 + DY**2)
C
C     Eliminate the call to sign to avoid using the fortran math library
C     LRLINE = INT(SIGN(1.0D0,T))
C
      SIGNUS = -1.0D0
      IF (T .GT. 0.0D0) SIGNUS = 1.0D0
C
      LRLINE = INT(SIGNUS)
      IF (ABS(T) .LE. TOLABS) LRLINE = 0
      END
C
C     The following code was excerpted from: shrnk2.f
C
      SUBROUTINE SHRNK2(NVRT,XC,YC,SDIST,NSHR,XS,YS,IEDGE)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NSHR,NVRT
      INTEGER IEDGE(0:NVRT)
      DOUBLE PRECISION SDIST(0:NVRT-1)
      DOUBLE PRECISION XC(0:NVRT),YC(0:NVRT),XS(0:NVRT),YS(0:NVRT)
C
C     Purpose: Shrink a convex polygon, with vertices given in CCW
C        order and with all interior angles < PI, by distance SDIST(I)
C        for Ith edge, I = 0,...,NVRT-1.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(0:NVRT),YC(0:NVRT) - vertex coordinates in CCW order;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        SDIST(0:NVRT-1) - nonnegative shrink distances for edges
C
C     Output parameters:
C        NSHR - number of vertices on boundary of shrunken polygon;
C              0 if shrunken polygon is empty else 3 <= NSHR <= NVRT
C        XS(0:NSHR),YS(0:NSHR) - coordinates of shrunken polygon in CCW
C              order if NSHR > 0; (XS(0),YS(0)) = (XS(NSHR),YS(NSHR))
C        IEDGE(0:NVRT) - indices of edges of shrunken polygon in
C              range from 0 to NVRT-1
C
C     Abnormal return:
C        IERR is set to 202
C
C     Routines called:
C        LRLINE, XLINE
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER LRLINE
      INTEGER I,J,K,LR
      DOUBLE PRECISION ALPHA,PI2,THETA
      LOGICAL FIRST,PARALL
C
      PI2 = 2.0D0*PI
      ALPHA = ATAN2(YC(1)-YC(0),XC(1)-XC(0))
      CALL XLINE(XC(0),YC(0),XC(1),YC(1),XC(1),YC(1),XC(2),YC(2),
     $   SDIST(0),SDIST(1),XS(1),YS(1),PARALL)
      IF (PARALL) THEN
	 IERR = 202
	 GO TO 90
      ENDIF
      IEDGE(0) = 0
      IEDGE(1) = 1
      I = 2
      J = 0
      NSHR = 1
      FIRST = .TRUE.
C
C     First while loop processes edges subtending angle <= PI
C     with respect to first edge.
C
   10 CONTINUE
      THETA = ATAN2(YC(I+1)-YC(I),XC(I+1)-XC(I)) - ALPHA
      IF (THETA .LT. 0.0D0) THETA = THETA + PI2
      IF (THETA .GT. PI + TOL) GO TO 40
   20    CONTINUE
	    LR = LRLINE(XS(NSHR),YS(NSHR),XC(I),YC(I),XC(I+1),YC(I+1),
     $         SDIST(I))
	    IF (LR .LT. 0) GO TO 30
	    NSHR = NSHR - 1
	    IF (NSHR .GE. 1) GO TO 20
   30    CONTINUE
	 IF (NSHR .LT. 1 .AND. ABS(THETA - PI) .LE. TOL) GO TO 90
	 K = IEDGE(NSHR)
	 NSHR = NSHR + 1
	 CALL XLINE(XC(K),YC(K),XC(K+1),YC(K+1),XC(I),YC(I),XC(I+1),
     $      YC(I+1),SDIST(K),SDIST(I),XS(NSHR),YS(NSHR),PARALL)
	 IF (PARALL) THEN
	    IERR = 202
	    GO TO 90
	 ENDIF
	 IEDGE(NSHR) = I
	 I = I + 1
      GO TO 10
C
C     Second while loop processes remaining edges.
C
   40 CONTINUE
	 IF (FIRST) THEN
	    FIRST = .FALSE.
	    GO TO 50
	 ENDIF
	 LR = LRLINE(XS(J),YS(J),XC(I),YC(I),XC(I+1),YC(I+1),SDIST(I))
	 IF (LR .LE. 0) GO TO 70
   50       CONTINUE
	       IF (NSHR .LE. J) GO TO 90
	       LR = LRLINE(XS(NSHR),YS(NSHR),XC(I),YC(I),XC(I+1),
     $            YC(I+1),SDIST(I))
	       IF (LR .GE. 0) THEN
		  NSHR = NSHR - 1
		  GO TO 50
	       ENDIF
	    K = IEDGE(NSHR)
	    NSHR = NSHR + 1
	    CALL XLINE(XC(K),YC(K),XC(K+1),YC(K+1),XC(I),YC(I),XC(I+1),
     $         YC(I+1),SDIST(K),SDIST(I),XS(NSHR),YS(NSHR),PARALL)
	    IF (PARALL) THEN
	       IERR = 202
	       GO TO 90
	    ENDIF
	    IEDGE(NSHR) = I
   60       CONTINUE
	    LR = LRLINE(XS(J+1),YS(J+1),XC(I),YC(I),XC(I+1),YC(I+1),
     $         SDIST(I))
	    IF (LR .GE. 0) THEN
	       J = J + 1
	       GO TO 60
	    ENDIF
	    K = IEDGE(J)
	    CALL XLINE(XC(K),YC(K),XC(K+1),YC(K+1),XC(I),YC(I),XC(I+1),
     $         YC(I+1),SDIST(K),SDIST(I),XS(J),YS(J),PARALL)
	    IF (PARALL) THEN
	       IERR = 202
	       GO TO 90
	    ENDIF
	    XS(NSHR+1) = XS(J)
	    YS(NSHR+1) = YS(J)
	    IEDGE(NSHR+1) = IEDGE(J)
   70    CONTINUE
	 I = I + 1
      IF (I .LT. NVRT) GO TO 40
C
      IF (J .GT. 0) THEN
	 DO 80 I = 0,NSHR+1-J
	    XS(I) = XS(I+J)
	    YS(I) = YS(I+J)
	    IEDGE(I) = IEDGE(I+J)
   80    CONTINUE
      ENDIF
      NSHR = NSHR + 1 - J
      RETURN
C
   90 CONTINUE
      NSHR = 0
      RETURN
      END
C
C     The following code was excerpted from: width2.f
C
      SUBROUTINE WIDTH2(NVRT,XC,YC,I1,I2,WIDSQ)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,NVRT
      DOUBLE PRECISION WIDSQ,XC(NVRT),YC(NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Find the width (minimum breadth) of a convex polygon with
C        vertices given in CCW order and with all interior angles < PI.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(1:NVRT),YC(1:NVRT) - vertex coordinates in CCW order
C
C     Output parameters:
C        I1,I2 - indices in XC,YC such that width is from vertex
C              (XC(I1),YC(I1)) to line joining (XC(I2),YC(I2)) and
C              (XC(I2+1),YC(I2+1)), where index NVRT+1 is same as 1
C        WIDSQ - square of width
C
C     Abnormal return:
C        IERR is set to 201
C
C     Routines called:
C        AREATR
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER A,B,C,J,JP1,K,KP1,M
      DOUBLE PRECISION AREATR
      DOUBLE PRECISION AREA1,AREA2,C1MTOL,C1PTOL,DIST,DX,DY
C
C     Find first vertex which is farthest from edge connecting
C     vertices with indices NVRT, 1.
C
      C1MTOL = 1.0D0 - TOL
      C1PTOL = 1.0D0 + TOL
      J = NVRT
      JP1 = 1
      K = 2
      AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
   10 CONTINUE
         AREA2 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K+1),YC(K+1))
         IF (AREA2 .GT. AREA1*C1PTOL) THEN
   	    AREA1 = AREA2
   	    K = K + 1
   	    GO TO 10
         ENDIF
      M = K
      WIDSQ = 0.0D0
C
C     Find width = min distance of antipodal edge-vertex pairs.
C
      AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
   20 CONTINUE
	 KP1 = K + 1
	 IF (KP1 .GT. NVRT) KP1 = 1
         AREA2 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(KP1),YC(KP1))
	 IF (AREA2 .GT. AREA1*C1PTOL) THEN
	    A = J
	    B = K
	    K = K + 1
	    C = K
	    IF (C .GT. NVRT) C = 1
	    AREA1 = AREA2
	 ELSE IF (AREA2 .LT. AREA1*C1MTOL) THEN
	    A = K
	    B = J
	    C = JP1
	    J = JP1
	    JP1 = J + 1
	    AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
	 ELSE
	    A = K
	    B = J
	    C = JP1
	    K = K + 1
	    J = JP1
	    JP1 = J + 1
	    AREA1 = AREATR(XC(J),YC(J),XC(JP1),YC(JP1),XC(K),YC(K))
	 ENDIF
         IF (J .GT. M .OR. K .GT. NVRT) THEN
	    IERR = 201
	    RETURN
	 ENDIF
	 DX = XC(C) - XC(B)
	 DY = YC(C) - YC(B)
	 DIST = ((YC(A) - YC(B))*DX - (XC(A) - XC(B))*DY)**2/
     $      (DX**2 + DY**2)
	 IF (DIST .LT. WIDSQ .OR. WIDSQ .LE. 0.0D0) THEN
	    WIDSQ = DIST
	    I1 = A
	    I2 = B
	 ENDIF
      IF (J .NE. M .OR. K .NE. NVRT) GO TO 20
      END
C
C     The following code was excerpted from: xedge.f
C
      SUBROUTINE XEDGE(MODE,XV1,YV1,XV2,YV2,XW1,YW1,XW2,YW2,XU,YU,
     $   INTSCT)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MODE
      DOUBLE PRECISION XU,XV1,XV2,XW1,XW2,YU,YV1,YV2,YW1,YW2
      LOGICAL INTSCT
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine whether two edges or a ray and an edge
C        intersect and return the intersection point if they do.
C
C     Input parameters:
C        MODE - 0 for two edges, 1 (or nonzero) for a ray and an edge
C        XV1,YV1, XV2,YV2, XW1,YW1, XW2,YW2 - vertex coordinates;
C              an edge (ray) is from (XV1,YV1) to (thru) (XV2,YV2);
C              an edge joins vertices (XW1,YW1) and (XW2,YW2)
C
C     Output parameters:
C        XU,YU - coordinates of the point of intersection iff INTSCT
C              is .TRUE.
C        INTSCT - .TRUE. if the edges/ray are nondegenerate, not
C              parallel, and intersect, .FALSE. otherwise
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      DOUBLE PRECISION DENOM,DXV,DXW,DYV,DYW,T,TOLABS
C
      INTSCT = .FALSE.
      DXV = XV2 - XV1
      DYV = YV2 - YV1
      DXW = XW2 - XW1
      DYW = YW2 - YW1
      TOLABS = TOL*MAX(ABS(DXV),ABS(DYV),ABS(DXW),ABS(DYW))
      DENOM = DYV*DXW - DXV*DYW
      IF (ABS(DENOM) .LE. TOLABS) RETURN
      T = (DYV*(XV1 - XW1) - DXV*(YV1 - YW1))/DENOM
      IF (T .LT. -TOL .OR. T .GT. 1.0D0 + TOL) RETURN
      XU = XW1 + T*DXW
      YU = YW1 + T*DYW
      IF (ABS(DXV) .GE. ABS(DYV)) THEN
	 T = (XU - XV1)/DXV
      ELSE
	 T = (YU - YV1)/DYV
      ENDIF
      IF (MODE .EQ. 0) THEN
         IF (T .GE. -TOL .AND. T .LE. 1.0D0 + TOL) INTSCT = .TRUE.
      ELSE
         IF (T .GE. -TOL) INTSCT = .TRUE.
      ENDIF
      END
C
C     The following code was excerpted from: xline.f
C
      SUBROUTINE XLINE(XV1,YV1,XV2,YV2,XW1,YW1,XW2,YW2,DV,DW,
     $   XU,YU,PARALL)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION DV,DW,XU,XV1,XV2,XW1,XW2,YU,YV1,YV2,YW1,YW2
      LOGICAL PARALL
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine the intersection point of two lines parallel
C        to lines through given points.
C
C     Input parameters:
C        XV1,YV1, XV2,YV2, XW1,YW1, XW2,YW2 - vertex coordinates;
C              first line is parallel to and at signed distance DV to
C              left of directed line from (XV1,YV1) to (XV2,YV2);
C              second line is parallel to and at signed distance DW to
C              left of directed line from (XW1,YW1) to (XW2,YW2)
C        DV,DW - signed distances (positive for left)
C
C     Output parameters:
C        XU,YU - coordinates of the point of intersection iff PARALL
C              is .FALSE.
C        PARALL - .TRUE. if the lines are parallel or two points for a
C              line are identical, .FALSE. otherwise
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      DOUBLE PRECISION A11,A12,A21,A22,B1,B2,DET,TOLABS
C
      PARALL = .TRUE.
      A11 = YV2 - YV1
      A12 = XV1 - XV2
      A21 = YW2 - YW1
      A22 = XW1 - XW2
      TOLABS = TOL*MAX(ABS(A11),ABS(A12),ABS(A21),ABS(A22))
      DET = A11*A22 - A21*A12
      IF (ABS(DET) .LE. TOLABS) RETURN
      B1 = XV1*A11 + YV1*A12
      IF (DV .NE. 0.0D0) B1 = B1 - DV*SQRT(A11**2 + A12**2)
      B2 = XW1*A21 + YW1*A22
      IF (DW .NE. 0.0D0) B2 = B2 - DW*SQRT(A21**2 + A22**2)
      XU = (B1*A22 - B2*A12)/DET
      YU = (B2*A11 - B1*A21)/DET
      PARALL = .FALSE.
      END
