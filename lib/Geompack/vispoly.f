C
C     The following code was excerpted from: vispol.f
C
      SUBROUTINE VISPOL(XEYE,YEYE,NVRT,XC,YC,NVIS,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NVIS,NVRT
      INTEGER IVIS(0:NVRT)
      DOUBLE PRECISION XEYE,YEYE
      DOUBLE PRECISION XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Compute the visibility polygon VP from an eyepoint in
C        the interior or blocked exterior of a simple polygon P or
C        on the boundary of a simply connected polygonal region P.
C	 In the latter case, the interior angles at all vertices must
C        be strictly between 0 and 2*PI.
C
C     Input parameters:
C        XEYE,YEYE - coordinates of eyepoint; must be a simple vertex
C              if it lies on the boundary (i.e. occurs only once)
C	 NVRT - upper subscript of XC, YC (approx number of vertices)
C	 XC(0:NVRT),YC(0:NVRT) - If eyepoint is interior or blocked
C              exterior then arrays contain coordinates in CCW or CW
C              order, respectively, with (XC(0),YC(0)) = (XC(NVRT),
C              YC(NVRT)); (XC(0),YC(0)) is a vertex visible from
C              (XEYE,YEYE), e.g. as computed by routine ROTIPG.
C              If eyepoint is a vertex of P then arrays contain
C	       coordinates in CCW order; (XC(0),YC(0)) is successor
C              vertex of (XEYE,YEYE); (XC(NVRT),YC(NVRT)) is
C              predecessor vertex of (XEYE,YEYE).
C
C     Updated parameters:
C        XC(0:NVIS),YC(0:NVIS) - vertices of VP in CCW order;
C              if eyepoint is interior or blocked exterior then
C              (XC(0),YC(0)) = (XC(NVIS),YC(NVIS)), else (XC(0),YC(0))
C              and (XC(NVIS),YC(NVIS)) are the successor and
C              predecessor vertices of (XEYE,YEYE) in VP
C
C     Output parameters:
C	 NVIS - upper subscript of XC, YC on output (approx number
C              of vertices of VP); NVIS <= NVRT
C        IVIS(0:NVIS) - contains information about the vertices of VP
C              w.r.t. the vertices of P; IVIS(I) = K if (XC(I),YC(I))
C              is the vertex of index K in the input polygon; IVIS(I)
C              = -K if (XC(I),YC(I)) is on the interior of the edge
C              joining vertices of index K-1 and K in input polygon
C
C     Note about algorithm:
C        On input, XC and YC contain vertex coordinates of P. During
C        the algorithm, part of XC, YC is used as a stack, which, on
C        output, contains the vertex coordinates of VP. The stack
C        vertices overwrite the input vertices as the input vertices
C        are scanned. Elements of IVIS are set when vertices are added
C        to the stack; these values may have +NV or -NV added to them
C        to indicate that stack point has same angle as previous one.
C
C     Reference: 
C        B. Joe and R. B. Simpson, BIT 27 (1987), pp. 458-473.
C
C     Abnormal return:
C        IERR is set to 206, 207, 208, 209, or 210
C
C     Routines called:
C        LRLINE, VPLEFT, VPRGHT, VPSCNA, VPSCNB, VPSCNC, VPSCND
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GVPVAR/
C
C     Variables in common block GVPVAR:
C        NV - NVRT
C        OPER - operation code 1 to 7 for LEFT, RIGHT, SCANA, SCANB,
C              SCANC, SCAND, FINISH
C        CUR - index of current vertex of P in XC, YC arrays
C        TOP - index of top vertex of stack in XC, YC arrays
C              (TOP <= CUR is always satisfied)
C        XE,YE - XEYE,YEYE
C        XW,YW - coordinates of point on last or second-last edge
C              processed (needed for routines VPSCNB, VPSCNC, VPSCND)
C        BEYE - .TRUE. iff eyepoint is on boundary
C
      INTEGER I,LR,LRLINE
C
      BEYE = XC(0) .NE. XC(NVRT) .OR. YC(0) .NE. YC(NVRT)
      NV = NVRT
      XE = XEYE
      YE = YEYE
      IVIS(0) = 0
      CUR = 1
      IF (.NOT. BEYE) GO TO 20
   10 CONTINUE
	 LR = LRLINE(XC(NV-1),YC(NV-1),XE,YE,XC(NV),YC(NV),0.0D0)
	 IF (LR .EQ. 0) THEN
	    NV = NV - 1
	    GO TO 10
	 ENDIF
C
   20 CONTINUE
         LR = LRLINE(XC(CUR),YC(CUR),XE,YE,XC(0),YC(0),0.0D0)
	 IF (LR .EQ. 0) THEN
	    CUR = CUR + 1
	    GO TO 20
	 ENDIF
      IF (LR .EQ. -1) THEN
	 OPER = 1
	 IF (CUR .EQ. 1) THEN
            TOP = 1
            IVIS(1) = CUR
	 ELSE IF (BEYE) THEN
	    TOP = 1
	    XC(0) = XC(CUR-1)
	    YC(0) = YC(CUR-1)
	    IVIS(0) = CUR - 1
	    XC(1) = XC(CUR)
	    YC(1) = YC(CUR)
	    IVIS(1) = CUR
	 ELSE
	    TOP = 2
	    XC(1) = XC(CUR-1)
	    YC(1) = YC(CUR-1)
	    IVIS(1) = CUR - 1 + NV
	    XC(2) = XC(CUR)
	    YC(2) = YC(CUR)
	    IVIS(2) = CUR
	 ENDIF
      ELSE
	 OPER = 3
	 TOP = 0
	 IF (BEYE .AND. CUR. GT. 1) THEN
	    XC(0) = XC(CUR-1)
	    YC(0) = YC(CUR-1)
	    IVIS(0) = CUR - 1
	 ENDIF
      ENDIF
C
C     Angular displacement of stack points are in nondecreasing order,
C     with at most two consecutive points having the same displacement.
C
   30 CONTINUE
	 IF (OPER .EQ. 1) THEN
	    CALL VPLEFT(XC,YC,IVIS)
	 ELSE IF (OPER .EQ. 2) THEN
	    CALL VPRGHT(XC,YC,IVIS)
	 ELSE IF (OPER .EQ. 3) THEN
	    CALL VPSCNA(XC,YC,IVIS)
	 ELSE IF (OPER .EQ. 4) THEN
	    CALL VPSCNB(XC,YC,IVIS)
	 ELSE IF (OPER .EQ. 5) THEN
	    CALL VPSCNC(XC,YC,IVIS)
	 ELSE
	    CALL VPSCND(XC,YC,IVIS)
	 ENDIF
	 IF (IERR .NE. 0) THEN
	    NVIS = TOP
	    RETURN
	 ENDIF
      IF (OPER .LE. 6) GO TO 30
C
C     Add or subtract NV from those IVIS values which are used to
C     indicate that stack point has same angle as previous one.
C
      DO 40 I = 1,TOP
	 IF (IVIS(I) .GT. NV) THEN
	    IVIS(I) = IVIS(I) - NV
	 ELSE IF (IVIS(I) .LT. -NV) THEN
	    IVIS(I) = IVIS(I) + NV
	 ENDIF
   40 CONTINUE
      NVIS = TOP
      END
C
C     The following code was excerpted from: visvrt.f
C
      SUBROUTINE VISVRT(ANGSPC,XEYE,YEYE,NVIS,XC,YC,IVIS,MAXN,NVSVRT,
     $   THETA)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXN,NVIS,NVSVRT
      INTEGER IVIS(0:MAXN)
      DOUBLE PRECISION ANGSPC,XEYE,YEYE
      DOUBLE PRECISION THETA(0:MAXN),XC(0:MAXN),YC(0:MAXN)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine a list of visible vertices, ordered by
C        increasing "polar angle", on the boundary of the visibilty
C        polygon from boundary eyepoint (XEYE,YEYE). This list
C        includes the vertices of visibility polygon such that a
C        line segment from (XEYE,YEYE) to vertex lies in interior
C        of polygon, as well as extra points on edges which subtend
C        an angle >= 2*ANGSPC at (XEYE,YEYE). These extra points are
C        at an equal angular spacing of >= ANGSPC and < 2*ANGSPC. The
C        successor and predecessor of eyepoint are included in list.
C
C     Input parameters:
C        ANGSPC - angle spacing parameter in radians which controls
C              how many extra points become visible vertices
C        XEYE,YEYE - coordinates of boundary eyepoint
C        NVIS - (number of vertices of visibility polygon) - 2
C        XC(0:NVIS),YC(0:NVIS) - the coordinates of the vertices of
C              visibility polygon in CCW order; (XC(0),YC(0)) and
C              (XC(NVIS),YC(NVIS)) are the successor and predecessor
C              vertices of eyepoint in visibility polygon; at most 2
C              consecutive vertices have same polar angle wrt eyepoint
C        IVIS(0:NVIS) - contains information about the vertices of
C              XC, YC arrays with respect to the original polygon from
C              which visibility polygon is computed; if IVIS(I) >= 0
C              then (XC(I),YC(I)) has index I in original polygon;
C              if IVIS(I) < 0 then (XC(I),YC(I)) is on the edge
C              ending at vertex of index -IVIS(I) in original polygon;
C              indexing starts at 0 from successor of eyepoint
C        MAXN - upper bound on NVSVRT; should be at least
C              NVIS + INT(PHI/ANGSPC) where PHI is the interior
C              angle at (XEYE,YEYE)
C
C     Updated parameters:
C        XC(0:NVSVRT),YC(0:NVSVRT) - coordinates of visible vertices
C              which overwrite the input coordinates
C        IVIS(0:NVSVRT) - contains information about the output
C              vertices of XC, YC arrays as described above for input
C
C     Output parameters:
C        NVSVRT - (number of visible vertices) - 1
C        THETA(0:NVSVRT) - polar angles of visible vertices wrt (XEYE,
C              YEYE) at origin and (XC(0),YC(0)) on positive x-axis
C
C     Routines called:
C        ANGLE, LRLINE
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER CUR,I,IND,K,LR,LRLINE,N,TOP
      DOUBLE PRECISION ALPHA,ANG,ANG1,ANG2,ANGDIF,ANGLE,ANGSP2
      DOUBLE PRECISION COSANG,DIFF,DX,DY,NUMER,R,SINANG
C
C     Shift input vertices right, and possibly remove first and last
C     vertices due to collinearity with eyepoint.
C
      ANGSP2 = 2.0D0*ANGSPC
      CUR = MAXN + 1
      N = MAXN
      DO 10 I = NVIS,0,-1
	 CUR = CUR - 1
	 XC(CUR) = XC(I)
	 YC(CUR) = YC(I)
	 IVIS(CUR) = IVIS(I)
   10 CONTINUE
      LR = LRLINE(XC(CUR+1),YC(CUR+1),XEYE,YEYE,XC(CUR),YC(CUR),0.0D0)
      IF (LR .GE. 0) THEN
	 CUR = CUR + 1
	 XC(0) = XC(CUR)
	 YC(0) = YC(CUR)
	 IVIS(0) = IVIS(CUR)
      ENDIF
      LR = LRLINE(XC(N-1),YC(N-1),XEYE,YEYE,XC(N),YC(N),0.0D0)
      IF (LR .LE. 0) N = N - 1
      ALPHA = ATAN2(YC(0)-YEYE,XC(0)-XEYE)
      ANG2 = 0.0D0
      THETA(0) = 0.0D0
      TOP = 0
      CUR = CUR + 1
C
C     Process edge from vertices of indices CUR-1, CUR.
C
   20 CONTINUE
	 ANG1 = ANG2
	 ANG2 = ANGLE(XC(CUR),YC(CUR),XEYE,YEYE,XC(0),YC(0))
         ANGDIF = ANG2 - ANG1
         IF (ANGDIF .LE. TOL) THEN
   	    DIFF = ((XC(CUR) - XEYE)**2 + (YC(CUR) - YEYE)**2) -
     $         ((XC(CUR-1) - XEYE)**2 + (YC(CUR-1) - YEYE)**2)
   	    IF (DIFF .LT. 0.0D0) THEN
   	       XC(TOP) = XC(CUR)
   	       YC(TOP) = YC(CUR)
   	       IVIS(TOP) = IVIS(CUR)
	       THETA(TOP) = ANG2
   	    ENDIF
         ELSE
	    IF (ANGDIF .GE. ANGSP2) THEN
   	       K = INT(ANGDIF/ANGSPC)
   	       IND = -ABS(IVIS(CUR))
   	       ANGDIF = ANGDIF/DBLE(K)
   	       DX = XC(CUR) - XC(CUR-1)
   	       DY = YC(CUR) - YC(CUR-1)
   	       NUMER = (XC(CUR) - XEYE)*DY - (YC(CUR) - YEYE)*DX
   	       DO 30 I = 1,K-1
   	          TOP = TOP + 1
		  THETA(TOP) = ANG1 + DBLE(I)*ANGDIF
		  ANG = THETA(TOP) + ALPHA
   	          COSANG = COS(ANG)
   	          SINANG = SIN(ANG)
   	          R = NUMER/(DY*COSANG - DX*SINANG)
   	          XC(TOP) = R*COSANG + XEYE
   	          YC(TOP) = R*SINANG + YEYE
   	          IVIS(TOP) = IND
   30          CONTINUE
	    ENDIF
   	    TOP = TOP + 1
   	    XC(TOP) = XC(CUR)
   	    YC(TOP) = YC(CUR)
   	    IVIS(TOP) = IVIS(CUR)
	    THETA(TOP) = ANG2
         ENDIF
         CUR = CUR + 1
      IF (CUR .LE. N) GO TO 20
      NVSVRT = TOP
      END
C
C     The following code was excerpted from: vornbr.f
C
      SUBROUTINE VORNBR(XEYE,YEYE,NVRT,XC,YC,NVOR,IVOR,XVOR,YVOR)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NVOR,NVRT
      INTEGER IVOR(0:NVRT)
      DOUBLE PRECISION XEYE,YEYE
      DOUBLE PRECISION XC(0:NVRT),XVOR(0:NVRT),YC(0:NVRT),YVOR(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine the Voronoi neighbours of (XEYE,YEYE) from a
C        list of vertices which are in increasing "polar angle" order.
C        The Voronoi neighbours are a sublist of this list. The
C        Voronoi polygon is restricted to the sector formed from the
C        the edges joining (XEYE,YEYE) to the first and last vertices
C        of this list. Each Voronoi neighbour corresponds to an edge
C        of the Voronoi polygon.
C
C     Input parameters:
C        XEYE,YEYE - coordinates of eyepoint
C        NVRT - (number of vertices in list) - 1
C        XC(0:NVRT),YC(0:NVRT) - vertex coordinates from which
C              Voronoi neighbours are determined; (XC(0),YC(0)),...,
C              (XC(NVRT),YC(NVRT)) are in increasing angular
C              displacement order w.r.t. (XEYE,YEYE)
C
C     Output parameters:
C        NVOR - (number of Voronoi neighbours) - 1 [<= NVRT]
C        IVOR(0:NVOR) - indices of Voronoi neighbours in XC, YC
C               arrays; 0 <= IVOR(0) < ... < IVOR(NVOR) <= NVRT
C
C     Working parameters:
C        XVOR(0:NVRT),YVOR(0:NVRT) - arrays for storing the vertex
C              coordinates of the Voronoi polygon
C
C     Abnormal return:
C        IERR is set to 212
C
C     Routines called:
C        LRLINE
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER IM,K,LR,LRLINE,M
      DOUBLE PRECISION A11,A12,A21,A22,B1,B2,DET,TOLABS,XI,YI
C
      K = 1
      M = 0
      IVOR(0) = 0
      XVOR(0) = (XEYE + XC(0))*0.5D0
      YVOR(0) = (YEYE + YC(0))*0.5D0
C
C     Beginning of main loop
C
   10 CONTINUE
      IF (K .GT. NVRT) GO TO 20
C
C        Determine the intersection of the perpendicular bisectors
C        of edges from (XEYE,YEYE) to (XC(K),YC(K)) and from
C        (XEYE,YEYE) to (XC(IM),YC(IM)).
C
	 IM = IVOR(M)
	 A11 = XC(K) - XEYE
	 A12 = YC(K) - YEYE
	 A21 = XC(IM) - XEYE
	 A22 = YC(IM) - YEYE
	 TOLABS = TOL*MAX(ABS(A11),ABS(A12),ABS(A21),ABS(A22))
	 DET = A11*A22 - A21*A12
	 IF (ABS(DET) .LE. TOLABS) THEN
	    IERR = 212
	    RETURN
	 ENDIF
	 B1 = (A11**2 + A12**2)*0.5D0
	 B2 = (A21**2 + A22**2)*0.5D0
	 XI = (B1*A22 - B2*A12)/DET
	 YI = (B2*A11 - B1*A21)/DET
C
C        Determine whether (XVOR(M+1),YVOR(M+1)) is to the left of or
C        on the directed line from (XEYE,YEYE) to (XVOR(M),YVOR(M)).
C
	 XVOR(M+1) = XI + XEYE
	 YVOR(M+1) = YI + YEYE
	 LR = LRLINE(XVOR(M+1),YVOR(M+1),XEYE,YEYE,XVOR(M),YVOR(M),
     1      0.0D0)
	 IF (LR .LE. 0) THEN
	    M = M + 1
	    IVOR(M) = K
	    K = K + 1
	 ELSE IF (M .GT. 0) THEN
	    M = M - 1
	 ELSE
C
C           Determine the intersection of edge from (XEYE,YEYE) to
C           (XC(0),YC(0)) and the perpendicular bisector of the edge
C           from (XEYE,YEYE) to (XC(K),YC(K)).
C
	    A11 = XC(K) - XEYE
	    A12 = YC(K) - YEYE
	    A21 = YC(0) - YEYE
	    A22 = XEYE - XC(0)
	    TOLABS = TOL*MAX(ABS(A11),ABS(A12),ABS(A21),ABS(A22))
	    DET = A11*A22 - A21*A12
	    IF (ABS(DET) .LE. TOLABS) THEN
	       IERR = 212
	       RETURN
	    ENDIF
	    B1 = (A11**2 + A12**2)*0.5D0
	    B2 = 0.0D0
	    XI = (B1*A22 - B2*A12)/DET
	    YI = (B2*A11 - B1*A21)/DET
	    XVOR(M) = XI + XEYE
	    YVOR(M) = YI + YEYE
	    IVOR(M) = K
	    K = K + 1
	 ENDIF
      GO TO 10
C
C     The following short loop determines which vertices at the end
C     of list are not Voronoi neighbours.
C
   20 CONTINUE
	 LR = LRLINE(XVOR(M),YVOR(M),XEYE,YEYE,XC(NVRT),YC(NVRT),0.0D0)
	 IF (LR .GE. 0) GO TO 30
	 M = M - 1
      IF (M .GE. 0) GO TO 20
   30 CONTINUE
      NVOR = M
      END
C
C     The following code was excerpted from: vpleft.f
C
      SUBROUTINE VPLEFT(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the LEFT
C        operation (OPER = 1).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GVPVAR/
C
      INTEGER J,LR,LR1,LR2,LRLINE
      DOUBLE PRECISION XU,YU
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a left turn, S(TOP) = V(CUR), TOP <= CUR,
C     S(TOP-1) = V(CUR-1) or on interior of edge V(CUR-1)-V(CUR).
C
   10 CONTINUE
      IF (CUR .EQ. NV) THEN
	 OPER = 7
	 RETURN
      ENDIF
      IF (.NOT. BEYE .AND. TOP .LE. 2) GO TO 20
C
C     Check if angular displacement of stack chain >= 2*PI or
C     interior angle at boundary viewpoint.
C
      CALL XEDGE(1,XE,YE,XC(NV),YC(NV),XC(TOP-1),YC(TOP-1),XC(TOP),
     $   YC(TOP),XU,YU,INTSCT)
      IF (INTSCT) THEN
	 OPER = 4
	 XW = XC(CUR)
	 YW = YC(CUR)
         LR = LRLINE(XC(TOP),YC(TOP),XE,YE,XC(NV),YC(NV),0.0D0)
	 IF (LR .EQ. -1) THEN
	    XC(TOP) = XU
	    YC(TOP) = YU
	    IVIS(TOP) = -CUR
	 ENDIF
	 RETURN
      ENDIF
C
C     Process next edge.
C
   20 CONTINUE
      LR = LRLINE(XC(CUR+1),YC(CUR+1),XE,YE,XC(CUR),YC(CUR),0.0D0)
      IF (LR .EQ. -1) THEN
	 CUR = CUR + 1
	 TOP = TOP + 1
	 XC(TOP) = XC(CUR)
	 YC(TOP) = YC(CUR)
	 IVIS(TOP) = CUR
      ELSE
	 J = CUR + 1
         LR1 = LRLINE(XC(J),YC(J),XC(TOP-1),YC(TOP-1),XC(CUR),YC(CUR),
     $      0.0D0)
	 IF (LR1 .EQ. 1) THEN
	    OPER = 3
	    CUR = J
	 ELSE
	    IF (LR .EQ. 1) THEN
	       LR2 = 1
	       GO TO 40
	    ENDIF
   30       CONTINUE
	       J = J + 1
	       LR2 = LRLINE(XC(J),YC(J),XE,YE,XC(CUR),YC(CUR),0.0D0)
	    IF (LR2 .EQ. 0) GO TO 30
   40       CONTINUE
	    IF (LR2 .EQ. -1) THEN
	       TOP = TOP + 1
	       XC(TOP) = XC(J-1)
	       YC(TOP) = YC(J-1)
	       IVIS(TOP) = J - 1 + NV
	       TOP = TOP + 1
	       XC(TOP) = XC(J)
	       YC(TOP) = YC(J)
	       IVIS(TOP) = J
	    ELSE
	       OPER = 2
	    ENDIF
	    CUR = J
	 ENDIF
      ENDIF
C
C     This test avoids extra subroutine calls.
C
      IF (OPER .EQ. 1) GO TO 10
      END
C
C     The following code was excerpted from: vprght.f
C
      SUBROUTINE VPRGHT(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the RIGHT
C        operation (OPER = 2).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Abnormal return:
C        IERR is set to 206
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GVPVAR/
C
      INTEGER CASE,J,LR,LR1,LR2,LRLINE
      DOUBLE PRECISION XU,YU
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a right turn, EYE-S(TOP)-V(CUR) is a right
C     turn, EYE-S(TOP-1)-S(TOP) is a left turn, TOP < CUR, S(TOP) =
C     V(CUR-1) and S(TOP-1)-S(TOP)-V(CUR) is a left turn or S(TOP) is
C     not on edge V(CUR-1)-V(CUR) and V(CUR-1)-V(CUR) intersects
C     EYE-S(TOP).
C     Pop points from stack. If BEYE, it is not possible for
C     (XC(CUR),YC(CUR)) to be identical to any stack points.
C
   10 CONTINUE
      CASE = 0
      J = TOP
   20 CONTINUE
	 IF (ABS(IVIS(J)) .LE. NV) THEN
	    LR = LRLINE(XC(CUR),YC(CUR),XE,YE,XC(J-1),YC(J-1),0.0D0)
	    IF (LR .EQ. -1) THEN
	       CASE = 1
	    ELSE IF (LR .EQ. 0) THEN
	       IF (ABS(IVIS(J-1)) .LE. NV) THEN
		  J = J - 1
		  CASE = 2
	       ELSE IF ((XC(J-2) - XE)**2 + (YC(J-2) - YE)**2 .GE.
     $            (XC(J-1) - XE)**2 + (YC(J-1) - YE)**2) THEN
		  J = J - 2
		  CASE = 2
	       ELSE
		  CASE = -1
	       ENDIF
	    ENDIF
	 ELSE IF (CASE .EQ. -1) THEN
	    IF ((XC(J-1) - XE)**2 + (YC(J-1) - YE)**2 .GE.
     $         (XC(CUR) - XE)**2 + (YC(CUR) - YE)**2) THEN
	       J = J - 1
	       CASE = 2
	    ELSE
	       XW = XC(CUR)
	       YW = YC(CUR)
	       CASE = 3
	    ENDIF
	 ELSE
	    CALL XEDGE(0,XC(CUR-1),YC(CUR-1),XC(CUR),YC(CUR),
     $         XC(J-1),YC(J-1),XC(J),YC(J),XW,YW,INTSCT)
	    IF (INTSCT) CASE = 3
	 ENDIF
	 IF (CASE .GT. 0) GO TO 30
	 J = J - 1
      IF (J .GE. 1) GO TO 20
C
C     Error from no more edges in stack.
C
      IERR = 206
      RETURN
C
C     Process next edge.
C
   30 CONTINUE
      IF (CASE .EQ. 3) THEN
	 OPER = 6
	 TOP = J - 1
      ELSE
	 TOP = J
	 XW = XC(CUR-1)
	 YW = YC(CUR-1)
	 IF (CASE .EQ. 1) THEN
	    CALL XEDGE(1,XE,YE,XC(CUR),YC(CUR),XC(TOP-1),YC(TOP-1),
     $         XC(TOP),YC(TOP),XU,YU,INTSCT)
	    XC(TOP) = XU
	    YC(TOP) = YU
	    IVIS(TOP) = -ABS(IVIS(TOP))
	 ENDIF
	 LR = LRLINE(XC(CUR+1),YC(CUR+1),XE,YE,XC(CUR),YC(CUR),0.0D0)
	 IF (LR .EQ. 1) THEN
	    CUR = CUR + 1
	 ELSE
	    J = CUR + 1
	    LR1 = LRLINE(XC(J),YC(J),XW,YW,XC(CUR),YC(CUR),0.0D0)
	    IF (LR1 .EQ. -1) THEN
	       OPER = 5
	       CUR = J
	    ELSE
	       IF (LR .EQ. -1) THEN
		  LR2 = -1
		  GO TO 50
	       ENDIF
   40          CONTINUE
		  J = J + 1
		  LR2 = LRLINE(XC(J),YC(J),XE,YE,XC(CUR),YC(CUR),0.0D0)
	       IF (LR2 .EQ. 0) GO TO 40
   50          CONTINUE
	       IF (LR2 .EQ. -1) THEN
	          OPER = 1
	          TOP = TOP + 1
	          XC(TOP) = XC(J-1)
	          YC(TOP) = YC(J-1)
	          IVIS(TOP) = J - 1 + NV
	          TOP = TOP + 1
	          XC(TOP) = XC(J)
	          YC(TOP) = YC(J)
	          IVIS(TOP) = J
	       ENDIF
	       CUR = J
	    ENDIF
	 ENDIF
      ENDIF
C
C     This test avoids extra subroutine calls.
C
      IF (OPER .EQ. 2) GO TO 10
      END
C
C     The following code was excerpted from: vpscna.f
C
      SUBROUTINE VPSCNA(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the SCANA
C        operation (OPER = 3).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Abnormal return:
C        IERR is set to 207
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GVPVAR/
C
      INTEGER CASE,J,K,LR,LR1,LR2,LR3,LRLINE
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a right turn or forward move, S(TOP) =
C     V(CUR-1) or EYE-S(TOP)-V(CUR-1) is a forward move and TOP = 0,
C     TOP < CUR; S(TOP-1)-S(TOP)-V(CUR) is a right turn if TOP >= 1
C     or EYE-S(TOP)-V(CUR) is a right turn if TOP = 0.
C     If BEYE, it is possible that (XC(TOP),YC(TOP)) is a non-simple
C     vertex but any edge incident on this vertex encountered during
C     scan must be invisible from (XE,YE).
C
      K = CUR
   10 CONTINUE
	 IF (XC(K+1) .EQ. XC(TOP) .AND. YC(K+1) .EQ. YC(TOP)) THEN
	    K = K + 2
	 ELSE
	    CALL XEDGE(1,XE,YE,XC(TOP),YC(TOP),XC(K),YC(K),XC(K+1),
     $         YC(K+1),XW,YW,INTSCT)
	    IF (INTSCT) THEN
	       LR = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(K),YC(K),0.0D0)
	       IF (LR .EQ. 1) THEN
		  IF ((XC(TOP) - XE)**2 + (YC(TOP) - YE)**2 .GE.
     $               (XW - XE)**2 + (YW - YE)**2) THEN
		     IF (TOP .GT. 0) THEN
		        CASE = 1
			GO TO 20
		     ENDIF
		  ELSE
	             LR1 = LRLINE(XC(K),YC(K),XE,YE,XC(TOP),YC(TOP),
     $                  0.0D0)
		     IF (LR1 .EQ. -1) THEN
		        CASE = 2
		        GO TO 20
		     ENDIF
		  ENDIF
	       ELSE
	          LR1 = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(TOP),YC(TOP),
     $               0.0D0)
		  IF (LR1 .EQ. -1) THEN
		     CASE = 3
		     GO TO 20
		  ENDIF
	       ENDIF
	    ENDIF
	    K = K + 1
	 ENDIF
      IF (K .LT. NV) GO TO 10
C
C     Error from unsuccessful scan.
C
      IERR = 207
      RETURN
C
C     Process current edge.
C
   20 CONTINUE
      IF (CASE .EQ. 3) THEN
	 OPER = 1
	 CUR = K + 1
	 LR = LRLINE(XC(K),YC(K),XE,YE,XC(TOP),YC(TOP),0.0D0)
	 TOP = TOP + 1
	 IF (LR .EQ. 0) THEN
	    XC(TOP) = XC(K)
	    YC(TOP) = YC(K)
	    IVIS(TOP) = K + NV
	 ELSE
	    XC(TOP) = XW
	    YC(TOP) = YW
	    IVIS(TOP) = -(K + 1 + NV)
	 ENDIF
	 TOP = TOP + 1
	 XC(TOP) = XC(CUR)
	 YC(TOP) = YC(CUR)
	 IVIS(TOP) = CUR
      ELSE IF (CASE .EQ. 1) THEN
	 CUR = K + 1
	 LR = LRLINE(XC(CUR),YC(CUR),XE,YE,XC(TOP),YC(TOP),0.0D0)
	 IF (LR .EQ. 1) THEN
	    OPER = 2
	 ELSE
	    J = CUR + 1
	    LR1 = LRLINE(XC(J),YC(J),XE,YE,XC(CUR),YC(CUR),0.0D0)
	    LR2 = LRLINE(XC(J),YC(J),XC(K),YC(K),XC(CUR),YC(CUR),0.0D0)
	    IF (LR1 .LE. 0 .AND. LR2 .EQ. -1) THEN
	       OPER = 5
	       XW = XC(K)
	       YW = YC(K)
	       CUR = J
	    ELSE
	       IF (LR1 .NE. 0) THEN
		  LR3 = LR1
		  GO TO 40
	       ENDIF
   30          CONTINUE
		  J = J + 1
		  LR3 = LRLINE(XC(J),YC(J),XE,YE,XC(CUR),YC(CUR),0.0D0)
	       IF (LR3 .EQ. 0) GO TO 30
   40          CONTINUE
	       IF (LR3 .EQ. 1) THEN
		  OPER = 2
	       ELSE
		  OPER = 1
		  TOP = TOP + 1
		  XC(TOP) = XC(J-1)
		  YC(TOP) = YC(J-1)
		  IVIS(TOP) = J - 1 + NV
		  TOP = TOP + 1
		  XC(TOP) = XC(J)
		  YC(TOP) = YC(J)
		  IVIS(TOP) = J
	       ENDIF
	       CUR = J
	    ENDIF
	 ENDIF
      ELSE
	 OPER = 6
	 CUR = K + 1
	 LR = LRLINE(XC(CUR),YC(CUR),XE,YE,XC(TOP),YC(TOP),0.0D0)
	 IF (LR .EQ. 0) THEN
	    XW = XC(CUR)
	    YW = YC(CUR)
	 ENDIF
      ENDIF
      END
C
C     The following code was excerpted from: vpscnb.f
C
      SUBROUTINE VPSCNB(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the SCANB
C        operation (OPER = 4).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Abnormal return:
C        IERR is set to 208
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION PI,TOL,XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GCONST/,/GVPVAR/
C
      INTEGER K,LR,LR1,LRLINE
      DOUBLE PRECISION TOLABS,XU,YU
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a left turn, S(TOP) = V(CUR) or S(TOP) is
C     on interior of edge V(CUR-1)-V(CUR), TOP <= CUR, S(TOP) has
C     angular displacement of 2*PI or interior angle at boundary eye.
C     (XW,YW) is the input version of (XC(CUR),YC(CUR)).
C     If BEYE, it is possible that (XC(TOP),YC(TOP)) is a non-simple
C     point but any edge containing this point encountered during scan
C     must be invisible from (XE,YE), except for 1 case where K = CUR.
C
      TOLABS = TOL*((XC(NV) - XC(TOP))**2 + (YC(NV) - YC(TOP))**2)
      K = CUR
      IF (IVIS(TOP) .LT. 0 .OR. K + 1 .EQ. NV) GO TO 10
      LR = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(TOP),YC(TOP),0.0D0)
      LR1 = LRLINE(XC(K+1),YC(K+1),XC(TOP-1),YC(TOP-1),XC(TOP),YC(TOP),
     $   0.0D0)
      IF (LR .EQ. 1 .AND. LR1 .EQ. -1) THEN
	 OPER = 2
	 CUR = K + 1
	 RETURN
      ELSE
	 K = K + 1
      ENDIF
C
   10 CONTINUE
	 IF (K + 1 .EQ. NV) THEN
	    OPER = 7
	    CUR = NV
	    TOP = TOP + 1
	    XC(TOP) = XC(NV)
	    YC(TOP) = YC(NV)
	    IVIS(TOP) = NV
	    RETURN
	 ELSE
	    IF (K .EQ. CUR) THEN
	       CALL XEDGE(0,XC(NV),YC(NV),XC(TOP),YC(TOP),XW,YW,
     $            XC(K+1),YC(K+1),XU,YU,INTSCT)
	    ELSE
	       CALL XEDGE(0,XC(NV),YC(NV),XC(TOP),YC(TOP),XC(K),YC(K),
     $            XC(K+1),YC(K+1),XU,YU,INTSCT)
	    ENDIF
	    IF (INTSCT) THEN
	       IF ((XC(TOP) - XU)**2 + (YC(TOP) - YU)**2 .LE. TOLABS)
     $            GO TO 20
	       LR = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(NV),YC(NV),0.0D0)
	       IF (LR .EQ. 1) THEN
		  OPER = 2
		  CUR = K + 1
		  RETURN
	       ENDIF
	    ENDIF
   20       CONTINUE
	    K = K + 1
	 ENDIF
      IF (K .LT. NV) GO TO 10
C
C     Error from unsuccessful scan.
C
      IERR = 208
      END
C
C     The following code was excerpted from: vpscnc.f
C
      SUBROUTINE VPSCNC(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the SCANC
C        operation (OPER = 5).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Abnormal return:
C        IERR is set to 209
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GVPVAR/
C
      INTEGER J,K,LR,LR1,LR2,LRLINE
      DOUBLE PRECISION XP,XU,YP,YU
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a left turn or forward move, EYE-V(CUR-2)-
C     V(CUR-1) is a right turn, V(CUR-2)-V(CUR-1)-V(CUR) is a left turn,
C     TOP < CUR-1, W = V(CUR-2), S(TOP) is not on V(CUR-1)-V(CUR), EYE-
C     S(TOP)-V(CUR-1) is a backward move, EYE-S(TOP-1)-S(TOP) is a left
C     turn. If BEYE, it is possible that V(CUR-1) is a non-simple point,
C     but intersection at (XC(TOP),YC(TOP)) cannot occur.
C
      XP = XC(CUR-1)
      YP = YC(CUR-1)
      K = CUR
   10 CONTINUE
	 IF (XC(K+1) .EQ. XP .AND. YC(K+1) .EQ. YP) THEN
	    GO TO 40
	 ELSE IF (XC(K) .EQ. XP .AND. YC(K) .EQ. YP) THEN
	    J = K + 1
	    LR = LRLINE(XC(J),YC(J),XE,YE,XP,YP,0.0D0)
	    LR1 = LRLINE(XC(J),YC(J),XW,YW,XP,YP,0.0D0)
	    IF (LR .LE. 0 .AND. LR1 .EQ. -1) GO TO 40
	    IF (LR .NE. 0) THEN
	       LR2 = LR
	       GO TO 30
	    ENDIF
   20       CONTINUE
	       J = J + 1
	       LR2 = LRLINE(XC(J),YC(J),XE,YE,XP,YP,0.0D0)
	    IF (LR2 .EQ. 0) GO TO 20
   30       CONTINUE
	    IF (LR2 .EQ. 1) THEN
	       OPER = 2
	    ELSE
	       OPER = 1
	       TOP = TOP + 1
	       XC(TOP) = XC(J-1)
	       YC(TOP) = YC(J-1)
	       IVIS(TOP) = J - 1 + NV
	       TOP = TOP + 1
	       XC(TOP) = XC(J)
	       YC(TOP) = YC(J)
	       IVIS(TOP) = J
	    ENDIF
	    CUR = J
	    RETURN
	 ELSE
	    CALL XEDGE(0,XP,YP,XC(TOP),YC(TOP),XC(K),YC(K),XC(K+1),
     $         YC(K+1),XU,YU,INTSCT)
	    IF (INTSCT) THEN
	       LR = LRLINE(XC(K+1),YC(K+1),XE,YE,XP,YP,0.0D0)
	       IF (LR .EQ. 1) THEN
		  OPER = 2
		  CUR = K + 1
		  RETURN
	       ENDIF
	    ENDIF
	 ENDIF
   40    CONTINUE
	 K = K + 1
      IF (K .LT. NV) GO TO 10
C
C     Error from unsuccessful scan.
C
      IERR = 209
      END
C
C     The following code was excerpted from: vpscnd.f
C
      SUBROUTINE VPSCND(XC,YC,IVIS)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IVIS(0:*)
      DOUBLE PRECISION XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: This routine is called by routine VISPOL for the SCAND
C        operation (OPER = 6).
C
C     Input and updated parameters:
C        XC,YC,IVIS - see comments in routine VISPOL
C
C     Abnormal return:
C        IERR is set to 210
C
C     Routines called:
C        LRLINE, XEDGE
C
      INTEGER CUR,IERR,NV,OPER,TOP
      DOUBLE PRECISION XE,XW,YE,YW
      LOGICAL BEYE
      COMMON /GERROR/ IERR
      COMMON /GVPVAR/ NV,OPER,CUR,TOP,XE,YE,XW,YW,BEYE
      SAVE /GERROR/,/GVPVAR/
C
      INTEGER K,LR,LR1,LR2,LRLINE
      DOUBLE PRECISION XP,XU,YP,YU
      LOGICAL INTSCT
C
C     EYE-V(CUR-1)-V(CUR) is a right turn, S(TOP) is a V vertex not on
C     V(CUR-1)-V(CUR), TOP < CUR, W is intersection of V(CUR-1)-V(CUR)
C     and ray EYE-S(TOP), EYE-S(TOP)-W is a forward move, and
C     EYE-S(TOP-1)-S(TOP) is a left turn if TOP >= 1.
C     If BEYE, it is possible that (XW,YW) is a non-simple point,
C     but intersection at (XC(TOP),YC(TOP)) cannot occur.
C
      XP = XC(CUR-1)
      YP = YC(CUR-1)
      K = CUR
   10 CONTINUE
	 CALL XEDGE(0,XW,YW,XC(TOP),YC(TOP),XC(K),YC(K),XC(K+1),
     $      YC(K+1),XU,YU,INTSCT)
	 IF (INTSCT) THEN
	    LR = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(K),YC(K),0.0D0)
	    LR1 = LRLINE(XC(K+1),YC(K+1),XE,YE,XC(TOP),YC(TOP),0.0D0)
	    IF (LR .EQ. -1 .AND. LR1 .EQ. -1) THEN
	       IF (XC(K) .NE. XW .OR. YC(K) .NE. YW) GO TO 20
	       LR2 = LRLINE(XC(K+1),YC(K+1),XP,YP,XW,YW,0.0D0)
	       IF (LR2 .EQ. -1) GO TO 30
   20          CONTINUE
	       OPER = 1
	       CUR = K + 1
	       LR2 = LRLINE(XC(K),YC(K),XE,YE,XC(TOP),YC(TOP),0.0D0)
	       TOP = TOP + 1
	       IF (LR2 .EQ. 0) THEN
		  XC(TOP) = XC(K)
		  YC(TOP) = YC(K)
		  IVIS(TOP) = K + NV
	       ELSE
		  XC(TOP) = XU
		  YC(TOP) = YU
		  IVIS(TOP) = -(K + 1 + NV)
	       ENDIF
	       TOP = TOP + 1
	       XC(TOP) = XC(CUR)
	       YC(TOP) = YC(CUR)
	       IVIS(TOP) = CUR
	       RETURN
	    ENDIF
	 ENDIF
   30    CONTINUE
	 K = K + 1
      IF (K .LT. NV) GO TO 10
C
C     Error from unsuccessful scan.
C
      IERR = 210
      END
