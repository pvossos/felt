C
C     The following code was excerpted from: bedgmv.f
C
      SUBROUTINE BEDGMV(NVC,NPOLG,NVERT,MAXVC,H,VCL,HVL,PVL,VSTART,VNUM)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXVC,NPOLG,NVC,NVERT
      INTEGER HVL(NPOLG),PVL(4,NVERT),VSTART(NVERT),VNUM(NVERT)
      DOUBLE PRECISION H(NPOLG),VCL(2,MAXVC)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Generate mesh vertices on boundary of convex polygons
C        of decomposition with spacing determined by H array.
C
C     Input parameters:
C        NVC - number of coordinates or positions used in VCL array
C        NPOLG - number of polygons or positions used in HVL array
C        NVERT - number of vertices or positions used in PVL array
C        MAXVC - maximum size available for VCL array
C        H(1:NPOLG) - spacing of mesh vertices for convex polygons
C        VCL(1:2,1:NVC) - vertex coordinate list
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT) - polygon vertex list
C
C     Updated parameters:
C        NVC,VCL
C
C     Output parameters:
C        VSTART(1:NVERT) - start location in VCL for mesh vertices on
C              each edge in PVL if there are any, else 0
C        VNUM(1:NVERT) - number of mesh vertices on interior of each
C              edge in PVL; entry is negated if mesh vertices are
C              listed in backward order in VCL
C
C     Abnormal return:
C        IERR is set to 3
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,IA,J,K,L,M,U,V
      DOUBLE PRECISION DX,DY,HH,LENG,X,Y
C
      DO 10 I = 1,NVERT
	 VSTART(I) = -1
   10 CONTINUE
      DO 40 K = 1,NPOLG
	 I = HVL(K)
   20    CONTINUE
	    J = PVL(SUCC,I)
	    IF (VSTART(I) .EQ. -1) THEN
	       U = PVL(LOC,I)
	       V = PVL(LOC,J)
	       X = VCL(1,U)
	       Y = VCL(2,U)
	       LENG = SQRT((VCL(1,V) - X)**2 + (VCL(2,V) - Y)**2) 
	       IA = PVL(EDGV,I)
	       IF (IA .LE. 0) THEN
	          HH = H(K)
	       ELSE
		  HH = SQRT(H(K)*H(PVL(POLG,IA)))
	       ENDIF
	       L = INT(LENG/HH)
	       IF (LENG/HH - L .GT. DBLE(L)/DBLE(2*L+1)) L = L + 1
	       IF (L .LE. 1) THEN
		  VSTART(I) = 0
		  VNUM(I) = 0
	       ELSE
		  DX = (VCL(1,V) - X)/DBLE(L)
		  DY = (VCL(2,V) - Y)/DBLE(L)
		  L = L - 1
		  IF (NVC + L .GT. MAXVC) THEN
		     IERR = 3
		     RETURN
		  ENDIF
		  VSTART(I) = NVC + 1
		  VNUM(I) = L
		  DO 30 M = 1,L
		     X = X + DX
		     Y = Y + DY
		     NVC = NVC + 1
		     VCL(1,NVC) = X
		     VCL(2,NVC) = Y
   30             CONTINUE
	       ENDIF
	       IF (IA .GT. 0) THEN
		  VSTART(IA) = VSTART(I)
	          VNUM(IA) = -VNUM(I)
	       ENDIF
	    ENDIF
	    I = J
	 IF (I .NE. HVL(K)) GO TO 20
   40 CONTINUE
      END
C
C     The following code was excerpted from: cvdtri.f
C
      SUBROUTINE CVDTRI(INTER,LDV,NT,VCL,TIL,TEDG,SPTR)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL INTER
      INTEGER LDV,NT
      INTEGER SPTR(NT),TEDG(3,NT),TIL(3,NT)
      DOUBLE PRECISION VCL(LDV,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Convert triangles in strip near boundary of polygon
C        or inside polygon to Delaunay triangles.
C
C     Input parameters:
C	 INTER - .TRUE. iff at least one interior mesh vertex
C        LDV - leading dimension of VCL in calling routine
C	 NT - number of triangles in strip or polygon
C        VCL(1:2,1:*) - vertex coordinate list
C        TIL(1:3,1:NT) - triangle incidence list
C        TEDG(1:3,1:NT) - TEDG(J,I) refers to edge with vertices
C              TIL(J:J+1,I) and contains index of merge edge or
C              > NT for edge of chains
C
C     Updated parameters:
C        TIL,TEDG - updated due to diagonal edge swaps
C
C     Working parameter:
C        SPTR(1:NT) - SPTR(I) = -1 if merge edge I is not in LOP stack,
C              else >= 0 and pointer (index of SPTR) to next edge in
C              stack (0 indicates bottom of stack)
C
C     Abnormal return:
C        IERR is set to 231
C
C     Routines called:
C        FNDTRI, LOP
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER E,IND(2),ITR(2),K,MXTR,TOP
      LOGICAL SFLAG
C
      SFLAG = .TRUE.
      DO 10 K = 1,NT
	 SPTR(K) = -1
   10 CONTINUE
      DO 30 K = 1,NT
	 MXTR = K + 1
	 IF (K .EQ. NT) THEN
	    IF (.NOT. INTER) RETURN
	    MXTR = NT
	    SFLAG = .FALSE.
	 ENDIF
	 TOP = K
	 SPTR(K) = 0
   20    CONTINUE
	    E = TOP
	    TOP = SPTR(E)
	    CALL FNDTRI(E,MXTR,SFLAG,TEDG,ITR,IND)
	    IF (IERR .NE. 0) RETURN
	    CALL LOP(ITR,IND,K,TOP,LDV,VCL,TIL,TEDG,SPTR)
	 IF (TOP .GT. 0) GO TO 20
   30 CONTINUE
      END
C
C     The following code was excerpted from: fndtri.f
C
      SUBROUTINE FNDTRI(IEDG,MXTR,SFLAG,TEDG,ITR,IND)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL SFLAG
      INTEGER IEDG,IND(2),ITR(2),MXTR,TEDG(3,MXTR)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Find two triangles containing edge with index IEDG
C        in array TEDG.
C
C     Input parameters:
C        IEDG - index of edge to be searched in TEDG
C        MXTR - maximum index of triangle to be searched in TEDG
C	 SFLAG - .TRUE. iff second triangle is to be searched from
C              end of array
C        TEDG(1:3,1:MXTR) - triangle edge indices; see routine CVDTRI
C
C     Output parameters:
C        ITR(1:2),IND(1:2) - indices such that IEDG =
C              TEDG(IND(1),ITR(1)) = TEDG(IND(2),ITR(2))
C
C     Abnormal return:
C        IERR is set to 231
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER I,J,K
C
C     Search from end of array TEDG.
C
      K = 1
      J = 1
      I = MXTR
   10 CONTINUE
	 IF (TEDG(J,I) .NE. IEDG) THEN
	    J = J + 1
	    IF (J .GT. 3) THEN
	       J = 1
	       I = I - 1
	       IF (I .LE. 0) THEN
		  IERR = 231
		  RETURN
	       ENDIF
	    ENDIF
	    GO TO 10
	 ENDIF
      ITR(K) = I
      IND(K) = J
      IF (K .EQ. 2) RETURN
      K = 2
      IF (SFLAG) THEN
	 J = 1
	 I = I - 1
	 IF (I .LE. 0) THEN
    	    IERR = 231
	    RETURN
	 ENDIF
	 GO TO 10
      ENDIF
C
C     Search from beginning of array TEDG for second triangle.
C
      J = 1
      I = 1
   20 CONTINUE
      IF (I .GE. ITR(1)) THEN
	 IERR = 231
	 RETURN
      ENDIF
   30 CONTINUE
	 IF (TEDG(J,I) .NE. IEDG) THEN
	    J = J + 1
	    IF (J .GT. 3) THEN
	       J = 1
	       I = I + 1
	       GO TO 20
	    ELSE
	       GO TO 30
	    ENDIF
	 ENDIF
      ITR(2) = I
      IND(2) = J
      END
C
C     The following code was excerpted from: inttri.f
C
      SUBROUTINE INTTRI(NVRT,XC,YC,H,IBOT,COSTH,SINTH,LDV,NVC,NTRI,
     $   MAXVC,MAXTI,MAXCW,VCL,TIL,NCW,CWALK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IBOT,LDV,MAXCW,MAXTI,MAXVC,NCW,NTRI,NVC,NVRT
      INTEGER CWALK(0:MAXCW),TIL(3,MAXTI)
      DOUBLE PRECISION COSTH,H,SINTH
      DOUBLE PRECISION VCL(LDV,MAXVC),XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Generate triangles inside convex polygon using quasi-
C        uniform grid of spacing H. It is assumed that diameter of
C        polygon is parallel to y-axis.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(0:NVRT),YC(0:NVRT) - vertex coordinates in CCW order;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        H - spacing of mesh vertices in polygon
C        IBOT - index of bottom vertex; diameter contains vertices
C              (XC(0),YC(0)) and (XC(IBOT),YC(IBOT))
C        COSTH,SINTH - COS(THETA), SIN(THETA) where THETA in [-PI,PI]
C              is rotation angle to get diameter parallel to y-axis
C        LDV - leading dimension of VCL in calling routine
C        NVC - number of coordinates or positions used in VCL array
C        NTRI - number of triangles or positions used in TIL
C        MAXVC - maximum size available for VCL array
C        MAXTI - maximum size available for TIL array
C        MAXCW - maximum size available for CWALK array; assumed to be
C              >= 6*(1 + INT((YC(0) - YC(IBOT))/H))
C        VCL(1:2,1:NVC) - vertex coordinate list
C        TIL(1:3,1:NTRI) - triangle incidence list
C
C     Updated parameters:
C        NVC,NTRI,VCL,TIL
C
C     Output parameters:
C        NCW - number of mesh vertices in closed walk, except NCW = 0
C              for 1 vertex
C        CWALK(0:NCW) - indices in VCL of mesh vertices of closed
C              walk; CWALK(0) = CWALK(NCW)
C
C     Abnormal return:
C        IERR is set to 3, 9, or 10
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER I,IL,IM1L,IM1R,IR,J,K,L,L0,L1,LW,M,N,P,R,R0,R1,RW
      DOUBLE PRECISION A,B,CY,SY,X,XJ,XK,XL,XM1L,XM1R,XR,Y
C

      IL = 0
      IM1L = 0
      IM1R = 0
      IR = 0
      LW = 0
      M = 0
      RW = 0
      XM1L = 0.0
      XM1R = 0.0
 
      N = INT((YC(0) - YC(IBOT))/H)
      Y = YC(0) - 0.5D0*(YC(0) - YC(IBOT) - DBLE(N)*H)
      L = 0
      R = NVRT
      DO 110 I = 0,N
C
C        Determine left and right x-coordinates of polygon for
C        scan line with y-coordinate Y, and generate mesh vertices.
C
   10    CONTINUE
	 IF (YC(L+1) .GT. Y) THEN
	    L = L + 1
	    GO TO 10
	 ENDIF
   20    CONTINUE
	 IF (YC(R-1) .GT. Y) THEN
	    R = R - 1
	    GO TO 20
	 ENDIF
	 XL = XC(L) + (XC(L+1) - XC(L))*(Y - YC(L))/(YC(L+1) - YC(L))
	 XR = XC(R) + (XC(R-1) - XC(R))*(Y - YC(R))/(YC(R-1) - YC(R))
	 M = INT((XR - XL)/H)
	 X = XL + 0.5D0*(XR - XL - DBLE(M)*H)
	 IF (NVC + M + 1 .GT. MAXVC) THEN
	    IERR = 3
	    RETURN
	 ENDIF
	 CY = COSTH*Y
	 SY = SINTH*Y
	 IL = NVC + 1
	 XL = X
	 DO 30 J = 0,M
	    NVC = NVC + 1
	    VCL(1,NVC) = COSTH*X + SY
	    VCL(2,NVC) = CY - SINTH*X
	    X = X + H
   30    CONTINUE
	 IR = NVC
	 XR = X - H
         IF (N .EQ. 0) THEN
	    NCW = 0
	    CWALK(0) = NVC
	    RETURN
	 ELSE IF (I .EQ. 0) THEN
	    LW = 0
	    CWALK(LW) = IL
	    RW = MAXCW + 1
	    DO 40 J = IL,IR
	       RW = RW - 1
	       CWALK(RW) = J
   40       CONTINUE
	    GO TO 100
         ENDIF
C
C        Generate triangles between scan lines Y+H and Y.
C
	 A = MAX(XL,XM1L)
	 B = MIN(XR,XM1R)
	 IF (XM1L .EQ. A) THEN
	    L0 = IM1L
	    X = (XM1L - XL)/H
	    J = INT(X + TOL)
	    IF (ABS(X - DBLE(J)) .LE. TOL) J = J - 1
	    IF (J .LT. 0) J = 0
	    L1 = IL + J
	 ELSE
	    L1 = IL
	    X = (XL - XM1L)/H
	    J = INT(X + TOL)
	    IF (ABS(X - DBLE(J)) .LE. TOL) J = J - 1
	    IF (J .LT. 0) J = 0
	    L0 = IM1L + J
	 ENDIF
	 IF (XM1R .EQ. B) THEN
	    R0 = IM1R
	    X = (XR - XM1R)/H
	    J = INT(X + TOL)
	    IF (ABS(X - DBLE(J)) .LE. TOL) J = J - 1
	    IF (J .LT. 0) J = 0
	    R1 = IR - J
	 ELSE
	    R1 = IR
	    X = (XM1R - XR)/H
	    J = INT(X + TOL)
	    IF (ABS(X - DBLE(J)) .LE. TOL) J = J - 1
	    IF (J .LT. 0) J = 0
	    R0 = IM1R - J
	 ENDIF
	 IF (L0 .LT. R0 .OR. L1 .LT. R1) THEN
	    J = L0
	    K = L1
	    XJ = XM1L + DBLE(J-IM1L)*H
	    XK = XL + DBLE(K-IL)*H
   50       CONTINUE
	       IF (K .LT. R1 .AND. (XK .LE. XJ .OR. J .EQ. R0)) THEN
		  P = K
		  K = K + 1
		  XK = XK + H
	       ELSE
		  P = J
		  J = J + 1
		  XJ = XJ + H
	       ENDIF
	       NTRI = NTRI + 1
	       IF (NTRI .GT. MAXTI) THEN
		  IERR = 9
		  RETURN
	       ENDIF
	       TIL(1,NTRI) = J
	       TIL(2,NTRI) = P
	       TIL(3,NTRI) = K
	    IF (J .LT. R0 .OR. K .LT. R1) GO TO 50
	 ENDIF
C
C        Generate paths of closed walk between scan lines Y+H and Y.
C
	 IF (XM1L .LT. XL) THEN
	    DO 60 J = IM1L+1,L0
	       LW = LW + 1
	       CWALK(LW) = J
   60       CONTINUE
            LW = LW + 1
	    CWALK(LW) = IL
	 ELSE
	    DO 70 J = L1,IL,-1
	       LW = LW + 1
	       CWALK(LW) = J
   70       CONTINUE
	 ENDIF
	 IF (XM1R .GT. XR) THEN
	    DO 80 J = IM1R-1,R0,-1
	       RW = RW - 1
	       CWALK(RW) = J
   80       CONTINUE
            RW = RW - 1
	    CWALK(RW) = IR
	 ELSE
	    DO 90 J = R1,IR
	       RW = RW - 1
	       CWALK(RW) = J
   90       CONTINUE
	 ENDIF
  100    CONTINUE
	 Y = Y - H
	 IM1L = IL
	 IM1R = IR
	 XM1L = XL
	 XM1R = XR
  110 CONTINUE
C
C     Add last path of left walk and shift indices of right walk.
C
      IF (M .EQ. 0) THEN
	 RW = RW + 1
      ELSE
	 DO 120 J = IL+1,IR-1
	    LW = LW + 1
	    CWALK(LW) = J
  120    CONTINUE
      ENDIF
      IF (RW .LE. LW) THEN
	 IERR = 10
	 RETURN
      ENDIF
      DO 130 J = RW,MAXCW
	 LW = LW + 1
	 CWALK(LW) = CWALK(J)
  130 CONTINUE
      NCW = LW
      END
C
C     The following code was excerpted from: lop.f
C
      SUBROUTINE LOP(ITR,IND,MXEDG,TOP,LDV,VCL,TIL,TEDG,SPTR)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IND(2),ITR(2),LDV,MXEDG,TOP
      INTEGER SPTR(*),TEDG(3,*),TIL(3,*)
      DOUBLE PRECISION VCL(LDV,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Apply local optimization procedure to two triangles
C        indicated by ITR(1) and ITR(2). This may result in swapping
C        diagonal edge of quadrilateral.
C
C     Input parameters:
C        ITR(1),ITR(2) - indices of triangles for LOP
C        IND(1),IND(2) - indices indicating common edge of triangles
C        MXEDG - maximum index of edge to be considered for LOP
C        TOP - index of SPTR indicating top of stack
C        LDV - leading dimension of VCL in calling routine
C        VCL(1:2,1:*) - vertex coordinate list
C        TIL(1:3,1:*) - triangle incidence list
C        TEDG(1:3,1:*) - triangle edge indices; see routine CVDTRI
C        SPTR(1:*) - stack pointers; see routine CVDTRI
C
C     Updated parameters:
C        TOP,TIL,TEDG,SPTR - updated due diagonal edge swaps
C
C     Routines called:
C        DIAEDG
C
      INTEGER A,B,C,D,DIAEDG,I,IEDG,IN,IND1M1,IND1P1,IND2M1,IND2P1,J
C
C     Common edge is BC, other two vertices are A and D.
C
      IEDG = TEDG(IND(1),ITR(1))
      SPTR(IEDG) = -1
      IND1M1 = IND(1) - 1
      IF (IND1M1 .LE. 0) IND1M1 = 3
      IND1P1 = IND(1) + 1
      IF (IND1P1 .GE. 4) IND1P1 = 1
      IND2M1 = IND(2) - 1
      IF (IND2M1 .LE. 0) IND2M1 = 3
      IND2P1 = IND(2) + 1
      IF (IND2P1 .GE. 4) IND2P1 = 1
      B = TIL(IND(1),ITR(1))
      C = TIL(IND1P1,ITR(1))
      A = TIL(IND1M1,ITR(1))
      D = TIL(IND2M1,ITR(2))
      IN = DIAEDG(VCL(1,D),VCL(2,D),VCL(1,C),VCL(2,C),VCL(1,A),VCL(2,A),
     $   VCL(1,B),VCL(2,B))
      IF (IN .EQ. 1) THEN
C
C        Check if four edges of quadrilateral should be put on LOP
C        stack, and swap edge BC for AD.
C
	 I = TEDG(IND1M1,ITR(1))
	 DO 10 J = 1,4
	    IF (J .EQ. 2) THEN
	       I = TEDG(IND1P1,ITR(1))
	    ELSE IF (J .EQ. 3) THEN
	       I = TEDG(IND2M1,ITR(2))
	    ELSE IF (J .EQ. 4) THEN
	       I = TEDG(IND2P1,ITR(2))
	    ENDIF
	    IF (I .LE. MXEDG) THEN
	       IF (SPTR(I) .EQ. -1) THEN
	          SPTR(I) = TOP
	          TOP = I
	       ENDIF
	    ENDIF
   10    CONTINUE
	 TIL(IND1P1,ITR(1)) = D
	 TIL(IND2P1,ITR(2)) = A
	 TEDG(IND(1),ITR(1)) = TEDG(IND2P1,ITR(2))
	 TEDG(IND(2),ITR(2)) = TEDG(IND1P1,ITR(1))
	 TEDG(IND1P1,ITR(1)) = IEDG
	 TEDG(IND2P1,ITR(2)) = IEDG
      ENDIF
      END
C
C     The following code was excerpted from: mtredg.f
C
      SUBROUTINE MTREDG(UTYPE,I1,I2,I3,IBNDRY,NT,TIL,TEDG)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL UTYPE
      INTEGER I1,I2,I3,IBNDRY,NT
      INTEGER TEDG(3,*),TIL(3,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Set fields for triangle as needed by routine TMERGE.
C
C     Input parameters:
C	 UTYPE - .TRUE. iff triangle contains two 'U' vertices
C	 I1, I2, I3 - indices of 3 triangle vertices in VCL; the first
C              2 indices also belong to the next merge edge
C        IBNDRY - index of boundary edge for TEDG
C        NT - number of entries in TIL, TEDG so far
C        TIL(1:NT) - triangle incidence list
C        TEDG(1:NT) - triangle edge indices; see routine TMERGE
C
C     Updated parameters:
C        NT,TIL,TEDG - one more triangle is added at end of arrays
C
      NT = NT + 1
      TIL(1,NT) = I1
      TIL(2,NT) = I2
      TIL(3,NT) = I3
      TEDG(1,NT) = NT
      IF (UTYPE) THEN
         TEDG(2,NT) = NT - 1
         TEDG(3,NT) = IBNDRY
      ELSE
         TEDG(2,NT) = IBNDRY
         TEDG(3,NT) = NT - 1
      ENDIF
      END
C
C     The following code was excerpted from: rotpg.f
C
      SUBROUTINE ROTPG(NVRT,XC,YC,I1,I2,IBOT,COSTH,SINTH)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,IBOT,NVRT
      DOUBLE PRECISION COSTH,SINTH,XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Rotate convex polygon so that a line segment joining two
C        of its vertices is parallel to y-axis.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(0:NVRT),YC(0:NVRT) - vertex coordinates in CCW order;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        I1,I2 - index of vertices of line segment; I1, I2 > 0
C
C     Output parameters:
C        XC(0:NVRT),YC(0:NVRT) - rotated vertex coordinates; indices are
C              also rotated so that (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C              is top vertex and (XC(IBOT),YC(IBOT)) is bottom vertex
C        IBOT - index of bottom vertex
C        COSTH,SINTH - COS(THETA) and SIN(THETA) where THETA in
C              [-PI,PI] is rotation angle
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER A,B,I,ITOP,J,K,L,M,R
      DOUBLE PRECISION THETA,X0,Y0
C
      ITOP = I1
      IBOT = I2
      IF (YC(I1) .EQ. YC(I2)) THEN
	 IF (XC(I1) .LT. XC(I2)) THEN
	    THETA = -PI/2.0D0
	 ELSE
	    THETA = PI/2.0D0
	 ENDIF
      ELSE
	 IF (YC(I1) .LT. YC(I2)) THEN
	    ITOP = I2
	    IBOT = I1
	 ENDIF
	 THETA = PI/2.0D0 - ATAN2(YC(ITOP)-YC(IBOT), XC(ITOP)-XC(IBOT))
      ENDIF
      COSTH = COS(THETA)
      SINTH = SIN(THETA)
      DO 10 I = 1,NVRT
	 X0 = XC(I)
	 XC(I) = COSTH*X0 - SINTH*YC(I)
	 YC(I) = SINTH*X0 + COSTH*YC(I)
   10 CONTINUE
C
C     Rotate indices.
C
      IF (ITOP .EQ. NVRT) GO TO 50
      A = NVRT
      B = ITOP
   20 CONTINUE
	 R = MOD(A,B)
	 A = B
	 B = R
      IF (R .GT. 0) GO TO 20
      M = NVRT/A - 1
      DO 40 I = 1,A
	 X0 = XC(I)
	 Y0 = YC(I)
	 K = I
	 DO 30 J = 1,M
	    L = K + ITOP
	    IF (L .GT. NVRT) L = L - NVRT
	    XC(K) = XC(L)
	    YC(K) = YC(L)
	    K = L
   30    CONTINUE
	 XC(K) = X0
	 YC(K) = Y0
   40 CONTINUE
      IBOT = IBOT - ITOP
      IF (IBOT .LT. 0) IBOT = IBOT + NVRT
   50 CONTINUE
      XC(0) = XC(NVRT)
      YC(0) = YC(NVRT)
      END
C
C     The following code was excerpted from: tmerge.f
C
      SUBROUTINE TMERGE(INTER,NBL,NCR,CHBL,CHCR,LDV,VCL,TIL,TEDG)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL INTER
      INTEGER LDV,NBL,NCR
      INTEGER CHBL(0:NBL),CHCR(0:NCR),TEDG(3,NBL+NCR),TIL(3,NBL+NCR)
      DOUBLE PRECISION VCL(LDV,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Form triangles in strip near boundary of polygon or
C        inside polygon by merging two chains of vertices.
C
C     Input parameters:
C	 INTER - .TRUE. iff at least one interior mesh vertex
C	 NBL - number of vertices on boundary cycle if INTER,
C              otherwise on left boundary chain
C	 NCR - number of vertices on closed walk if INTER,
C              otherwise on right boundary chain
C	 CHBL(0:NBL) - indices in VCL of vertices on boundary cycle
C              or left boundary chain; if INTER, CHBL(NBL) = CHBL(0)
C	 CHCR(0:NCR) - indices in VCL of vertices on closed walk
C              or right boundary chain; if INTER, CHCR(NCR) = CHCR(0),
C              otherwise CHCR(0) is not referenced
C        LDV - leading dimension of VCL in calling routine
C        VCL(1:2,1:*) - vertex coordinate list
C
C     Output parameters:
C        TIL(1:3,1:NT) - triangle incidence list, where NT =
C              NBL + NCR - K where K = 0 if INTER, else K = 2
C        TEDG(1:3,1:NT) - TEDG(J,I) refers to edge with vertices
C              TIL(J:J+1,I) and contains index of merge edge or
C              NBL+NCR+1 for edge of chains
C        [Note: It is assumed there is enough space in 2 arrays.]
C
C     Abnormal return:
C        IERR is set to 230
C
C     Routines called:
C        DIAEDG, LRLINE, MTREDG
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER DIAEDG,I,IBNDRY,IN,J,LRI,LRIP1,LRLINE,NL,NR,NT
      DOUBLE PRECISION XI,XIP1,XJ,XJP1,YI,YIP1,YJ,YJP1
C
      IBNDRY = NBL + NCR + 1
      NT = 0
      LRI = 0
      LRIP1 = 0
      IF (INTER) THEN
	 NL = NBL
	 NR = NCR
         I = 0
         J = 0
      ELSE
	 CALL MTREDG(.TRUE.,CHBL(1),CHCR(1),CHBL(0),IBNDRY,NT,TIL,TEDG)
	 TEDG(2,1) = IBNDRY
	 IF (NBL + NCR .LE. 3) RETURN
	 NL = NBL - 1
	 NR = NCR - 1
         I = 1
         J = 1
	 LRI = 1
	 LRIP1 = 1
      ENDIF
C
C     Main while loop for determining next triangle and edge.
C
   10 CONTINUE
      IF (I .GE. NL .OR. J .GE. NR) GO TO 20
	 XI = VCL(1,CHBL(I))
	 YI = VCL(2,CHBL(I))
	 XIP1 = VCL(1,CHBL(I+1))
	 YIP1 = VCL(2,CHBL(I+1))
	 XJ = VCL(1,CHCR(J))
	 YJ = VCL(2,CHCR(J))
	 XJP1 = VCL(1,CHCR(J+1))
	 YJP1 = VCL(2,CHCR(J+1))
	 IN = DIAEDG(XJP1,YJP1,XJ,YJ,XI,YI,XIP1,YIP1)
	 IF (INTER) THEN
	    LRI = LRLINE(XI,YI,XJ,YJ,XJP1,YJP1,0.0D0)
	    LRIP1 = LRLINE(XIP1,YIP1,XJ,YJ,XJP1,YJP1,0.0D0)
	 ENDIF
	 IF (IN .LE. 0 .OR. LRI .LE. 0 .AND. LRIP1 .LE. 0) THEN
	    CALL MTREDG(.TRUE.,CHBL(I+1),CHCR(J),CHBL(I),IBNDRY,NT,TIL,
     $         TEDG)
	    I = I + 1
	 ELSE
	    CALL MTREDG(.FALSE.,CHBL(I),CHCR(J+1),CHCR(J),IBNDRY,NT,TIL,
     $         TEDG)
	    J = J + 1
	 ENDIF
      GO TO 10
C
C     Add remaining triangles at end of strip or bottom of polygon.
C
   20 CONTINUE
      IF (I .LT. NL) THEN
	 IF (.NOT. INTER .AND. J .EQ. NR) NL = NL + 1
   30    CONTINUE
	    CALL MTREDG(.TRUE.,CHBL(I+1),CHCR(J),CHBL(I),IBNDRY,NT,TIL,
     $         TEDG)
	    I = I + 1
	 IF (I .LT. NL) GO TO 30
      ELSE
C        J < NR .OR. I = NL = J = NR = 1
	 IF (.NOT. INTER .AND. I .EQ. NL) NR = NR + 1
   40    CONTINUE
	    CALL MTREDG(.FALSE.,CHBL(I),CHCR(J+1),CHCR(J),IBNDRY,NT,TIL,
     $         TEDG)
	    IF (INTER) THEN
	       LRI = LRLINE(VCL(1,CHBL(I)),VCL(2,CHBL(I)),
     $            VCL(1,CHCR(J+1)),VCL(2,CHCR(J+1)),VCL(1,CHCR(J)),
     $            VCL(2,CHCR(J)),0.0D0)
	       IF (LRI .GE. 0) THEN
		  IERR = 230
		  RETURN
	       ENDIF
	    ENDIF
	    J = J + 1
	 IF (J .LT. NR) GO TO 40
      ENDIF
C
      IF (INTER) THEN
	 IF (TEDG(2,1) .EQ. 0) THEN
	    TEDG(2,1) = NBL + NCR
	 ELSE
	    TEDG(3,1) = NBL + NCR
	 ENDIF
      ENDIF
      END
C
C     The following code was excerpted from: tripr2.f
C
      SUBROUTINE TRIPR2(NVC,NPOLG,NVERT,MAXVC,MAXTI,MAXIW,MAXWK,H,VCL,
     $   HVL,PVL,IANG,NTRI,TIL,VSTART,VNUM,TSTART,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXIW,MAXTI,MAXVC,MAXWK,NPOLG,NTRI,NVC,NVERT
      INTEGER HVL(NPOLG),IWK(MAXIW),PVL(4,NVERT),TIL(3,MAXTI)
      INTEGER TSTART(NPOLG),VNUM(NVERT),VSTART(NVERT)
      DOUBLE PRECISION H(NPOLG),IANG(NVERT),VCL(2,MAXVC),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Generate mesh vertices and triangles inside each convex
C        polygon of decomposition according to mesh spacings in H array
C        to get a triangulation of a polygonal region.
C
C     Input parameters:
C        NVC - number of vertex coordinates or positions used in VCL
C              array
C        NPOLG - number of polygonal subregions or positions used in
C              HVL array
C        NVERT - number of polygon vertices or positions used in PVL
C              array
C        MAXVC - maximum size available for VCL array, should be >=
C              number of mesh vertices in triangulation of region
C        MAXTI - maximum size available for TIL array, should be >= 
C              number of triangles in triangulation of region
C        MAXIW - maximum size available for IWK array, should be >=
C              5*(NBC+NCW)+2 where NBC is maximum number of mesh edges
C              on boundary of a polygon, NCW is maximum number of edges
C              on boundary of interior triangulation
C        MAXWK - maximum size available for WK array, should be >=
C              5*NVRT+4 where NVRT is max no. of vertices in a polygon
C	 H(1:NPOLG) - mesh spacings for polygons of decomposition
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles; see routine DSPGDC for more details
C
C     Updated parameters:
C        NVC,VCL - updated due to generation of mesh vertices
C
C     Output parameters:
C        NTRI - number of triangles in triangulation of region
C        TIL(1:3,1:NTRI) - triangle incidence list; TIL(1:3,I) contains
C              indices in VCL of 3 vertices of Ith triangle in CCW order
C        VSTART(1:NVERT) - start location in VCL for mesh vertices on
C              each edge in PVL if there are any, else 0
C        VNUM(1:NVERT) - number of mesh vertices on interior of each
C              edge in PVL; entry is negated if mesh vertices are
C              listed in backward order in VCL
C        TSTART(1:NPOLG) - start location in TIL of triangles in
C              each polygon; TIL(1:3,I) for I=TSTRT(K),...,TSTRT(K+1)-1
C              are the triangles in Kth polygon
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 6, 7, 9, 10, 200, 202, 230, or 231
C
C     Routines called:
C        BEDGMV, TRPOLG
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER BNDCYC,I,J,K,NBC,NVRT,XC,YC
      DOUBLE PRECISION PIMTOL
C
      NTRI = 0
      PIMTOL = PI - TOL
      CALL BEDGMV(NVC,NPOLG,NVERT,MAXVC,H,VCL,HVL,PVL,VSTART,VNUM)
      IF (IERR .NE. 0) RETURN
      DO 50 K = 1,NPOLG
	 NVRT = 0
	 NBC = 0
	 I = HVL(K)
   10    CONTINUE
	    IF (IANG(I) .LT. PIMTOL) NVRT = NVRT + 1
	    NBC = NBC + 1 + ABS(VNUM(I))
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(K)) GO TO 10
	 IF (NBC + 1 .GT. MAXIW) THEN
	    IERR = 6
	    RETURN
	 ELSE IF (2*NVRT + 2 .GT. MAXWK) THEN
	    IERR = 7
	    RETURN
	 ENDIF
	 XC = 1
	 YC = XC + NVRT + 1
	 BNDCYC = 1
   20    CONTINUE
	    J = PVL(LOC,I)
	    IF (IANG(I) .LT. PIMTOL) THEN
	       WK(XC) = VCL(1,J)
	       WK(YC) = VCL(2,J)
	       XC = XC + 1
	       YC = YC + 1
	    ENDIF
	    IWK(BNDCYC) = J
	    BNDCYC = BNDCYC + 1
	    IF (VNUM(I) .GE. 0) THEN
	       DO 30 J = VSTART(I),VSTART(I)+VNUM(I)-1
		  IWK(BNDCYC) = J
		  BNDCYC = BNDCYC + 1
   30          CONTINUE
	    ELSE
	       DO 40 J = VSTART(I)-VNUM(I)-1,VSTART(I),-1
		  IWK(BNDCYC) = J
		  BNDCYC = BNDCYC + 1
   40          CONTINUE
	    ENDIF
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(K)) GO TO 20
	 WK(XC) = WK(1)
	 WK(YC) = WK(NVRT+2)
	 IWK(BNDCYC) = IWK(1)
	 XC = 1
	 YC = XC + NVRT + 1
	 BNDCYC = 1
         TSTART(K) = NTRI + 1
	 CALL TRPOLG(NVRT,WK(XC),WK(YC),H(K),NBC,IWK(BNDCYC),2,NVC,NTRI,
     $      MAXVC,MAXTI,MAXIW-NBC-1,MAXWK-2*NVRT-2,VCL,TIL,IWK(NBC+2),
     $      WK(2*NVRT+3))
         IF (IERR .NE. 0) RETURN
   50 CONTINUE
      END
C
C     The following code was excerpted from: trpolg.f
C
      SUBROUTINE TRPOLG(NVRT,XC,YC,H,NBC,BNDCYC,LDV,NVC,NTRI,MAXVC,
     $   MAXTI,MAXIW,MAXWK,VCL,TIL,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER LDV,MAXIW,MAXTI,MAXVC,MAXWK,NBC,NTRI,NVC,NVRT
      INTEGER BNDCYC(0:NBC),TIL(3,MAXTI),IWK(MAXIW)
      DOUBLE PRECISION H,VCL(LDV,MAXVC),WK(MAXWK),XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Generate Delaunay triangular mesh inside convex polygon
C        using quasi-uniform grid of spacing H.
C
C     Input parameters:
C	 NVRT - number of vertices on the boundary of convex polygon
C	 XC(0:NVRT),YC(0:NVRT) - vertex coordinates in CCW order;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT)); it is assumed
C              that all interior angles are < PI
C        H - spacing of mesh vertices in polygon
C        NBC - size of BNDCYC
C        BNDCYC(0:NBC) - indices in VCL of mesh vertices of boundary
C              cycle; BNDCYC(0) = BNDCYC(NBC); contains (XC(I),YC(I))
C        LDV - leading dimension of VCL in calling routine
C        NVC - number of coordinates or positions used in VCL array
C        NTRI - number of triangles or positions used in TIL
C        MAXVC - maximum size available for VCL array
C        MAXTI - maximum size available for TIL array
C        MAXIW - maximum size available for IWK array, should be >=
C              6*(1 + INT(DIAM/H)) + 4*(NBC + NCW) where DIAM is
C              diameter of polygon, NCW is number of edges on boundary
C              of interior triangulation
C        MAXWK - maximum size available for WK array, should be >=
C              3*NVRT+2
C        VCL(1:2,1:NVC) - vertex coordinate list
C        TIL(1:3,1:NTRI) - triangle incidence list
C
C     Updated parameters:
C        BNDCYC(0:NBC) - elements of array may be rotated
C        NVC,NTRI,VCL,TIL
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 6, 7, 9, 10, 200, 202, 230, or 231
C
C     Routines called:
C        CVDTRI, DIAM2, INTTRI, ROTIAR, ROTPG, SHRNK2, TMERGE
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER CWALK,I,I1,I2,IBOT,IEDGE,IND,MAXCW,MBC,NCW,NSHR,NT
      INTEGER SDIST,SPTR,TEDG,XS,YS
      DOUBLE PRECISION COSTH,DIST,HS,SINTH,SMDIST,X0,XI,Y0,YI,YR
      LOGICAL INTER

      CWALK = 0.0
      YR = 0.0
C
      IF (NVRT + 1 .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ELSE IF (3*NVRT + 2 .GT. MAXWK) THEN
	 IERR = 7
	 RETURN
      ENDIF
      XS = 1
      YS = XS + NVRT + 1
      SDIST = YS + NVRT + 1
      IEDGE = 1
      HS = H/SQRT(2.0D0)
      DO 10 I = 0,NVRT-1
	 WK(SDIST+I) = HS
   10 CONTINUE
      CALL SHRNK2(NVRT,XC,YC,WK(SDIST),NSHR,WK(XS),WK(YS),IWK(IEDGE))
      IF (IERR .NE. 0) RETURN
      INTER = (NSHR .GT. 0)
C
      IF (INTER) THEN
	 CALL DIAM2(NSHR,WK(XS+1),WK(YS+1),I1,I2,DIST)
	 IF (IERR .NE. 0) RETURN
	 CALL ROTPG(NSHR,WK(XS),WK(YS),I1,I2,IBOT,COSTH,SINTH)
	 MAXCW = 6*(1 + INT((WK(YS) - WK(YS+IBOT))/H))
         IF (MAXCW + 1 .GT. MAXIW) THEN
	    IERR = 6
	    RETURN
         ENDIF
	 CWALK = 1
         CALL INTTRI(NSHR,WK(XS),WK(YS),H,IBOT,COSTH,SINTH,LDV,NVC,NTRI,
     $      MAXVC,MAXTI,MAXCW,VCL,TIL,NCW,IWK(CWALK))
         IF (IERR .NE. 0) RETURN
C
C        Determine the mesh vertex which should be moved to front of
C        BNDCYC - closest to CWALK(0) and also with y-coordinate >
C        that of CWALK(0) when rotated if NCW > 0.
C
	 X0 = VCL(1,IWK(CWALK))
	 Y0 = VCL(2,IWK(CWALK))
	 IF (NCW .GT. 0) YR = SINTH*X0 + COSTH*Y0
	 SMDIST = 100000.0D0*H**2
	 DO 20 I = 0,NBC-1
	    XI = VCL(1,BNDCYC(I))
	    YI = VCL(2,BNDCYC(I))
	    IF (NCW .GT. 0) THEN
	       IF (SINTH*XI + COSTH*YI .LE. YR) GO TO 20
	    ENDIF
	    DIST = (XI - X0)**2 + (YI - Y0)**2
	    IF (DIST .LT. SMDIST) THEN
	       SMDIST = DIST
	       IND = I
	    ENDIF
   20    CONTINUE
	 CALL ROTIAR(NBC,BNDCYC,IND)
	 BNDCYC(NBC) = BNDCYC(0)
	 NT = NBC + NCW
	 TEDG = CWALK + NCW + 1
      ELSE
	 CALL DIAM2(NVRT,XC(1),YC(1),I1,I2,DIST)
	 IF (IERR .NE. 0) RETURN
	 IND = 0
   30    CONTINUE
	 IF (IND .GE. NBC) GO TO 40
	    IF (XC(I1) .EQ. VCL(1,BNDCYC(IND)) .AND. YC(I1) .EQ.
     $         VCL(2,BNDCYC(IND))) GO TO 40
	    IND = IND + 1
	    GO TO 30
   40    CONTINUE
	 CALL ROTIAR(NBC,BNDCYC,IND)
	 BNDCYC(NBC) = BNDCYC(0)
	 MBC = 1
   50    CONTINUE
	 IF (MBC .GE. NBC) GO TO 60
	    IF (XC(I2) .EQ. VCL(1,BNDCYC(MBC)) .AND. YC(I2) .EQ.
     $         VCL(2,BNDCYC(MBC))) GO TO 60
	    MBC = MBC + 1
	    GO TO 50
   60    CONTINUE
	 IND = NBC
	 DO 70 I = MBC+1,MBC+(NBC-MBC-1)/2
	    IND = IND - 1
	    I1 = BNDCYC(I)
	    BNDCYC(I) = BNDCYC(IND)
	    BNDCYC(IND) = I1
   70    CONTINUE
	 BNDCYC(NBC) = BNDCYC(MBC)
	 NT = NBC - 2
	 TEDG = 1
C        Left boundary chain contains mesh vertices BNDCYC(0:MBC)
C        and right chain contains BNDCYC(0,MBC+1:NBC); MBC < NBC.
      ENDIF
C
      IF (NTRI + NT .GT. MAXTI) THEN
	 IERR = 9
	 RETURN
      ELSE IF (TEDG + 4*NT - 1 .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ENDIF
      IF (INTER) THEN
 	 CALL TMERGE(INTER,NBC,NCW,BNDCYC,IWK(CWALK),LDV,VCL,
     $      TIL(1,NTRI+1),IWK(TEDG))
      ELSE
 	 CALL TMERGE(INTER,MBC,NBC-MBC,BNDCYC,BNDCYC(MBC),LDV,VCL,
     $      TIL(1,NTRI+1),IWK(TEDG))
      ENDIF
      IF (IERR .NE. 0) RETURN
      SPTR = TEDG + 3*NT
      CALL CVDTRI(INTER,LDV,NT,VCL,TIL(1,NTRI+1),IWK(TEDG),IWK(SPTR))
      NTRI = NTRI + NT
      END
