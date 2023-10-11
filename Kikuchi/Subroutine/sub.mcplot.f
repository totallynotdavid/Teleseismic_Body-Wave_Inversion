        subroutine mcplot (STRK1,DIP1,RAKE,R,nfill)
*          copied from SUBROUTINE GT and modified slightly
*  if nfill >= 2, fill quadrants.
	CHARACTER*1 F
	COMMON AZ,TOA,RC,X,Y,RAD,A,D,PI,P2

	PI = 3.14159265
	RAD = PI/180.
	P2 = PI/2.

	RC =R
	NPC = 100
	NPP = 100
	NPCQ = 30
	ncq=50*r
	sgn=1.

	IF (RAKE .GT. 0. .AND. RAKE .LT. 180.) THEN
	F = 'T'
	ELSEIF (RAKE .LT. 0. .OR. RAKE .GT. 180.) THEN
	F = 'N'
	ELSEIF (RAKE .EQ. 0.) THEN
	F = 'N'
	IF (STRK1 .LT. 90.0) 	F = 'T'
	IF (STRK1 .LT. 270.0 .AND. STRK1 .GE. 180.) F = 'T'
	ELSEIF (RAKE .EQ. 180.) THEN
	F = 'T'
	IF (STRK1 .LT.  90.0) 	F = 'N'
	IF (STRK1 .LT. 270.0 .AND. STRK1 .GE. 180.) F = 'N'
	ENDIF

	A1 = STRK1*RAD
	D1 = DIP1 *RAD

      SD1=SIN(D1)
      D2=ACOS(SIN(rake*rad)*SD1)
      IF(rake.EQ.0..OR.rake.EQ.180..OR.Dip1.EQ.90.) GOTO 11
      IF(strk1.EQ.90.) THEN
        a2=a1+PI
        GOTO 12
      ENDIF
      AT=1/TAN(rake*rad)/COS(D1)
      a2=a1+ATAN(AT)+PI
      GOTO 12
11    a2=a1-SIGN(P2,SIN(rake*2*rad))
12    IF(D2.LE.P2) GOTO 13
         D2=PI-D2
13     strk2=a2/rad
       dip2=d2/rad
C......FUDGE FACTORS

	CHECK = ABS(STRK1-STRK2)
	IF (CHECK .GE. 179.9 .AND. CHECK .LT. 180.1) A1 = A1+.01

	IF (DIP1 .GE. 89.9 .AND. DIP1 .LT. 90.1) D1 = D1-.01
	IF (DIP2 .GE. 89.9 .AND. DIP2 .LT. 90.1) D2 = D2-.01
	IF (F .EQ. 'S') F = 'N'
	T = A2 - A1
	IF (ABS(T) .GE. PI) SGN = 1.
	IF (ABS(T) .LT. PI) SGN = -1.
	IF (T .LT. 0.) SGN = -1.*SGN
	IF (F .EQ. 'N') SGN = -1.*SGN

C......DRAW CIRCLES

	call circ1(0.,0.,r)
	call plot(0.,r,3)
	call plot(0.,r-.1,2)

C......DRAW PLANES

	call nplane(a1,d1,r)
	call nplane(a2,d2,r)

        if(nfill.lt.2) return

C........FIND POLE

	CALL POLE (A1,D1,A2,D2)
	AZP = AZ
	BP = P2-TOA

C........FIND SLIP PLANE

	AN = AZP + P2
	DN = P2 - BP
	AZS1 = A2 - P2
	BS1 = P2 - D2
	AZS2 = A1 - P2
	BS2 = P2 - D1

C...FILL IN QUADRANTS

	AS1 = ASIN (SIN(AZP-AZS1)*COS(BS1))

	DO 22 I=1,NCQ
	PHI = PI/NCQ*I
	M = NPCQ+1
	DO 2 J=1,M
	G = P2/NPCQ * (J-1)*SGN
	BL = B
	bb=  SIN(BP) * COS(PHI) + COS(BP) * SIN(PHI) * COS(AS1+G)
	if(abs(bb).le.1.) b=asin(bb)
	if(abs(bb).gt.1.) b=sign(p2,bb)
5	aars= SIN(PHI) * SIN(AS1+G)/COS(B)
 	if(abs(aars).le.1.) ars=asin(aars)
	if(abs(aars).gt.1.) ars=sign(p2,aars)
	PHI0 = ACOS (SIN(BP) * SIN(B))
	AZ = AZP - ARS
	IF (PHI .GT. PHI0) AZ = AZP+ARS-PI
	TOA = P2-B
	CALL EQAREA
	IF (J .EQ. 1) THEN
		CALL PLOT (X,Y,3)
		GOTO 2
	ENDIF
	IF (B*BL .GT. 0.) THEN
		CALL PLOT (X,Y,2)
		GOTO 2
	ENDIF
	IF (B .EQ. 0.) GOTO 6

	X0 = X
	Y0 = Y
	B0 = B
	B=0
	G = SIGN(ACOS(-TAN(BP)/TAN(PHI)),(AS1+G))-AS1
	GOTO 5

6	IF (BL .LT. 0.) THEN
		X = -X
		Y = -Y
	ENDIF
	CALL PLOT (X,Y,2)

	X = -X
	Y = -Y
	CALL PLOT (X,Y,3)
	X = X0
	Y = Y0
	CALL PLOT(X,Y,2)
	B = B0
2	CONTINUE
22	CONTINUE
	END
