	SUBROUTINE EQAREA

	COMMON AZ,TOA,RC,X,Y,RAD,A,D,PI,P2

	IF (TOA .GT. P2) THEN
		AZ = AZ+PI
		TOA = PI-TOA
	ENDIF
	S2 = 2**.5
	R = RC* S2 * SIN (TOA/2.)
	TH = (P2-AZ)
	X = R* COS(TH)
	Y = R* SIN(TH)
	END
