      SUBROUTINE DBAS(SK,DP,SL,AMO,V)
      REAL V(6)
      PARAMETER(RAD=.01745329)
*  < Mij >
       SD=SIN(DP*RAD)
       CD=COS(DP*RAD)
       S2D=SIN(DP*2*RAD)
       C2D=COS(DP*2*RAD)
       SSL=SIN(SL*RAD)
       CSL=COS(SL*RAD)
       SSK=SIN(SK*RAD)
       CSK=COS(SK*RAD)
       S2SK=SIN(SK*2*RAD)
       C2SK=COS(SK*2*RAD)
       A11=-(SD*CSL*S2SK+S2D*SSL*SSK**2) *AMO
       A12= (SD*CSL*C2SK+.5*S2D*SSL*S2SK)*AMO
       A13=-(CD*CSL*CSK+C2D*SSL*SSK)     *AMO
       A22= (SD*CSL*S2SK-S2D*SSL*CSK**2) *AMO
       A23=-(CD*CSL*SSK-C2D*SSL*CSK)     *AMO
       A33=  S2D*SSL                     *AMO
*  < Basis >
      V(1)=A12
      V(2)=-A22
      V(3)=A23
      V(4)=A13
      V(5)=A33
      V(6)=0
      END
