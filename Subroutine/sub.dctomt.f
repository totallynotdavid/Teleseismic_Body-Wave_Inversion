      Subroutine dctomt(Str,Dip,Slp,Smo,Mxy)
      REAL mxy(3,3)
      PARAMETER(RAD=.01745329)
*  < Moment-Tensor for a Double-Couple >
       SD=SIN(Dip*RAD)
       CD=COS(Dip*RAD)
       S2D=SIN(Dip*2*RAD)
       C2D=COS(Dip*2*RAD)
       SSL=SIN(Slp*RAD)
       CSL=COS(Slp*RAD)
       SSK=SIN(Str*RAD)
       CSK=COS(Str*RAD)
       S2SK=SIN(Str*2*RAD)
       C2SK=COS(Str*2*RAD)
       Mxy(1,1) =-(SD*CSL*S2SK+S2D*SSL*SSK**2) *Smo
       Mxy(1,2) = (SD*CSL*C2SK+.5*S2D*SSL*S2SK)*Smo
        Mxy(2,1) = Mxy(1,2)
       Mxy(1,3) =-(CD*CSL*CSK+C2D*SSL*SSK)     *Smo
        Mxy(3,1) = Mxy(1,3)
       Mxy(2,2) = (SD*CSL*S2SK-S2D*SSL*CSK**2) *Smo
       Mxy(2,3) =-(CD*CSL*SSK-C2D*SSL*CSK)     *Smo
        Mxy(3,2) = Mxy(2,3)
       Mxy(3,3) =  S2D*SSL                     *Smo
      END
