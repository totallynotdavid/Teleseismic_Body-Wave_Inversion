KIK=Subroutine/mylib.a
REC=Recipes/recipes.a

lists: Main/lists.f
	f77 -O Main/lists.f -o lists 
listg: Main/listg.f
	f77 -O Main/listg.f -o listg 
rotSH: Main/rotSH.f
	f77 -O Main/rotSH.f $(KIK) -o rotSH
plotw: Main/plotw.f
	f77 -O Main/plotw.f $(KIK) $(REC) -o plotw
gregra: Main/gregra.f
	f77 -O Main/gregra.f $(KIK) -o gregra
gr3: Main/gr3.f
	f77 -O Main/gr3.f $(KIK) $(REC) -o gr3
mom3: Main/mom3.f
	f77 -O Main/mom3.f $(KIK) $(REC) -o mom3
plotcm: Main/plotcm.f
	f77 -O Main/plotcm.f $(KIK) $(REC) -o plotcm
graphics3: Main/graphics3.f
	f77 -O Main/graphics3.f $(KIK) -o graphics3
inversion3: Main/inversion3.f
	f77 -O Main/inversion3.f $(KIK) $(REC) -o inversion3
green3: Main/green3.f
	f77 -O Main/green3.f $(KIK) -o green3
graphics2: Main/graphics2.f
	f77 -O Main/graphics2.f $(KIK) -o graphics2
inversion2: Main/inversion2.f
	f77 -O Main/inversion2.f $(KIK) $(REC) -o inversion2
green: Main/green.f
	f77 -O Main/green.f $(KIK) -o green
inversion: Main/inversion.f
	f77 -O Main/inversion.f $(KIK) $(REC) -o inversion
graphics: Main/graphics.f
	f77 -O Main/graphics.f $(KIK) $(REC) -o graphics
conv.sac.farm: Main/conv.sac.farm.f
	f77 -O Main/conv.sac.farm.f $(KIK) -o conv.sac.farm
conv.sac.iris: Main/conv.sac.iris.f
	f77 -O Main/conv.sac.iris.f $(KIK) -o conv.sac.iris
