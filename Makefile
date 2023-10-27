KIK=Subroutine/mylib.a
REC=Recipes/recipes.a
VER=ifort

lists: Main/lists.f
	$(VER) -O Main/lists.f -o lists 
listg: Main/listg.f
	$(VER) -O Main/listg.f -o listg 
rotSH: Main/rotSH.f
	$(VER) -O Main/rotSH.f $(KIK) -o rotSH
plotw: Main/plotw.f
	$(VER) -O Main/plotw.f $(KIK) $(REC) -o plotw
gregra: Main/gregra.f
	$(VER) -O Main/gregra.f $(KIK) -o gregra
gr3: Main/gr3.f
	$(VER) -O Main/gr3.f $(KIK) $(REC) -o gr3
mom3: Main/mom3.f
	$(VER) -O Main/mom3.f $(KIK) $(REC) -o mom3
plotcm: Main/plotcm.f
	$(VER) -O Main/plotcm.f $(KIK) $(REC) -o plotcm
graphics3: Main/graphics3.f
	$(VER) -O Main/graphics3.f $(KIK) -o graphics3
inversion3: Main/inversion3.f
	$(VER) -O Main/inversion3.f $(KIK) $(REC) -o inversion3
green3: Main/green3.f
	$(VER) -O Main/green3.f $(KIK) -o green3
graphics2: Main/graphics2.f
	$(VER) -O Main/graphics2.f $(KIK) -o graphics2
inversion2: Main/inversion2.f
	$(VER) -O Main/inversion2.f $(KIK) $(REC) -o inversion2
green: Main/green.f
	$(VER) -O Main/green.f $(KIK) -o green
inversion: Main/inversion.f
	$(VER) -O Main/inversion.f $(KIK) $(REC) -o inversion
graphics: Main/graphics.f
	$(VER) -O Main/graphics.f $(KIK) $(REC) -o graphics
conv.sac.farm: Main/conv.sac.farm.f
	$(VER) -O Main/conv.sac.farm.f $(KIK) -o conv.sac.farm
conv.sac.iris: Main/conv.sac.iris.f
	$(VER) -O Main/conv.sac.iris.f $(KIK) -o conv.sac.iris
