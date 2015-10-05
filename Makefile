POVRAY = ../povray/.obj/unix/povray
PLACE = nearside-from-space
GCC = g++-4.8
export TERRAINMAKER_OPTIONS = \
	scripts/data.tm \
	places/kiess-island.tm \
	--longitude 83.60 \
	--shmixels 400 \
	--altitude 3 \
	--timeofday 11.3 \
	--azimuth -10 \

#\

all: newmoon.png

/tmp/script.tm: Makefile
	@echo config=scripts/data > $@
	@echo config=places/$(PLACE).tm >> $@
	
newmoon.png: Makefile newmoon.ini newmoon.inc newmoon.pov earth.inc \
		/tmp/camera.inc terrainlib/moon.so $(POVRAY)
	$(POVRAY) +Inewmoon +A +D +P +W800 +H600 +SP256 +EP4
	#$(POVRAY) +Inewmoon -A +D +P +W400 +H300
	
/tmp/camera.inc: Makefile terrainmaker/terrainmaker \
		/tmp/script.tm \
		$(wildcard scripts/*) $(wildcard places/*)
	./terrainmaker/terrainmaker scripts/makecamera.tm
	
terrainmaker/terrainmaker: $(wildcard terrainmaker/*.h) $(wildcard terrainmaker/*.cc)
	make -C terrainmaker terrainmaker

terrainlib/moon.so: \
		terrainlib/moon.cc \
		$(wildcard terrainmaker/*.h) \
		$(wildcard terrainlib/include/*.h)
	$(GCC) \
		-g -O3 -shared -fpic -fPIC -std=c++11 \
		-I$(shell llvm-config-3.3 --includedir) \
		-L$(shell llvm-config-3.3 --libdir) \
		-Iterrainmaker \
		-Iterrainlib/include \
		-I../calculon/include \
		-o terrainlib/moon.so \
		terrainlib/moon.cc \
		-lnoise \
		-lboost_iostreams \
		-lboost_program_options \
		-lLLVM-3.3

