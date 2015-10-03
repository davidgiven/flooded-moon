POVRAY = ../povray/.obj/unix/povray
PLACE = kiess-island
PARAMS = \
	--timeofday 6 \

GCC = g++-4.8

#\
	--altitude 20 \
	--azimuth -20

all: newmoon.png

newmoon.png: Makefile newmoon.ini newmoon.inc newmoon.pov earth.inc \
		/tmp/camera.inc terrainlib/moon.so $(POVRAY)
	$(POVRAY) +Inewmoon +A +D +P +W800 +H600 +SP256 +EP4
	#$(POVRAY) +Inewmoon -A +D +P +W400 +H300
	
/tmp/camera.inc: Makefile terrainmaker/terrainmaker \
		$(wildcard scripts/*) $(wildcard places/*)
	./terrainmaker/terrainmaker scripts/data.tm scripts/makecamera.tm places/$(PLACE).tm $(PARAMS)
	
terrainmaker/terrainmaker: $(wildcard terrainmaker/*.h) $(wildcard terrainmaker/*.cc)
	make -C terrainmaker terrainmaker

terrainlib/moon.so: Makefile \
		terrainlib/moon.cc \
		$(wildcard terrainmaker/*.h)
	$(GCC) \
		-g -O3 -shared -fpic -fPIC -std=c++11 \
		-Iterrainmaker -Iterrainlib/include \
		-o terrainlib/moon.so \
		terrainlib/moon.cc \
		-lnoise \
		-lboost_iostreams

