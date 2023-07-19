POVRAY = ../povray/.obj/unix/povray
PLACE = nearside-from-space
GCC = g++
export TERRAINMAKER_OPTIONS = \
	scripts/data.tm \
	places/mountainscape.tm \
	--shmixels 100 \

#\
	--azimuth -20 \
	--altitude 5 \

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
		-Iterrainmaker \
		-Iterrainlib/include \
		-I../calculon/include \
		-o terrainlib/moon.so \
		terrainlib/moon.cc \
		-lnoise -DNOISEINC=\"libnoise/noise.h\" \
		-lboost_iostreams \
		-lboost_program_options \
		-I$(shell llvm-config --includedir) $(shell llvm-config --libs)

